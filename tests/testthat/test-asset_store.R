# asset_store.R: one content-addressed store for every distribution file of every release ----

sq <- function(x0, y0, s = 1) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + s, y0), c(x0 + s, y0 + s),
                                                       c(x0, y0 + s), c(x0, y0))))
vec <- function(geoms, key = "bl|1", crs = 4326, ...) sf::st_sf(
  mdl_key = key, ..., geometry = sf::st_sfc(geoms, crs = crs))

test_that("asset_enc names one tag per file family and refuses an unknown one", {
  expect_equal(asset_enc("native_pmtiles"), "mvt-z0-10-simp10")
  expect_equal(asset_enc("cog_model"), "int1u-nd0-ovr")   # the container; the quantisation is in the hashed pixels (pixel_hashes)
  expect_false(asset_enc("cog_model") == asset_enc("cog_model_usa05"))        # v1-v7 (no overviews) is a different object
  expect_true(all(c("cog_model", "native_am", "native_ax", "native_pmtiles") %in% names(asset_enc())))
  expect_error(asset_enc("jpeg"), "unknown asset family")
})

test_that("content_hashes_df equals content_hashes on the same rows, and ignores row order", {
  d <- data.frame(mdl_key = rep(c("a", "b"), each = 3), cell_id = c(1:3, 4:6), val = c(10, 20, 30, 1, 2, 3))
  h <- content_hashes_df(d)
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", d)
  ref <- content_hashes(con, "t", "mdl_key"); ref <- ref[order(ref$mdl_key), ]; rownames(ref) <- NULL
  expect_equal(h, ref)
  expect_equal(content_hashes_df(d[sample(nrow(d)), ]), h)                   # order-blind
  # R integers vs doubles must not change the hash: the payload is canonical (INTEGER, DOUBLE)
  dd <- d; dd$cell_id <- as.double(dd$cell_id)
  expect_equal(content_hashes_df(dd), h)
  dd <- d; dd$val <- as.integer(dd$val)
  expect_equal(content_hashes_df(dd), h)
  d2 <- d; d2$val[1] <- 11
  h2 <- content_hashes_df(d2)
  expect_false(h2$content_hash[h2$mdl_key == "a"] == h$content_hash[h$mdl_key == "a"])
  expect_equal(h2$content_hash[h2$mdl_key == "b"], h$content_hash[h$mdl_key == "b"])   # only the changed model moves
  expect_match(h$content_hash, "^[0-9a-f]{16}$")
})

test_that("native_raster_rows returns the (cell_id, val) a COG was painted from - cropped window included", {
  g <- list(nc = 20L, nr = 10L, xmin = 0, ymax = 10, resx = 1, resy = 1, crs = "EPSG:4326")
  d <- data.frame(cell_id = c(23, 24, 45, 46, 120), val = c(5, 6, 7, 8, 9))     # rows 2-6, cols 3-6 of a 20x10 grid
  f <- tempfile(fileext = ".tif"); publish_cog(d$cell_id, d$val, f, g)
  r <- native_raster_rows(terra::rast(f), g)
  expect_equal(r[order(r$cell_id), ], d, ignore_attr = TRUE)
  # a hash from decoded pixels equals the hash of the rows it was painted from
  src <- cbind(mdl_key = "m", d)
  expect_equal(content_hashes_df(cbind(mdl_key = "m", r))$content_hash, content_hashes_df(src)$content_hash)
  # the wrong grid is an error, never a silent mis-index
  expect_error(native_raster_rows(terra::rast(f), modifyList(g, list(nc = 4L, nr = 4L))), "outside the grid")
})

test_that("native_vector_hash is blind to feature order and extra columns, and sees geometry and tile attributes", {
  x  <- vec(list(sq(0, 0), sq(5, 5)))
  h  <- native_vector_hash(x)
  expect_match(h, "^[0-9a-f]{16}$")
  expect_equal(native_vector_hash(x[2:1, ]), h)                              # feature order
  x$area <- c(1, 2); x$junk <- "z"
  expect_equal(native_vector_hash(x), h)                                     # source columns that never reach the tile
  expect_false(native_vector_hash(vec(list(sq(0, 0), sq(5, 5.001)))) == h)   # a moved vertex
  expect_false(native_vector_hash(vec(list(sq(0, 0), sq(5, 5)), key = "bl|2")) == h)   # mdl_key reaches the tile
  expect_false(native_vector_hash(vec(list(sq(0, 0)))) == h)                 # a dropped feature
  expect_equal(native_vector_hash(vec(list(sq(0, 0), sq(5, 5), sf::st_polygon()))), h)   # empties are dropped (as publish_pmtiles does)
})

test_that("native_vector_hash normalises CRS and Z the way publish_pmtiles does", {
  x4326 <- vec(list(sq(0, 0), sq(5, 5)))
  xna   <- x4326; sf::st_crs(xna) <- NA
  expect_equal(native_vector_hash(xna), native_vector_hash(x4326))           # NA crs is assumed 4326
  x3857 <- sf::st_transform(x4326, 3857)
  expect_equal(native_vector_hash(x3857), native_vector_hash(sf::st_transform(x3857, 4326)))
  z <- vec(list(sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), 7))), sq(5, 5)))
  expect_equal(native_vector_hash(z), native_vector_hash(x4326))             # Z dropped
})

test_that("native_vector_hashes hashes each model separately", {
  x <- rbind(vec(list(sq(0, 0)), "bl|1"), vec(list(sq(5, 5)), "bl|2"), vec(list(sq(9, 9)), "bl|2"))
  h <- native_vector_hashes(x)
  expect_equal(h$mdl_key, c("bl|1", "bl|2")); expect_equal(h$n_features, c(1L, 2L))
  expect_equal(h$content_hash[2], native_vector_hash(x[x$mdl_key == "bl|2", ]))
  expect_false(anyDuplicated(h$content_hash) > 0)
})

test_that("asset_key lays out cog/{grid} and native/{ds}; asset_key_from_url finds the store key or NA", {
  H <- "0123456789abcdef"; B <- "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas"
  expect_equal(asset_key("cog", "global05", H), paste0("cog/global05/", H, ".tif"))
  expect_equal(asset_key("native", "bl", H, "pmtiles"), paste0("native/bl/", H, ".pmtiles"))
  expect_error(asset_key("cog", "global05", "xyz")); expect_error(asset_key("cog", "a/b", H))
  expect_equal(asset_key_from_url(paste0(B, "/cog/usa05/", H, ".tif")), paste0("cog/usa05/", H, ".tif"))
  expect_equal(asset_key_from_url(paste0(B, "/native/bl/", H, ".pmtiles")), paste0("native/bl/", H, ".pmtiles"))
  expect_equal(asset_key_from_url(paste0("https://file.marinesensitivity.org/pmtiles/native/bl/", H, ".pmtiles?v=1")),
               paste0("native/bl/", H, ".pmtiles"))                          # the legacy file-host mirror of a store object
  # versioned paths are NOT store objects: that is how a release still holding them is found
  expect_true(is.na(asset_key_from_url(paste0(B, "/v8/native/am/am_Fis-1.tif"))))
  expect_true(is.na(asset_key_from_url("https://file.marinesensitivity.org/pmtiles/v8/bl/22694799.pmtiles?v=1")))
  expect_true(is.na(asset_key_from_url(NA_character_)))
})

cat_fx <- function() {
  enc <- asset_enc("cog_model"); ch <- c("1111111111111111", "2222222222222222")
  h   <- content_hash_encoded(ch, enc)
  hp  <- content_hash_encoded("3333333333333333", asset_enc("native_pmtiles"))
  data.frame(store = c("cog", "cog", "native"),
    key = c(asset_key("cog", "global05", h[1]), asset_key("cog", "global05", h[2]), asset_key("native", "bl", hp, "pmtiles")),
    content_hash = c(ch, "3333333333333333"), enc = c(enc, enc, asset_enc("native_pmtiles")),
    asset_type = c("cog", "cog", "pmtiles"), grid_id = c("global05", "global05", NA), ds_key = c(NA, NA, "bl"),
    bytes = c(10, 20, 30), md5 = c("a", "b", "c"), created = as.Date("2026-10-01"), first_ver = c("v8", "v8", "v9"),
    stringsAsFactors = FALSE)
}

test_that("asset_catalog_check accepts a sound catalog and rejects each way it can lie", {
  expect_silent(asset_catalog_check(cat_fx()))
  x <- cat_fx(); expect_error(asset_catalog_check(x[, -2]), "lacks column")
  x <- cat_fx(); x$key[2] <- x$key[1]; expect_error(asset_catalog_check(x), "duplicated key")
  x <- cat_fx(); x$key[1] <- "v8/native/am/x.tif"; expect_error(asset_catalog_check(x), "is not")
  x <- cat_fx(); x$store[1] <- "native"; expect_error(asset_catalog_check(x), "`store` disagrees")
  x <- cat_fx(); x$grid_id[1] <- "usa05"; expect_error(asset_catalog_check(x), "disagrees with its key")
  x <- cat_fx(); x$asset_type[3] <- "cog"; expect_error(asset_catalog_check(x), "extension")
  # the key must BE the encoded content hash: a mislabelled enc (or content) cannot slip in
  x <- cat_fx(); x$enc[1] <- "flt4s-nd9999-noovr"; expect_error(asset_catalog_check(x), "not content_hash_encoded")
  x <- cat_fx(); x$content_hash[2] <- "9999999999999999"; expect_error(asset_catalog_check(x), "not content_hash_encoded")
})

test_that("asset_catalog_add is idempotent, appends only new keys, and refuses a key with new content", {
  cat0 <- cat_fx()[1:2, ]
  a <- asset_catalog_add(cat0, cat_fx())
  expect_equal(nrow(a), 3L); expect_equal(attr(a, "added"), cat_fx()$key[3])
  expect_equal(nrow(asset_catalog_add(a, cat_fx())), 3L)                     # same objects again: nothing to add
  expect_length(attr(asset_catalog_add(a, cat_fx()), "added"), 0)
  bad <- cat_fx(); bad$content_hash[1] <- "4444444444444444"                 # same key, other content
  expect_error(asset_catalog_add(cat0, bad), "not content_hash_encoded|different content")
  bad <- cat_fx(); bad$enc[1] <- cat0$enc[1]; bad$content_hash[1] <- cat0$content_hash[2]
  bad$key[1] <- cat0$key[1]
  expect_error(asset_catalog_add(cat0, bad))
})

test_that("asset_catalog_write/read round-trips the validated catalog (a local base stands in for the bucket)", {
  d <- tempfile(); dir.create(d); f <- file.path(d, "assets.parquet")
  asset_catalog_write(cat_fx(), f)
  x <- asset_catalog_read(d)
  expect_equal(x$key, cat_fx()$key); expect_equal(x$content_hash, cat_fx()$content_hash)
  bad <- cat_fx(); bad$key[2] <- bad$key[1]
  expect_error(asset_catalog_write(bad, tempfile(fileext = ".parquet")), "duplicated key")   # an invalid catalog is never written
})

test_that("store_unreferenced finds catalog rows no release points at, across every release", {
  B <- "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas"; x <- cat_fx()
  v8 <- data.frame(asset_url = paste0(B, "/", x$key[1]))
  v9 <- data.frame(asset_url = c(paste0(B, "/", x$key[1]), paste0("https://file.marinesensitivity.org/pmtiles/", x$key[3], "?v=9")))
  u <- store_unreferenced(x, list(v8, v9))
  expect_equal(u$key, x$key[2])                                              # the only unreferenced object
  expect_equal(nrow(store_unreferenced(x, list(v8, v9, paste0(B, "/", x$key[2])))), 0L)   # a character vector of URLs works too
  # a release still holding a VERSIONED pointer does not reference the store object it duplicates
  v7 <- data.frame(asset_url = paste0(B, "/v8/native/am/am_Fis-1.tif"))
  expect_equal(nrow(store_unreferenced(x, list(v7))), 3L)
})

test_that("asset_key_collisions: one key, one content (the migration invariant)", {
  m <- data.frame(key = c("k1", "k1", "k2"), content_hash = c("a", "a", "b"), enc = "e")
  expect_equal(nrow(asset_key_collisions(m)), 0L)
  m$content_hash[2] <- "z"
  expect_equal(unique(asset_key_collisions(m)$key), "k1")
  m2 <- data.frame(key = c("k1", "k1"), content_hash = "a", enc = c("e1", "e2"))   # same payload, other encoding
  expect_equal(nrow(asset_key_collisions(m2)), 2L)
})

test_that("pixel_quant_sql reproduces what each writer stores: terra truncates, ax/suit-merged round half-to-even", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  v <- c(0.4, 1, 1.99, 2.5, 3.5, 12.34, 254.9, 300, 100)
  q <- function(mode) DBI::dbGetQuery(con, sprintf("SELECT %s AS q FROM (SELECT unnest(%s) AS val) t",
    pixel_quant_sql("val", mode), paste0("[", paste(v, collapse = ","), "]::DOUBLE[]")))$q
  expect_equal(q("trunc"),       c(0, 1, 1, 2, 3, 12, 254, 255, 100))
  expect_equal(q("round_trunc"), c(0, 1, 2, 2, 4, 12, 255, 255, 100))   # 2.5 -> 2 and 3.5 -> 4: half to even, like R
  expect_equal(q("none"), v)
})

test_that("pixel_hashes == the hash of the pixels a painted COG decodes to (the key is the file's own content)", {
  g <- list(nc = 20L, nr = 10L, xmin = 0, ymax = 10, resx = 1, resy = 1, crs = "EPSG:4326")
  rows <- data.frame(mdl_key = "m", cell_id = c(23, 24, 45, 46, 120, 121), val = c(5.9, 6.2, 7, 0.6, 99.99, 1.01))
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "r", rows)
  f <- tempfile(fileext = ".tif"); publish_cog(rows$cell_id, rows$val, f, g)       # INT1U: terra truncates
  dec <- native_raster_rows(terra::rast(f), g)
  expect_equal(sort(dec$val), c(1, 5, 6, 7, 99))                                      # 0.6 -> 0 = NoData: not a pixel
  want <- content_hashes_df(cbind(mdl_key = "m", dec))$content_hash
  expect_equal(pixel_hashes(con, "r", "mdl_key", "trunc")$content_hash, want)
  # rounding gives different pixels, hence a different key, from the same rows
  expect_false(pixel_hashes(con, "r", "mdl_key", "round_trunc")$content_hash == want)
  # sub-1 pixels never enter the hash; min_pixel = -Inf keeps them
  expect_equal(pixel_hashes(con, "r", "mdl_key", "trunc")$n, 5L)
  expect_equal(pixel_hashes(con, "r", "mdl_key", "trunc", min_pixel = -Inf)$n, 6L)
})

test_that("pixel_hashes dedup = 'max' paints the value the merge consumes where a cell repeats", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  rows <- data.frame(mdl_key = "t", cell_id = c(1, 1, 2, 3, 3), val = c(10, 40, 20, 30.9, 30))   # cells 1 and 3 repeat
  DBI::dbWriteTable(con, "r", rows)
  one <- data.frame(mdl_key = "t", cell_id = 1:3, val = c(40, 20, 30.9))                          # max per cell
  DBI::dbWriteTable(con, "one", one)
  expect_equal(pixel_hashes(con, "r", dedup = "max")$content_hash, pixel_hashes(con, "one")$content_hash)
  expect_equal(pixel_hashes(con, "r", dedup = "max")$n, 3L)
  expect_equal(pixel_hashes(con, "r")$n, 5L)                                      # without it the repeats count twice
  expect_false(pixel_hashes(con, "r")$content_hash == pixel_hashes(con, "one")$content_hash)
})
