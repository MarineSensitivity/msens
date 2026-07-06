# tests for the H3 hex helpers in R/hex.R
# these require duckdb + the `h3` community extension; skip where it can't load

skip_if_no_h3 <- function() {
  ok <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
    TRUE
  }, error = function(e) FALSE)
  testthat::skip_if_not(ok, "DuckDB h3 extension unavailable")
}

# H3 ids exceed 2^53, so compare via bit64/string, never as double
test_that("hex_id_from_lonlat returns a BIGINT (integer64) matching a known id", {
  skip_if_no_h3()
  id <- hex_id_from_lonlat(-117.20, 32.70, res = 7)
  expect_s3_class(id, "integer64")
  expect_equal(as.character(id), "608718504098529279")   # 0x8729a411cffffff
  expect_equal(hex_id_to_string(id), "8729a411cffffff")
})

test_that("hex_centroids round-trips near the input point", {
  skip_if_no_h3()
  lon <- -117.20; lat <- 32.70
  id  <- hex_id_from_lonlat(lon, lat, res = 7)
  ctr <- hex_centroids(id)
  expect_s3_class(ctr$hex_id, "integer64")
  expect_lt(abs(ctr$lon - lon), 0.05)
  expect_lt(abs(ctr$lat - lat), 0.05)
})

test_that("hex_ocean enumerates a gap-free BIGINT ocean tiling (geometry only)", {
  skip_if_no_h3(); skip_if_not_installed("terra")
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 1)
  names(r) <- "depth_mean"; terra::values(r) <- 1     # all ocean
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  hex_ocean(r, con, res = 7, mask_layer = "depth_mean", out_tbl = "hex")
  d <- DBI::dbGetQuery(con, "SELECT count(*) n, count(DISTINCT hex_id) u,
    round(avg(area_km2),3) a, typeof(hex_id) t FROM hex")
  expect_equal(d$n, d$u)                              # unique hexes
  expect_gt(d$n, terra::ncell(r))                     # more hexes than 400 pixels
  expect_true(d$a > 4 && d$a < 7)                     # res-7 ~5.16 km^2
  expect_equal(d$t, "BIGINT")
  # hex_ocean assigns NO values — only hex_id + area_km2
  expect_setequal(DBI::dbGetQuery(con,
    "SELECT column_name FROM information_schema.columns WHERE table_name='hex'")$column_name,
    c("hex_id", "area_km2"))
})

test_that("hex_interp_idw interpolates source centroids onto hex centroids", {
  skip_if_no_h3(); skip_if_not_installed("terra"); skip_if_not_installed("FNN")
  # field linear in lon => IDW at a hex centroid should ~equal the hex lon
  r <- terra::rast(xmin = -118, xmax = -116, ymin = 32, ymax = 34,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 2)
  names(r) <- c("depth_mean", "field")
  xy <- terra::crds(r, na.rm = FALSE); terra::values(r) <- cbind(1, xy[, 1])
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  hex_ocean(r, con, res = 7, mask_layer = "depth_mean", out_tbl = "hex")
  pix <- as.data.frame(r, xy = TRUE, na.rm = TRUE); names(pix)[1:2] <- c("lon", "lat")
  hex_interp_idw(con, pix, "hex", val_cols = "field", k = 8, power = 2)
  chk <- DBI::dbGetQuery(con, "SELECT h3_cell_to_lng(hex_id) lon, field FROM hex")
  expect_true("field" %in% names(chk))
  expect_gt(cor(chk$field, chk$lon), 0.999)          # recovers the linear field
  expect_lt(mean(abs(chk$field - chk$lon)), 0.02)
})

test_that("hex_add_membership flags in-polygon hexes, buffered for full coverage", {
  skip_if_no_h3(); skip_if_not_installed("terra"); skip_if_not_installed("sf")
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 1)
  names(r) <- "depth_mean"; terra::values(r) <- 1
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  hex_ocean(r, con, res = 7, mask_layer = "depth_mean", out_tbl = "hex")

  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(-118, 32.5), c(-117.5, 32.5), c(-117.5, 33.5), c(-118, 33.5), c(-118, 32.5)))),
    crs = 4326))
  gpkg <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(poly, gpkg, quiet = TRUE)

  # buffered membership should cover >= unbuffered (captures boundary-straddling hexes)
  hex_add_membership(con, "hex", gpkg, "in_buf",   res = 7, buffer = TRUE)
  hex_add_membership(con, "hex", gpkg, "in_nobuf", res = 7, buffer = FALSE)
  n <- DBI::dbGetQuery(con, "SELECT
    count(*) FILTER (WHERE in_buf)   AS nb,
    count(*) FILTER (WHERE in_nobuf) AS nn,
    count(*) AS tot FROM hex")
  expect_gt(n$nb, 0)
  expect_gte(n$nb, n$nn)          # buffering never drops coverage
  expect_lt(n$nb, n$tot)          # west-half polygon: not all hexes
  expect_equal(DBI::dbGetQuery(con, "SELECT typeof(hex_id) t FROM hex LIMIT 1")$t, "BIGINT")
})
