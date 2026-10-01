# native_asset_backfill(): a v8-shaped native_asset for a model_asset-only release ----

ref_row <- function(key, type = "cog", rep = "native", url = paste0("https://x.invalid/", key),
                    layer = NA_character_, bbox = c(-180, 180, -10, 10)) data.frame(
  ms_merge_key = "ms_merge|WORMS:1", mdl_key = key, ds_key = sub("\\|.*$", "", key),
  asset_type = type, representation = rep, asset_url = url, rescale_min = 1L, rescale_max = 100L,
  colormap = "spectral_r", xmin = bbox[1], xmax = bbox[2], ymin = bbox[3], ymax = bbox[4],
  source_layer = layer, stringsAsFactors = FALSE)

fx_ma <- data.frame(mdl_seq = c(10L, 20L, 30L, 40L, 50L),
  ds_key = c("am", "bl", "rng_iucn", "ms_merge", "ch_nmfs"),
  cog_url = paste0("https://x.invalid/cog/", 1:5, ".tif"), stringsAsFactors = FALSE)
fx_xw <- data.frame(mdl_seq = c(10L, 20L, 30L, 40L, 50L),
  mdl_key = c("am|Fis-1", "bl|bl:777", "rng_iucn|rng_iucn:Zus aus", "ms_merge|WORMS:1",
              "ch_nmfs|ch_nmfs:Acropora palmata"), stringsAsFactors = FALSE)
fx_ref <- rbind(
  ref_row("am|Fis-1"),
  ref_row("bl|777", "pmtiles", layer = "bl", bbox = c(1, 2, 3, 4)),
  ref_row("ch_nmfs|Acropora_palmata", "pmtiles", layer = "ch_nmfs"),
  ref_row("am|Fis-1", "cog", "model"),                     # a v8 model row: never copied
  ref_row("ms_merge|WORMS:1", "cog", "model"))

H16 <- "0123456789abcdef"; H32 <- paste0(H16, "fedcba9876543210")
fx_store <- data.frame(
  asset_url = c("https://x.invalid/am|Fis-1", "https://x.invalid/bl|777",
                "https://x.invalid/ch_nmfs|Acropora_palmata"),
  hash = c(H32, H16, H32), stringsAsFactors = FALSE)
B <- "https://s3.example/marine-atlas"; P <- "https://files.example/pmtiles"

test_that("native_key / native_url lay out the unversioned store, COGs on S3 and PMTiles on the file host", {
  expect_equal(native_key("am", H32), paste0("native/am/", H32, ".tif"))
  expect_equal(native_key("bl", H16, "pmtiles"), paste0("native/bl/", H16, ".pmtiles"))
  expect_equal(native_url("am", H32, base = B), paste0(B, "/native/am/", H32, ".tif"))
  expect_equal(native_url("bl", H16, "pmtiles", pmtiles_base = P), paste0(P, "/native/bl/", H16, ".pmtiles"))
  expect_false(grepl("v[0-9]", native_url("bl", H16, "pmtiles", pmtiles_base = P)))   # no version in the path
  expect_error(native_key("am", "nothex"))                                  # a malformed hash never makes a key
  expect_error(native_key("am|x", H16))                                     # nor a ds_key with a separator
})

test_that("native_hash_file is the MD5 of the bytes (== an S3 single-part ETag)", {
  f <- tempfile(); writeBin(charToRaw("hello"), f)
  expect_equal(native_hash_file(f), "5d41402abc4b2a76b9719d911017c592")
})

test_that("native_store_rewrite re-points native rows and refuses an unmapped one", {
  ref <- fx_ref[fx_ref$mdl_key %in% c("am|Fis-1", "bl|777") & fx_ref$representation == "native", ]
  o <- native_store_rewrite(ref, fx_store, base = B, pmtiles_base = P)
  expect_equal(o$asset_url, c(paste0(B, "/native/am/", H32, ".tif"), paste0(P, "/native/bl/", H16, ".pmtiles")))
  expect_error(native_store_rewrite(ref, fx_store[1, ]), "no store mapping")
  m <- rbind(fx_store, fx_store[1, ])
  expect_error(native_store_rewrite(ref, m))                                # duplicate mapping
})

test_that("native_key_v8 spells v1-v7 keys the way v8 does", {
  expect_equal(native_key_v8("bl|bl:22698216"), "bl|22698216")
  expect_equal(native_key_v8("ch_nmfs|ch_nmfs:Acropora globiceps"), "ch_nmfs|Acropora_globiceps")
  expect_equal(native_key_v8("am|Fis-29291"), "am|Fis-29291")            # already identical
  expect_equal(native_key_v8("ms_merge|WORMS:137209"), "ms_merge|WORMS:137209")
  expect_equal(native_key_v8("rng_turtle_swot_dps|rng_turtle_swot_dps:CC"), "rng_turtle_swot_dps|CC")
})

test_that("a matched input gets a model row AND a native row, copied from the reference", {
  o <- native_asset_backfill(fx_ma, fx_xw, fx_ref, fx_store, base = B, pmtiles_base = P)
  expect_named(o, c("ms_merge_key", "mdl_key", "ds_key", "asset_type", "representation",
                    "asset_url", "rescale_min", "rescale_max", "colormap", "xmin", "xmax",
                    "ymin", "ymax", "source_layer"))
  a <- o[o$mdl_key == "20", ]
  expect_setequal(a$representation, c("model", "native"))
  nat <- a[a$representation == "native", ]
  expect_equal(nat$asset_type, "pmtiles"); expect_equal(nat$source_layer, "bl")
  expect_equal(nat$asset_url, paste0(P, "/native/bl/", H16, ".pmtiles"))   # store URL, not the reference's
  expect_false(any(grepl("x.invalid", o$asset_url[o$representation == "native"])))
  expect_equal(c(nat$xmin, nat$xmax, nat$ymin, nat$ymax), c(1, 2, 3, 4))
  mod <- a[a$representation == "model", ]
  expect_equal(mod$asset_url, "https://x.invalid/cog/2.tif")             # v7's OWN gridded COG
  expect_equal(c(mod$rescale_min, mod$rescale_max, mod$colormap), c("1", "100", "spectral_r"))
})

test_that("an unmatched input keeps exactly one model row", {
  o <- native_asset_backfill(fx_ma, fx_xw, fx_ref, fx_store, base = B, pmtiles_base = P)
  expect_equal(o$representation[o$mdl_key == "30"], "model")             # rng_iucn: no original
})

test_that("a merged model gets only a model row, keyed by its own seq (taxon key, as v1-v7)", {
  o <- native_asset_backfill(fx_ma, fx_xw, fx_ref, fx_store, base = B, pmtiles_base = P)
  m <- o[o$mdl_key == "40", ]
  expect_equal(nrow(m), 1L); expect_equal(m$representation, "model")
  expect_equal(m$ms_merge_key, "40"); expect_equal(m$ds_key, "ms_merge")
  expect_true(all(is.na(o$ms_merge_key[o$mdl_key != "40"])))
})

test_that("regression: a single-dataset taxon's own model keeps its taxon key (v1-v7 merged card)", {
  # v1-v7 give a taxon's surface via taxon.mdl_seq == model_asset.mdl_seq, even for an `am` model
  o <- native_asset_backfill(fx_ma, fx_xw, fx_ref, fx_store, taxon_key = c(10, 40))
  expect_equal(o$ms_merge_key[o$mdl_key == "10" & o$representation == "model"], "10")
  expect_true(is.na(o$ms_merge_key[o$mdl_key == "10" & o$representation == "native"]))
  expect_equal(o$ms_merge_key[o$mdl_key == "40"], "40")
  expect_true(all(is.na(o$ms_merge_key[o$mdl_key %in% c("20", "30", "50")])))
})

test_that("a v8 'model' row is never mistaken for an original", {
  o <- native_asset_backfill(fx_ma, fx_xw, fx_ref[fx_ref$representation == "model", ], fx_store)
  expect_false(any(o$representation == "native"))
})

test_that("an original with no store mapping is an error (never falls back to the versioned URL)", {
  expect_error(native_asset_backfill(fx_ma, fx_xw, fx_ref, fx_store[1:2, ]), "no store mapping")
})

test_that("a duplicate (mdl_key, representation) in the reference is an error", {
  expect_error(native_asset_backfill(fx_ma, fx_xw, rbind(fx_ref, ref_row("am|Fis-1")), fx_store),
               "duplicate \\(mdl_key, representation\\)")
})

test_that("a duplicated mdl_seq or crosswalk key is an error", {
  expect_error(native_asset_backfill(rbind(fx_ma, fx_ma[1, ]), fx_xw, fx_ref, fx_store), "duplicated mdl_seq")
  expect_error(native_asset_backfill(fx_ma, rbind(fx_xw, fx_xw[1, ]), fx_ref, fx_store), "twice")
})

test_that("mdl_key spelling matches taxon_model (mdl_seq as a plain string, no '.0')", {
  ma <- fx_ma; ma$mdl_seq <- as.numeric(ma$mdl_seq)                      # v7 stores DOUBLEs
  o <- native_asset_backfill(ma, fx_xw, fx_ref, fx_store)
  expect_true(all(grepl("^[0-9]+$", o$mdl_key)))
})

test_that("regression: a v7-shaped release (model_asset + backfilled native_asset) ships both reps", {
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = tempfile("na_", fileext = ".duckdb")))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "taxon", data.frame(
    taxon_id = as.numeric(137162), taxon_authority = "worms", scientific_name = "Sterna paradisaea",
    common_name = "Arctic Tern", sp_cat = "bird", mdl_seq = 40L, is_ok = TRUE,
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "taxon_model", data.frame(
    taxon_id = as.numeric(137162), ds_key = c("ms_merge", "am_0.05", "bl", "rng_iucn"),
    mdl_seq = c(40L, 10L, 20L, 30L), stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "model_asset", data.frame(
    mdl_key = fx_xw$mdl_key[1:4], mdl_seq = fx_ma$mdl_seq[1:4], ds_key = fx_ma$ds_key[1:4],
    cog_url = fx_ma$cog_url[1:4], grid_id = "usa05", ver = "v7", stringsAsFactors = FALSE))
  before <- app_taxon_shards(con, "v7")
  DBI::dbWriteTable(con, "native_asset", native_asset_backfill(
    fx_ma[1:4, ], fx_xw[1:4, ], fx_ref, fx_store, taxon_key = DBI::dbGetQuery(con, "SELECT mdl_seq FROM taxon")$mdl_seq))
  after <- app_taxon_shards(con, "v7")
  card <- function(sh) unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)[[1]]
  b <- card(before); a <- card(after)
  reps <- function(cd) setNames(lapply(cd$inputs, function(i) vapply(i$assets, `[[`, "", "rep")),
                                vapply(cd$inputs, `[[`, "", "mdl_key"))
  expect_true(all(lengths(reps(b)) == 1L))                               # before: one asset each
  expect_setequal(reps(a)[["10"]], c("model", "native"))                 # am
  expect_setequal(reps(a)[["20"]], c("model", "native"))                 # bl
  expect_equal(reps(a)[["30"]], "model")                                 # rng_iucn: unmatched
  expect_equal(a$merged$url, b$merged$url)                               # merged surface unchanged
  # only `assets` differs: everything else in the card is identical
  strip <- function(cd) { cd$inputs <- lapply(cd$inputs, function(i) { i$assets <- NULL; i }); cd }
  expect_identical(strip(a), strip(b))
})

test_that("native_vintage_check flags a source whose vintage differs, not one that matches", {
  a <- data.frame(ds_key = c("am_0.05", "bl", "rng_iucn", "zz"), year_pub = c(2023L, 2024L, 2025L, 1L),
                  date_created = c("2025-05-27", NA, "2026-02-10", NA), stringsAsFactors = FALSE)
  b <- data.frame(ds_key = c("am", "bl", "rng_iucn"), year_pub = c(2023L, 2025L, 2025L),
                  date_created = c("2025-05-27", NA, "2026-02-10"), stringsAsFactors = FALSE)
  o <- native_vintage_check(a, b)
  expect_equal(o$ds_key, c("am", "bl", "rng_iucn", "zz"))
  expect_equal(o$same, c(TRUE, FALSE, TRUE, NA))                 # NA = no such dataset in the reference
  expect_equal(o$differs[2], "year_pub")
})

test_that("regression: a taxon whose own surface is an input model keeps `merged` after the backfill", {
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = tempfile("na2_", fileext = ".duckdb")))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "taxon", data.frame(
    taxon_id = as.numeric(1), taxon_authority = "worms", scientific_name = "Zus aus",
    common_name = "Z", sp_cat = "fish", mdl_seq = 10L, is_ok = TRUE, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "taxon_model", data.frame(
    taxon_id = as.numeric(1), ds_key = c("am_0.05", "bl"), mdl_seq = c(10L, 20L),
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "model_asset", data.frame(
    mdl_key = fx_xw$mdl_key[1:2], mdl_seq = fx_ma$mdl_seq[1:2], ds_key = fx_ma$ds_key[1:2],
    cog_url = fx_ma$cog_url[1:2], grid_id = "usa05", ver = "v7", stringsAsFactors = FALSE))
  card <- function(sh) unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)[[1]]
  before <- card(app_taxon_shards(con, "v7"))$merged$url
  expect_equal(before, "https://x.invalid/cog/1.tif")
  DBI::dbWriteTable(con, "native_asset", native_asset_backfill(
    fx_ma[1:2, ], fx_xw[1:2, ], fx_ref, fx_store, taxon_key = 10L))
  expect_equal(card(app_taxon_shards(con, "v7"))$merged$url, before)
})

test_that("native_store_index lists only {ds_key}/{hash}.ext objects, ignoring the release-scoped layout", {
  fake <- tempfile(); H <- "0123456789abcdef"
  writeLines(c("#!/bin/sh", "cat <<EOF",
    sprintf("2026-09-30 10:00:00  1 marine-atlas/native/am/%s.tif", H),
    sprintf("2026-09-30 10:00:00  1 marine-atlas/native/bl/%s.pmtiles", "fedcba9876543210"),
    "2026-09-30 10:00:00  1 marine-atlas/native/am_native/am_Fis-1.tif",
    "2026-09-30 10:00:00  1 marine-atlas/native/pmtiles/bl/22694870.pmtiles", "EOF"), fake)
  Sys.chmod(fake, "0755")
  expect_setequal(native_store_index(aws = fake), c(H, "fedcba9876543210"))
  empty <- tempfile(); writeLines(c("#!/bin/sh", "exit 1"), empty); Sys.chmod(empty, "0755")
  expect_equal(native_store_index(aws = empty), character())                # empty store is not an error
})
