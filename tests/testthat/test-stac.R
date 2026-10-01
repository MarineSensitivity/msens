# Guards stac_build() across the two release schemas. A release's `model` table is keyed by
# `mdl_key` from v8 and by `mdl_seq` before it (v1-v7b, usa05). stac_build() selected `mdl_key`
# unconditionally, so every legacy release died with a binder error AFTER the root and version
# nodes were written -- which is how v7b (v7.1) could not be registered in the catalog.

stac_fixture_con <- function(legacy) {
  con <- DBI::dbConnect(duckdb::duckdb())
  ds  <- data.frame(
    ds_key = c("ms_merge", "am_0.05"), name_short = c("merge", "am"),
    name_display = c("Merged models", "AquaMaps"), description = c("merged", "suitability"),
    response_type = c("suitability", "suitability"), temporal_res = c("static", "static"),
    source_broad = c("MarineSensitivity", "AquaMaps"), citation = NA_character_,
    year_pub = c(2026L, 2019L), sort_order = 1:2,
    date_obs_beg = as.Date(NA), date_obs_end = as.Date(NA),
    date_env_beg = as.Date(NA), date_env_end = as.Date(NA), stringsAsFactors = FALSE)
  if (!legacy) ds$native_format <- c(NA_character_, "raster")
  DBI::dbWriteTable(con, "dataset", ds)
  mdl <- if (legacy)
    data.frame(mdl_seq = c(54241L, 100L), ds_key = c("ms_merge", "am_0.05"), time_period = NA_character_)
  else
    data.frame(mdl_key = c("ms_merge|WORMS:137209", "am|Fis-1"), ds_key = c("ms_merge", "am_0.05"))
  DBI::dbWriteTable(con, "model", mdl)
  con
}
read_item <- function(d, ver, ds) jsonlite::fromJSON(
  file.path(d, ver, ds, sprintf("msens-%s-%s-model_cell.json", ver, ds)), simplifyVector = FALSE)

test_that("a LEGACY (mdl_seq) release builds a whole catalog that names only what it publishes", {
  con <- stac_fixture_con(legacy = TRUE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d   <- tempfile("stac_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ma  <- data.frame(
    mdl_seq = c(54241L, 100L), ds_key = c("ms_merge", "am_0.05"),
    cog_url = c("https://x.org/marine-atlas/cog/usa05/fea392a9c090b6cf.tif",
                "https://x.org/marine-atlas/cog/usa05/8f3828a996ab0ba4.tif"))
  expect_no_error(stac_build("v7b", dir_out = d, con = con, model_asset = ma))

  # PERMANENT REGRESSION: every dataset node is written, not just the root + version
  for (k in c("ms_merge", "am_0.05"))
    expect_true(file.exists(file.path(d, "v7b", k, "collection.json")), info = k)

  it <- read_item(d, "v7b", "ms_merge")
  expect_match(it$assets$data$href, "/v7b/tables/model_asset\\.parquet$")
  expect_equal(it$assets$data$`table:columns`[[1]]$name, "mdl_seq")
  expect_match(it$assets$tables$href, "/v7b/tables/$")
  # nothing a legacy release does not have: no dist_merged Parquet, no SQL tile factory
  js <- paste(readLines(file.path(d, "v7b", "ms_merge", "msens-v7b-ms_merge-model_cell.json")), collapse = "")
  expect_false(grepl("dist_merged", js, fixed = TRUE))
  expect_false(grepl("sql_template", js, fixed = TRUE))
  expect_false(grepl("mdl_key", js, fixed = TRUE))

  # the example links render THIS dataset's own COG through stock titiler /cog
  rels <- vapply(it$links, `[[`, "", "rel")
  xyz  <- it$links[[which(rels == "xyz")]]$href
  expect_match(xyz, "/cog/tiles/WebMercatorQuad/\\{z\\}/\\{x\\}/\\{y\\}\\.png\\?url=", perl = TRUE)
  expect_match(xyz, "fea392a9c090b6cf", fixed = TRUE)
  expect_match(it$links[[which(rels == "tilejson")]]$href, "/cog/WebMercatorQuad/tilejson\\.json\\?url=")
})

test_that("a legacy release without model_asset still builds, without inventing tile links", {
  con <- stac_fixture_con(legacy = TRUE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d   <- tempfile("stac_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(stac_build("v7b", dir_out = d, con = con))
  rels <- vapply(read_item(d, "v7b", "am_0.05")$links, `[[`, "", "rel")
  expect_false(any(rels %in% c("xyz", "tilejson")))
  expect_true("self" %in% rels)
})

test_that("a v8+ (mdl_key) release is unchanged: dist_merged Parquet + the SQL surface", {
  con <- stac_fixture_con(legacy = FALSE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d   <- tempfile("stac_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(stac_build("v9", dir_out = d, con = con))
  it <- read_item(d, "v9", "ms_merge")
  expect_match(it$assets$data$href, "/v9/dist_merged/$")
  expect_match(it$assets$data$alternate$duckdb_sql$`sdm:sql_template`, "mdl_key = '\\{mdl_key\\}'", perl = TRUE)
  expect_true("cog_native" %in% names(read_item(d, "v9", "am_0.05")$assets))
})

test_that("stac_catalog_register adds a version once, in release order, keeping the others", {
  f <- tempfile(fileext = ".json"); on.exit(unlink(f))
  for (v in c("v7", "v8", "v9")) stac_catalog_register(f, v)
  kids <- stac_catalog_register(f, "v7b")
  # a patch release dated AFTER v9 still lands beside the release it patches
  expect_equal(kids, paste0("./", c("v7", "v7b", "v8", "v9"), "/collection.json"))
  expect_equal(stac_catalog_register(f, "v7b"), kids)          # idempotent
  expect_equal(stac_catalog_register(f, "v10"), c(kids, "./v10/collection.json"))  # 10 after 9
  cat0 <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  rels <- vapply(cat0$links, `[[`, "", "rel")
  expect_equal(sum(rels == "root"), 1L); expect_equal(sum(rels == "self"), 1L)
  expect_error(stac_catalog_register(f, "../etc"))
})
