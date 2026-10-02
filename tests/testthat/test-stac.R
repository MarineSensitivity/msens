# Guards stac_build() across the two release schemas. A release's `model` table is keyed by
# `mdl_key` from v8 and by `mdl_seq` before it (v1-v7b, usa05). stac_build() selected `mdl_key`
# unconditionally, so every legacy release died with a binder error AFTER the root and version
# nodes were written -- which is how v7b (v7.1) could not be registered in the catalog.

stac_fixture_con <- function(legacy, native_asset = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb())
  ds  <- data.frame(
    ds_key = c("ms_merge", "am_0.05"), name_short = c("merge", "am"),
    name_display = c("Merged models", "AquaMaps"), description = c("merged", "suitability"),
    response_type = c("suitability", "suitability"), temporal_res = c("static", "static"),
    source_broad = c("MarineSensitivity", "AquaMaps"), citation = NA_character_,
    year_pub = c(2026L, 2019L), sort_order = 1:2,
    date_obs_beg = as.Date(NA), date_obs_end = as.Date(NA),
    date_env_beg = as.Date(NA), date_env_end = as.Date(NA), stringsAsFactors = FALSE)
  if (!legacy) {
    ds$native_format <- c(NA_character_, "raster")
    # a vector dataset beside the raster one
    ds <- rbind(ds, transform(ds[2, ], ds_key = "rng_iucn", name_short = "iucn",
                              name_display = "IUCN ranges", sort_order = 3L, native_format = "vector"))
  }
  DBI::dbWriteTable(con, "dataset", ds)
  mdl <- if (legacy)
    data.frame(mdl_seq = c(54241L, 100L), ds_key = c("ms_merge", "am_0.05"), time_period = NA_character_)
  else
    data.frame(mdl_key = c("ms_merge|WORMS:137209", "am|Fis-1", "rng_iucn|1"),
               ds_key  = c("ms_merge", "am_0.05", "rng_iucn"))
  DBI::dbWriteTable(con, "model", mdl)
  # the release's pointer table, when it publishes one (v7, v7b, v8, v9; not v1-v6)
  if (native_asset) DBI::dbWriteTable(con, "native_asset", data.frame(
    mdl_key = "am|Fis-1", ds_key = "am_0.05", asset_type = "cog", representation = "native",
    asset_url = "https://x.org/marine-atlas/native/am_0.05/3004e503c8824a67.tif"))
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
})

# ---- per-model files: one pointer table, never a directory ------------------------------------

stac_asset_hrefs <- function(d, ver) {
  f <- list.files(file.path(d, ver), "-model_cell\\.json$", recursive = TRUE, full.names = TRUE)
  unlist(lapply(f, function(x) vapply(
    jsonlite::fromJSON(x, simplifyVector = FALSE)$assets, `[[`, "", "href")))
}
old_assets <- c("native_asset", "cog_native", "cog_model", "pmtiles_native")

test_that("a raster and a vector dataset Item each carry ONE native_asset: the release's pointer table", {
  con <- stac_fixture_con(legacy = FALSE, native_asset = TRUE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d   <- tempfile("stac_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(stac_build("v9", dir_out = d, con = con))
  for (k in c("am_0.05", "rng_iucn")) {            # raster, vector
    a  <- read_item(d, "v9", k)$assets
    na <- a[["native_asset"]]
    expect_equal(sum(names(a) == "native_asset"), 1L, info = k)
    expect_equal(na$href, "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v9/tables/native_asset.parquet", info = k)
    expect_equal(na$type, "application/vnd.apache.parquet", info = k)
    expect_equal(unlist(na$roles), c("metadata", "index"), info = k)
    expect_match(na$title, k, fixed = TRUE)         # tells the reader which ds_key to filter on
    expect_true(all(c("mdl_key", "ds_key", "representation", "asset_type", "asset_url") %in%
                    vapply(na$`table:columns`, `[[`, "", "name")), info = k)
    # the old per-directory assets are gone
    expect_false(any(c("cog_native", "cog_model", "pmtiles_native") %in% names(a)), info = k)
  }
  # the merged dataset has no native format: nothing to point at
  expect_false("native_asset" %in% names(read_item(d, "v9", "ms_merge")$assets))
  # the data asset is untouched
  expect_match(read_item(d, "v9", "am_0.05")$assets$data$href, "/v9/dist_merged/$")
})

test_that("a release WITHOUT a native_asset table gets no native asset at all, not a dead link", {
  con <- stac_fixture_con(legacy = FALSE, native_asset = FALSE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d   <- tempfile("stac_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(stac_build("v9", dir_out = d, con = con))
  for (k in c("ms_merge", "am_0.05", "rng_iucn"))
    expect_false(any(old_assets %in% names(read_item(d, "v9", k)$assets)), info = k)
  # an explicit has_native_asset overrides the table check, both ways
  d2 <- tempfile("stac_"); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  stac_build("v9", dir_out = d2, con = con, has_native_asset = TRUE)
  expect_true("native_asset" %in% names(read_item(d2, "v9", "am_0.05")$assets))
  con2 <- stac_fixture_con(legacy = FALSE, native_asset = TRUE); on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  d3 <- tempfile("stac_"); on.exit(unlink(d3, recursive = TRUE), add = TRUE)
  stac_build("v9", dir_out = d3, con = con2, has_native_asset = FALSE)
  expect_false("native_asset" %in% names(read_item(d3, "v9", "am_0.05")$assets))
})

test_that("no asset points at a file-host pmtiles directory (REGRESSION: the dead pmtiles/ + native/ directory hrefs)", {
  for (na in c(TRUE, FALSE)) {
    con <- stac_fixture_con(legacy = FALSE, native_asset = na)
    d   <- tempfile("stac_")
    stac_build("v9", dir_out = d, con = con)
    h <- stac_asset_hrefs(d, "v9")
    expect_false(any(grepl("/pmtiles/", h, fixed = TRUE)), info = as.character(na))
    expect_false(any(grepl("/native/", h, fixed = TRUE)), info = as.character(na))
    expect_false(any(grepl("^https://file\\.marinesensitivity\\.org/pmtiles", h)), info = as.character(na))
    # the only directory href left is the data asset's dist_merged/ Parquet partition root
    expect_setequal(unique(h[grepl("/$", h)]), c(
      "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v9/dist_merged/"))
    DBI::dbDisconnect(con, shutdown = TRUE); unlink(d, recursive = TRUE)
  }
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

# ---- root catalog alias for another origin ------------------------------------

alias_src <- function(kids = c("v7", "v7b", "v9"), rel_prefix = "./", extra = NULL) list(
  type = "Catalog", stac_version = "1.0.0", id = "marinesensitivity",
  title = "MarineSensitivity STAC Catalog", description = "d",
  links = c(list(list(rel = "root", href = "./catalog.json"),
                 list(rel = "self", href = "https://file.marinesensitivity.org/stac/catalog.json")),
            lapply(kids, function(v) list(rel = "child", href = paste0(rel_prefix, v, "/collection.json"))),
            extra))
hrefs <- function(a) vapply(a$links, `[[`, "", "href")
relsv <- function(a) vapply(a$links, `[[`, "", "rel")

test_that("stac_catalog_alias makes every child absolute, in a fixed link order", {
  a <- stac_catalog_alias(alias_src())
  expect_equal(relsv(a), c("root", "self", "canonical", "service-desc", "child", "child", "child"))
  expect_equal(hrefs(a), c(
    "https://marinesensitivity.org/stac/catalog.json",
    "https://marinesensitivity.org/stac/catalog.json",
    "https://file.marinesensitivity.org/stac/catalog.json",
    "https://stac-api.marinesensitivity.org/",
    "https://file.marinesensitivity.org/stac/v7/collection.json",
    "https://file.marinesensitivity.org/stac/v7b/collection.json",
    "https://file.marinesensitivity.org/stac/v9/collection.json"))
  expect_false(any(grepl("^[.]", hrefs(a))))
  expect_equal(a$links[[1]]$type, "application/json")
  expect_equal(a$links[[4]]$title, "searchable STAC API (one Item per model)")
  expect_equal(unname(hrefs(a)[relsv(a) %in% c("root", "self")]), rep("https://marinesensitivity.org/stac/catalog.json", 2))
  # the source's own self URL survives only as `canonical`
  src_self <- "https://file.marinesensitivity.org/stac/catalog.json"
  expect_equal(relsv(a)[hrefs(a) == src_self], "canonical")
  # same top-level fields in the same order
  expect_equal(names(a), names(alias_src()))
  expect_equal(a[names(a) != "links"], alias_src()[names(alias_src()) != "links"])
})

test_that("stac_catalog_alias: absolute child hrefs are untouched, bare relative ones resolve like ./", {
  ex <- list(list(rel = "child", href = "https://elsewhere.org/stac/x/collection.json", type = "application/json"),
             list(rel = "describedby", href = "docs/readme.html", type = "text/html"))
  a <- stac_catalog_alias(alias_src(extra = ex))
  expect_equal(hrefs(a)[8], "https://elsewhere.org/stac/x/collection.json")
  expect_equal(a$links[[9]]$href, "https://file.marinesensitivity.org/stac/docs/readme.html")
  expect_equal(a$links[[9]]$type, "text/html")                      # an existing type is kept
  expect_equal(hrefs(stac_catalog_alias(alias_src(rel_prefix = ""))),
               hrefs(stac_catalog_alias(alias_src())))
  # custom hosts, trailing slashes tolerated
  b <- stac_catalog_alias(alias_src(), self_url = "https://x.test/c.json", src_base = "http://h/stac/", api_url = "http://api/")
  expect_equal(hrefs(b)[c(1, 3, 4, 5)], c("https://x.test/c.json", "http://h/stac/catalog.json", "http://api/", "http://h/stac/v7/collection.json"))
})

test_that("stac_catalog_alias refuses a non-Catalog and a catalog without children", {
  expect_error(stac_catalog_alias(alias_src(kids = character())), "child")
  expect_error(stac_catalog_alias(list(type = "Collection", links = alias_src()$links)), "Catalog")
  expect_error(stac_catalog_alias(list(type = "Catalog")), "links")
})

test_that("register then alias gives children in release order v7 v7b v8 v9 (and reads a path)", {
  f <- tempfile(fileext = ".json"); on.exit(unlink(f))
  for (v in c("v9", "v7", "v8", "v7b")) stac_catalog_register(f, v)
  a <- stac_catalog_alias(f)
  expect_equal(hrefs(a)[relsv(a) == "child"],
               paste0("https://file.marinesensitivity.org/stac/", c("v7", "v7b", "v8", "v9"), "/collection.json"))
})

test_that("alias of the deployed root matches the alias published today", {
  src <- alias_src(kids = c("v7", "v7b", "v8", "v9"))
  expect_equal(src$id, "marinesensitivity"); expect_equal(src$title, "MarineSensitivity STAC Catalog")
  expect_equal(hrefs(stac_catalog_alias(src)), c(
    "https://marinesensitivity.org/stac/catalog.json",
    "https://marinesensitivity.org/stac/catalog.json",
    "https://file.marinesensitivity.org/stac/catalog.json",
    "https://stac-api.marinesensitivity.org/",
    paste0("https://file.marinesensitivity.org/stac/", c("v7", "v7b", "v8", "v9"), "/collection.json")))
})

test_that("stac_catalog_alias_write writes the same pretty JSON as the package's node writer", {
  a <- stac_catalog_alias(alias_src())
  f <- tempfile(fileext = ".json"); g <- tempfile(fileext = ".json"); on.exit(unlink(c(f, g)))
  expect_equal(stac_catalog_alias_write(a, f), f)
  msens:::.stac_write(a, g)
  expect_identical(readLines(f), readLines(g))
  expect_equal(hrefs(jsonlite::fromJSON(f, simplifyVector = FALSE)), hrefs(a))
  expect_error(stac_catalog_alias_write(list(type = "Item"), f))
})
