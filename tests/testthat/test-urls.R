# the egress rule: bulk bytes come from the object store, the VM serves computed
# responses and small pages. One fixture per class and per classification rule.

cls <- function(u, ...) url_audit(u, ...)$class

test_that("atlas_bases() defaults are byte-identical to the hosts the package used to hardcode", {
  withr::local_options(msens.atlas_bases = NULL)
  expect_identical(atlas_bases(), list(
    store          = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public",
    atlas          = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas",
    file           = "https://file.marinesensitivity.org",
    storage        = "https://storage.marinesensitivity.org",
    titiler        = "https://titiler-v8.marinesensitivity.org",
    titiler_legacy = "https://titiler.marinesensitivity.org/msens",
    stac           = "https://file.marinesensitivity.org/stac",
    stac_api       = "https://stac-api.marinesensitivity.org",
    app            = "https://app.marinesensitivity.org",
    preview        = "https://preview.marinesensitivity.org"))
})

test_that("refactored defaults resolve to the strings they replaced (pure refactor)", {
  withr::local_options(msens.atlas_bases = NULL)
  cfg <- stac_cfg("v9")
  expect_identical(cfg$stac_base,    "https://file.marinesensitivity.org/stac")
  expect_identical(cfg$data_base,    "https://file.marinesensitivity.org/derived")
  expect_identical(cfg$file_base,    "https://file.marinesensitivity.org")
  expect_identical(cfg$titiler_base, "https://titiler.marinesensitivity.org/msens")
  expect_identical(cfg$pg_base,      "https://tile.marinesensitivity.org")
  expect_identical(cfg$atlas_base,   "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas")
  fm <- function(f, arg) formals(f)[[arg]]
  for (f in list(cell_tile_url, cog_tile_url, cog_point_value, cell_stats))
    expect_identical(eval(fm(f, "base")), "https://titiler-v8.marinesensitivity.org")
  expect_identical(eval(fm(build_storage_index, "site_url")), "https://storage.marinesensitivity.org")
  expect_identical(eval(fm(build_storage_index, "obj_url")),
                   "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public")
  # the one tile-URL function with a default base, called without it
  expect_match(cell_tile_url(mdl_key = "ms_merge|WORMS:1"), "^https://titiler-v8[.]marinesensitivity[.]org/")
})

test_that("msens.atlas_bases overrides merge over the defaults; atlas follows store", {
  withr::local_options(msens.atlas_bases = list(store = "http://localhost:9000/b/"))
  b <- atlas_bases()
  expect_identical(b$store, "http://localhost:9000/b")
  expect_identical(b$atlas, "http://localhost:9000/b/marine-atlas")
  expect_identical(b$file,  "https://file.marinesensitivity.org")
  withr::local_options(msens.atlas_bases = list(store = "http://x/b", atlas = "http://y/a"))
  expect_identical(atlas_bases()$atlas, "http://y/a")
  withr::local_options(msens.atlas_bases = list(nope = "http://x"))
  expect_error(atlas_bases(), "unknown")
  # and it reaches the refactored defaults
  withr::local_options(msens.atlas_bases = list(store = "http://localhost:9000/b"))
  expect_identical(stac_cfg()$atlas_base, "http://localhost:9000/b/marine-atlas")
})

test_that("store: anything under the store base, any extension", {
  expect_equal(cls("https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v9/native/am/x.tif"), "store")
  expect_equal(cls("https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v9/manifest.json"), "store")
  # a sibling bucket sharing the prefix is not the store
  expect_equal(cls("https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public-other/x.tif"), "external")
})

test_that("vm_bulk: the published PMTiles pointer URL, and every bulk extension", {
  expect_equal(cls("https://file.marinesensitivity.org/pmtiles/v9/rng_iucn/187464.pmtiles?v=1786644601"), "vm_bulk")
  expect_equal(cls("https://file.marinesensitivity.org/derived/v7/x.gpkg"), "vm_bulk")
  for (ext in c("tif", "tiff", "pmtiles", "parquet", "gpkg", "duckdb", "zip", "gz", "nc"))
    expect_equal(cls(paste0("https://file.marinesensitivity.org/a/b.", ext)), "vm_bulk", info = ext)
  # case-insensitive; query and fragment ignored
  expect_equal(cls("https://file.marinesensitivity.org/a/B.TIF"), "vm_bulk")
  expect_equal(cls("https://file.marinesensitivity.org/a/b.parquet#frag"), "vm_bulk")
  # other project hosts count too
  expect_equal(cls("https://storage.marinesensitivity.org/x/y.parquet"), "vm_bulk")
  expect_equal(cls("https://app.marinesensitivity.org/data.zip"), "vm_bulk")
  expect_equal(cls("https://something.oceanmetrics.io/a.nc"), "vm_bulk")
})

test_that("vm_bulk: directory URLs under /pmtiles/ and /derived/ on the file host", {
  expect_equal(cls("https://file.marinesensitivity.org/pmtiles/v9/dps_nmfs/"), "vm_bulk")
  expect_equal(cls("https://file.marinesensitivity.org/derived/v7/"), "vm_bulk")
  expect_equal(cls("https://file.marinesensitivity.org/pmtiles/"), "vm_bulk")
  # a directory elsewhere, or on another host, is a page
  expect_equal(cls("https://file.marinesensitivity.org/stac/v9/"), "page")
  expect_equal(cls("https://storage.marinesensitivity.org/pmtiles/v9/"), "page")
})

test_that("computed: tile and API hosts, even with a bulk extension in the query", {
  expect_equal(cls("https://titiler-v8.marinesensitivity.org/cog/tiles/WebMercatorQuad/3/1/2.png?url=https%3A%2F%2Fx%2Fx.tif"), "computed")
  expect_equal(cls("https://titiler-v8.marinesensitivity.org/cog/tiles/3/1/2.png?url=https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/x.tif"), "computed")
  expect_equal(cls("https://titiler.marinesensitivity.org/msens/tiles/3/1/2.png"), "computed")
  expect_equal(cls("https://stac-api.marinesensitivity.org/collections"), "computed")
  expect_equal(cls("https://api.marinesensitivity.org/cell_stats?x=1"), "computed")
  for (h in c("tile", "tilecache", "titilecache", "h3t", "pmtiles"))
    expect_equal(cls(sprintf("https://%s.marinesensitivity.org/z/x/y.pbf", h)), "computed", info = h)
  # a computed host is computed even for a bulk path (it renders, it is not a download)
  expect_equal(cls("https://tile.marinesensitivity.org/data.parquet"), "computed")
})

test_that("page: small HTML/JSON on project hosts and the bare domain", {
  expect_equal(cls("https://file.marinesensitivity.org/stac/v9/collection.json"), "page")
  expect_equal(cls("https://storage.marinesensitivity.org/marine-atlas/v8/"), "page")
  expect_equal(cls("https://app.marinesensitivity.org/scores/"), "page")
  expect_equal(cls("https://preview.marinesensitivity.org/v9/scores/?x=a.tif"), "page")
  expect_equal(cls("https://marinesensitivity.org/docs/index.html"), "page")
  expect_equal(cls("https://marinesensitivity.org"), "page")
})

test_that("external and NA", {
  expect_equal(cls("https://obis-maps.s3.amazonaws.com/x.tif"), "external")
  expect_equal(cls("https://example.com/marinesensitivity.org/x.tif"), "external")
  expect_equal(cls("https://notmarinesensitivity.org/x.tif"), "external")
  expect_equal(cls("relative/path.tif"), "external")
  a <- url_audit(c("https://file.marinesensitivity.org/a.tif", NA, "https://x.org/a"))
  expect_equal(a$class, c("vm_bulk", NA, "external"))
  expect_equal(a$host[1], "file.marinesensitivity.org")
  expect_equal(nrow(url_audit(character())), 0)
})

test_that("an option override of `store` reclassifies a URL", {
  u <- "http://localhost:9000/b/marine-atlas/v9/x.tif"
  withr::local_options(msens.atlas_bases = NULL)
  expect_equal(cls(u), "external")
  withr::local_options(msens.atlas_bases = list(store = "http://localhost:9000/b"))
  expect_equal(cls(u), "store")
  # the default store host stops being the store
  expect_equal(cls("https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/x.tif"), "external")
  # an overridden `file` host is still the VM
  withr::local_options(msens.atlas_bases = list(file = "http://vm.local:8080"))
  expect_equal(cls("http://vm.local:8080/pmtiles/v9/a.pmtiles"), "vm_bulk")
  expect_equal(cls("http://vm.local:8080/pmtiles/v9/dps/"), "vm_bulk")
})

test_that("url_audit_assert() stops on vm_bulk, with count and examples; allow is the escape hatch", {
  ok  <- c("https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public/marine-atlas/v9/a.tif",
           "https://file.marinesensitivity.org/stac/v9/collection.json",
           "https://example.com/x.tif")
  bad <- sprintf("https://file.marinesensitivity.org/pmtiles/v9/rng_iucn/%d.pmtiles?v=1", 1:7)
  expect_silent(res <- url_audit_assert(ok, "pointers"))
  expect_equal(res$class, c("store", "page", "external"))

  expect_error(url_audit_assert(c(ok, bad), "pointer table"), "pointer table: 7 URL")
  err <- tryCatch(url_audit_assert(c(ok, bad)), error = conditionMessage)
  expect_equal(lengths(regmatches(err, gregexpr("rng_iucn/", err))), 5L)   # at most 5 examples
  expect_false(grepl("collection.json", err))

  # allow: partial and full
  expect_error(url_audit_assert(bad, allow = "rng_iucn/[1-3][.]"), "4 URL")
  expect_silent(url_audit_assert(bad, allow = "/pmtiles/"))
  expect_s3_class(url_audit_assert(bad, allow = "/pmtiles/"), "data.frame")
})
