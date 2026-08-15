# The version registry is what lets ONE app render every release. Its failure mode
# is silent and severe: resolving to a plausible-but-wrong version renders the wrong
# science under the right label. So every test here asserts an ERROR where a lenient
# implementation would guess.
#
# `.atlas_fetch()` uses readLines(), which reads local paths as happily as URLs, so
# a temp directory stands in for the bucket and none of this touches the network.

# build a fake atlas root; returns its path (usable as `base =`)
fake_atlas <- function(latest = "v8",
                       versions = data.frame(
                         ver      = c("v9", "v8", "v7", "v6"),
                         status   = c("prerelease", "released", "released", "retired"),
                         released = c(NA, "2026-07-28", "2026-06-12", "2026-04-09")),
                       manifests = list(v8 = NULL)) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(latest, file.path(d, "latest.txt"))
  writeLines(jsonlite::toJSON(list(versions = versions), auto_unbox = TRUE, na = "null"),
             file.path(d, "versions.json"))
  for (v in names(manifests)) {
    dir.create(file.path(d, v), showWarnings = FALSE)
    m <- manifests[[v]] %||% list(
      ver = v, status = "released", grid_id = "global05", id_field = "mdl_key",
      capabilities = list(cell_species_list = TRUE, native_representation = TRUE),
      tables = list(taxon = "taxon.parquet"))
    writeLines(jsonlite::toJSON(m, auto_unbox = TRUE), file.path(d, v, "manifest.json"))
  }
  d
}

`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("atlas_latest reads latest.txt and rejects junk", {
  expect_equal(atlas_latest(fake_atlas(latest = "v8")), "v8")
  # trailing whitespace/newlines are normal in a hand- or CI-written pointer file
  expect_equal(atlas_latest(fake_atlas(latest = "  v4b \n")), "v4b")
  expect_error(atlas_latest(fake_atlas(latest = "main")), "valid version")
  expect_error(atlas_latest(file.path(tempdir(), "nope")), "could not read")
})

test_that("atlas_resolve_ver defaults to latest, never to a hardcoded version", {
  b <- fake_atlas()
  expect_equal(atlas_resolve_ver(NULL, b),     "v8")
  expect_equal(atlas_resolve_ver("latest", b), "v8")
  expect_equal(atlas_resolve_ver("", b),       "v8")
  expect_equal(atlas_resolve_ver("v7", b),     "v7")
})

test_that("a pre-release is reachable only by name, never as 'latest'", {
  b <- fake_atlas(latest = "v8")   # v9 exists but is prerelease
  expect_equal(atlas_resolve_ver(NULL, b), "v8")
  expect_equal(atlas_resolve_ver("v9", b), "v9")
  # ...and a caller may refuse pre-releases outright
  expect_error(atlas_resolve_ver("v9", b, allow = "released"), "status 'prerelease'")
})

test_that("unknown or malformed versions error rather than resolve", {
  b <- fake_atlas()
  expect_error(atlas_resolve_ver("v99", b),        "unknown version")
  expect_error(atlas_resolve_ver("../etc", b),     "not a version label")
  expect_error(atlas_resolve_ver("v8; DROP", b),   "not a version label")
})

test_that("atlas_versions rejects an unrecognized status", {
  b <- fake_atlas(versions = data.frame(ver = "v8", status = "beta"))
  expect_error(atlas_versions(b), "unknown status")
})

# --- access: who may be shown a version --------------------------------------
# `status` (where a release is in its life) and `access` (who may see it) are
# separate axes. A pre-release under review is `restricted`: shown only through
# the signed-in preview host. The failure mode to guard is a LEAK, so every
# default here closes rather than opens.

test_that("access is derived fail-closed when versions.json predates the field", {
  d <- atlas_versions(fake_atlas())          # fake_atlas writes no `access`
  expect_true("access" %in% names(d))
  expect_equal(d$access[d$ver == "v9"], "restricted")   # prerelease -> restricted
  expect_equal(d$access[d$ver == "v8"], "public")       # released
  expect_equal(d$access[d$ver == "v6"], "public")       # retired stays citable
  expect_equal(atlas_access_default(c("prerelease", "released", "retired")),
               c("restricted", "public", "public"))
})

test_that("an explicit access column wins over the derived default, per row", {
  b <- fake_atlas(versions = data.frame(
    ver = c("v9", "v8", "v7"), status = c("prerelease", "released", "retired"),
    access = c("public", "restricted", NA)))            # NA -> derived
  d <- atlas_versions(b)
  expect_equal(d$access[d$ver == "v9"], "public")       # a public pre-release is allowed
  expect_equal(d$access[d$ver == "v8"], "restricted")   # a released version may be gated
  expect_equal(d$access[d$ver == "v7"], "public")       # NA derived from status
  expect_equal(atlas_ver_access("v8", b), "restricted")
  expect_error(atlas_ver_access("v99", b), "unknown version")
})

test_that("atlas_versions rejects an unrecognized access value", {
  b <- fake_atlas(versions = data.frame(ver = "v8", status = "released", access = "hidden"))
  expect_error(atlas_versions(b), "unknown access")
})

test_that("the public instance cannot resolve a restricted version; preview can", {
  b <- fake_atlas()                                     # v9 prerelease -> restricted
  # library default stays permissive: notebooks and the docs CI render restricted releases
  expect_equal(atlas_resolve_ver("v9", b), "v9")
  # the public app instance passes allow_access = "public" and gets a CLASSED error,
  # distinguishable from "unknown version" so it can point at the preview host
  expect_error(atlas_resolve_ver("v9", b, allow_access = "public"), class = "msens_restricted")
  e <- tryCatch(atlas_resolve_ver("v9", b, allow_access = "public"), msens_restricted = function(e) e)
  expect_equal(e$ver, "v9")
  expect_equal(e$access, "restricted")
  expect_match(conditionMessage(e), "preview.marinesensitivity.org")
  # public versions resolve on either instance
  expect_equal(atlas_resolve_ver("v7", b, allow_access = "public"), "v7")
  # ...and `latest` is never affected: the promoted version is public by construction
  expect_equal(atlas_resolve_ver(NULL, b, allow_access = "public"), "v8")
})

test_that("atlas_allow_access is the instance policy: MS_PREVIEW decides", {
  withr::local_envvar(MS_PREVIEW = "")
  expect_false(atlas_is_preview())
  expect_equal(atlas_allow_access(), "public")
  withr::local_envvar(MS_PREVIEW = "1")
  expect_true(atlas_is_preview())
  expect_setequal(atlas_allow_access(), c("public", "restricted"))
  withr::local_envvar(MS_PREVIEW = "true")
  expect_true(atlas_is_preview())
  # explicit argument beats the environment (tests, notebooks)
  expect_equal(atlas_allow_access(preview = FALSE), "public")
})

test_that("atlas_preview_url is overridable and never carries a trailing slash", {
  withr::local_envvar(MS_PREVIEW_URL = "")
  expect_equal(atlas_preview_url(), "https://preview.marinesensitivity.org")
  withr::local_envvar(MS_PREVIEW_URL = "https://review.example.org/")
  expect_equal(atlas_preview_url(), "https://review.example.org")
})

test_that("atlas_manifest resolves, fetches and validates", {
  m <- atlas_manifest("v8", fake_atlas())
  expect_equal(m$ver, "v8")
  expect_true(manifest_can(m, "cell_species_list"))
})

test_that("a missing capabilities block fails loudly", {
  # the whole point: an absent capability must NOT read as "supported"
  bad <- list(ver = "v8", status = "released", grid_id = "global05",
              id_field = "mdl_key", tables = list(taxon = "t.parquet"))
  expect_error(validate_manifest(bad), "missing required key")

  empty <- c(bad, list(capabilities = setNames(list(), character())))
  expect_error(validate_manifest(empty), "non-empty")
})

test_that("manifest_can is FALSE for capabilities a release never declared", {
  m <- list(capabilities = list(cell_species_list = TRUE))
  expect_true(manifest_can(m, "cell_species_list"))
  expect_false(manifest_can(m, "native_representation"))   # absent
  expect_false(manifest_can(m, "totally_made_up"))
})

test_that("validate_manifest guards id_field and the ver cross-check", {
  ok <- list(ver = "v6", status = "released", grid_id = "usa05", id_field = "mdl_seq",
             capabilities = list(cell_species_list = TRUE), tables = list(taxon = "t.parquet"))
  expect_equal(validate_manifest(ok, ver = "v6")$ver, "v6")
  expect_error(validate_manifest(ok, ver = "v7"), "declares ver")
  expect_error(validate_manifest(modifyList(ok, list(id_field = "mdl_id"))), "id_field")
})

# --- manifest_build: introspect the release, never assume -------------------

mk_rel <- function(env = parent.frame(), v8 = TRUE, extras = character()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  idc <- if (v8) "mdl_key VARCHAR" else "mdl_seq INTEGER"
  DBI::dbExecute(con, sprintf("CREATE TABLE model (%s, ds_key VARCHAR)", idc))
  DBI::dbExecute(con, "CREATE TABLE metric (metric_seq INTEGER, metric_key VARCHAR, description VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO metric VALUES (1,'extrisk_bird','x'), (2,'ecoregion_min','zone-only')")
  DBI::dbExecute(con, "CREATE TABLE cell_metric (cell_id BIGINT, metric_seq INTEGER, val DOUBLE)")
  DBI::dbExecute(con, "INSERT INTO cell_metric VALUES (1, 1, 5)")   # metric 2 is zone-only
  DBI::dbExecute(con, "CREATE TABLE zone (zone_seq INTEGER, tbl VARCHAR, fld VARCHAR, val VARCHAR)")
  DBI::dbExecute(con, sprintf("INSERT INTO zone VALUES (1,'ply_x','%s','A')",
                              if (v8) "programarea_key" else "planarea_key"))
  DBI::dbExecute(con, "CREATE TABLE taxon (taxon_id VARCHAR)")
  for (t in extras) DBI::dbExecute(con, sprintf("CREATE TABLE %s (x INTEGER)", t))
  con
}

test_that("manifest_build reads the public model id off the release", {
  expect_equal(manifest_build(mk_rel(v8 = TRUE),  "v8")$id_field, "mdl_key")
  expect_equal(manifest_build(mk_rel(v8 = FALSE), "v7")$id_field, "mdl_seq")
})

test_that("manifest_build records access, defaulting from status", {
  expect_equal(manifest_build(mk_rel(), "v8", status = "prerelease")$access, "restricted")
  expect_equal(manifest_build(mk_rel(), "v8", status = "released")$access,   "public")
  expect_equal(manifest_build(mk_rel(), "v8", status = "prerelease", access = "public")$access,
               "public")
  expect_error(manifest_build(mk_rel(), "v8", access = "hidden"), "access")
})

test_that("capabilities are derived from presence and default to FALSE", {
  bare <- manifest_build(mk_rel(v8 = FALSE), "v6")
  # no cell_model / native_asset / zone_taxon in this release
  expect_false(manifest_can(bare, "cell_species_list"))
  expect_false(manifest_can(bare, "native_representation"))
  expect_false(manifest_can(bare, "zone_taxon"))
  # v1/v2-style release: Planning Areas, NOT Program Areas
  expect_true(manifest_can(bare,  "planareas"))
  expect_false(manifest_can(bare, "programareas"))

  full <- manifest_build(mk_rel(v8 = TRUE, extras = c("cell_model", "native_asset", "zone_taxon")), "v8")
  expect_true(manifest_can(full, "cell_species_list"))
  expect_true(manifest_can(full, "native_representation"))
  expect_true(manifest_can(full, "programareas"))
})

test_that("only cell-level metrics are declared, and score_cogs waits for real COGs", {
  m <- manifest_build(mk_rel(), "v8")
  expect_equal(m$metrics$metric_key, "extrisk_bird")   # the zone-only metric is excluded
  expect_false(manifest_can(m, "score_cogs"))          # none published yet

  withcog <- manifest_build(mk_rel(), "v8", metrics = data.frame(
    metric_key = "extrisk_bird", subregion_key = "FULL",
    cog = "https://x/cog/global05/abc.tif", rescale_min = 0, rescale_max = 96))
  expect_true(manifest_can(withcog, "score_cogs"))
  expect_match(withcog$metrics$cog, "abc\\.tif")
})

test_that("manifest_build declares only tables the release actually has", {
  m <- manifest_build(mk_rel(v8 = FALSE, extras = "zone_taxon"), "v6",
                      base = "https://x/marine-atlas")
  expect_true(all(c("model", "metric", "zone", "taxon", "zone_taxon") %in% names(m$tables)))
  expect_false("native_asset" %in% names(m$tables))
  expect_equal(m$tables$taxon, "https://x/marine-atlas/v6/tables/taxon.parquet")
})

test_that("manifest_build refuses a version with no registered grid", {
  expect_error(manifest_build(mk_rel(), "v99"), "no grid registered")
})

test_that("capability overrides cover surfaces that are not tables in `con`", {
  # v8's cell_model is a Parquet directory beside the DB, not a table: without an
  # override the manifest would advertise FALSE and disable a working panel
  m <- manifest_build(mk_rel(), "v8", capabilities = list(cell_species_list = TRUE))
  expect_true(manifest_can(m, "cell_species_list"))
  # an override may also switch something OFF (e.g. a table present but not released)
  expect_false(manifest_can(
    manifest_build(mk_rel(v8 = TRUE, extras = "native_asset"), "v8",
                   capabilities = list(native_representation = FALSE)),
    "native_representation"))
})

test_that("malformed capability overrides are rejected, not silently ignored", {
  expect_error(manifest_build(mk_rel(), "v8", capabilities = list(TRUE)), "NAMED list")
  expect_error(manifest_build(mk_rel(), "v8", capabilities = list(x = "yes")), "single logicals")
})

test_that("zones carry zone_set_key and its PMTiles when the release has them", {
  con <- mk_rel(v8 = TRUE)
  DBI::dbExecute(con, "ALTER TABLE zone ADD COLUMN zone_set_key VARCHAR")
  DBI::dbExecute(con, "UPDATE zone SET zone_set_key = 'programarea_2026-01'")
  m <- manifest_build(con, "v8",
    zone_tiles = list("programarea_2026-01" = "https://x/zones/programarea_2026-01/zones.pmtiles"))
  expect_true("zone_set_key" %in% names(m$zones))
  expect_match(m$zones$pmtiles[1], "programarea_2026-01/zones\\.pmtiles$")
})

test_that("a release predating zone_set_key still builds a manifest", {
  # v1-v7 as originally written have no such column; the app falls back
  m <- manifest_build(mk_rel(v8 = FALSE), "v7")
  expect_false("zone_set_key" %in% names(m$zones))
  expect_gt(nrow(m$zones), 0)
})

# disk cache ----------------------------------------------------------------
# shiny-server starts a fresh R process per session, so the in-process memo never
# survives; without a disk cache every visitor re-fetched latest.txt +
# versions.json + manifest.json (~0.58 s) inside time-to-first-byte.

test_that("a fetch is served from disk by a process with a cold memo", {
  d <- withr::local_tempdir()
  withr::local_envvar(MSENS_ATLAS_CACHE = d, MSENS_ATLAS_TTL = "300")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("v7", f)
  u <- paste0("file://", f)

  expect_equal(trimws(msens:::.atlas_fetch(u)), "v7")
  expect_length(list.files(d), 1)              # landed on disk

  # simulate a NEW process: clear the in-memory memo, then remove the source.
  # a disk hit is the only way this can still answer.
  rm(list = ls(envir = msens:::.atlas_reg), envir = msens:::.atlas_reg)
  unlink(f)
  expect_equal(trimws(msens:::.atlas_fetch(u)), "v7")
})

test_that("an expired entry is NOT served — a promoted latest.txt must be seen", {
  d <- withr::local_tempdir()
  withr::local_envvar(MSENS_ATLAS_CACHE = d, MSENS_ATLAS_TTL = "300")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("v7", f)
  u <- paste0("file://", f)
  msens:::.atlas_fetch(u)

  # age the cache file past the TTL and update the source
  cf <- list.files(d, full.names = TRUE)[1]
  Sys.setFileTime(cf, Sys.time() - 3600)
  writeLines("v8", f)
  rm(list = ls(envir = msens:::.atlas_reg), envir = msens:::.atlas_reg)
  expect_equal(trimws(msens:::.atlas_fetch(u)), "v8")
})

test_that("TTL=0 and refresh=TRUE both bypass the cache entirely", {
  d <- withr::local_tempdir()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("v7", f)
  u <- paste0("file://", f)

  withr::local_envvar(MSENS_ATLAS_CACHE = d, MSENS_ATLAS_TTL = "0")
  msens:::.atlas_fetch(u)
  expect_length(list.files(d), 0)              # nothing written when disabled

  writeLines("v8", f)
  expect_equal(trimws(msens:::.atlas_fetch(u, refresh = TRUE)), "v8")
})

test_that("distinct urls do not collide in the cache", {
  expect_false(identical(msens:::.hash_str("https://x/latest.txt"),
                         msens:::.hash_str("https://x/versions.json")))
})

test_that("zones collapse to one row per spatial unit", {
  # v2/v3 carry TWO subregion tables under one fld; both resolve to the canonical
  # vintage, so the manifest listed subregion twice and a manifest-driven picker
  # would offer the same choice twice
  z <- data.frame(
    zone_set_key = c("subregion_2025-06", "subregion_2025-06", "programarea_2026-01"),
    tbl = c("ply_subregions_a", "ply_subregions_b", "ply_programareas_2026"),
    fld = c("subregion_key", "subregion_key", "programarea_key"),
    n   = c(4, 9, 20), stringsAsFactors = FALSE)
  ord <- order(z$zone_set_key, -as.numeric(z$n))
  z2  <- z[ord, , drop = FALSE]
  z2  <- z2[!duplicated(z2$zone_set_key) | is.na(z2$zone_set_key), , drop = FALSE]
  expect_equal(nrow(z2), 2)
  expect_equal(z2$n[z2$fld == "subregion_key"], 9)   # keeps the larger count
})
