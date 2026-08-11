# A zone set is identified by its GEOMETRY, not by the release that used it.
# The failure modes are both silent: if the hash varies with feature order, six
# identical copies of one layer become six "vintages" and get scored six times;
# if two genuinely different boundaries collapse to one key, a v3 score gets
# compared against a v8 score for a different polygon.

skip_if_no_sf <- function() skip_if_not_installed("sf")

# two squares, keyed
mk_sf <- function(keys = c("A", "B"), shift = 0) {
  sq <- function(x0, y0) sf::st_polygon(list(cbind(
    c(x0, x0 + 1, x0 + 1, x0, x0), c(y0, y0, y0 + 1, y0 + 1, y0))))
  sf::st_sf(programarea_key = keys,
            geometry = sf::st_sfc(sq(0 + shift, 0), sq(2, 2), crs = 4326))
}

test_that("the fingerprint ignores feature order but not geometry", {
  skip_if_no_sf()
  a <- mk_sf()
  b <- a[c(2, 1), ]                                  # same layer, shuffled
  expect_equal(zone_geom_hash(a)$geom_hash, zone_geom_hash(b)$geom_hash)
  expect_false(zone_geom_hash(a)$geom_hash == zone_geom_hash(mk_sf(shift = 0.5))$geom_hash)
})

test_that("the key column is detected and reported", {
  skip_if_no_sf()
  h <- zone_geom_hash(mk_sf())
  expect_equal(h$key_col, "programarea_key")
  expect_equal(h$n, 2L)
  expect_equal(h$keys, c("A", "B"))
  expect_equal(nchar(h$geom_hash), 16L)
})

test_that("a gpkg round-trips to the same fingerprint as its sf", {
  skip_if_no_sf()
  a <- mk_sf()
  f <- withr::local_tempfile(fileext = ".gpkg")
  suppressWarnings(sf::st_write(a, f, quiet = TRUE))
  expect_equal(zone_geom_hash(f)$geom_hash, zone_geom_hash(a)$geom_hash)
  expect_error(zone_geom_hash(file.path(tempdir(), "nope.gpkg")), "no such file")
})

test_that("zone_set_key validates its parts", {
  expect_equal(zone_set_key("programarea", "2026-03"), "programarea_2026-03")
  expect_equal(zone_set_key(c("planarea", "ecoregion"), c("2025-06", "2025-06")),
               c("planarea_2025-06", "ecoregion_2025-06"))
  expect_error(zone_set_key("planningarea", "2026-03"), "unknown zone_type")
  expect_error(zone_set_key("programarea", "2026"),     "YYYY-MM")
  expect_error(zone_set_key("programarea", "v8"),       "YYYY-MM")
})

test_that("one key names one geometry, and one geometry carries one key", {
  ok <- data.frame(zone_set_key = c("programarea_2026-01", "programarea_2026-03"),
                   zone_type = "programarea", vintage = c("2026-01", "2026-03"),
                   geom_hash = c("aaaa", "bbbb"), n_zones = 20)
  expect_silent(validate_zone_sets(ok))

  # same key, two geometries -> which polygon is "programarea_2026-03"?
  bad1 <- rbind(ok, data.frame(zone_set_key = "programarea_2026-03",
                               zone_type = "programarea", vintage = "2026-03",
                               geom_hash = "cccc", n_zones = 20))
  expect_error(validate_zone_sets(bad1), "duplicate zone_set_key")

  # same geometry under two keys -> the same polygons scored twice and compared
  # as though they were different places
  bad2 <- data.frame(zone_set_key = c("programarea_2026-01", "programarea_2026-03"),
                     zone_type = "programarea", vintage = c("2026-01", "2026-03"),
                     geom_hash = c("aaaa", "aaaa"), n_zones = 20)
  expect_error(validate_zone_sets(bad2), "published under >1 key")
})

test_that("validate_zone_sets catches a key that disagrees with its columns", {
  d <- data.frame(zone_set_key = "programarea_2026-03", zone_type = "ecoregion",
                  vintage = "2026-03", geom_hash = "aaaa", n_zones = 12)
  expect_error(validate_zone_sets(d), "does not match")
  expect_error(validate_zone_sets(data.frame(zone_set_key = "x")), "missing column")
})

test_that("zone_set_group collapses identical layers and flags unlabelled ones", {
  # the real shape of the measurement: six per-version program-area files that
  # are byte-identical, plus one older vintage
  x <- data.frame(
    source    = c("v2/pa.gpkg", paste0("v", 3:8, "/pa.gpkg")),
    zone_type = "programarea",
    geom_hash = c("old", rep("new", 6)))
  g <- zone_set_group(x, vintage_of = c(old = "2026-01", new = "2026-03"))
  expect_equal(length(unique(g$zone_set_key)), 2L)
  expect_equal(unique(g$zone_set_key[g$geom_hash == "new"]), "programarea_2026-03")
  expect_silent(validate_zone_sets(data.frame(
    zone_set_key = unique(g$zone_set_key), zone_type = "programarea",
    vintage = c("2026-01", "2026-03"), geom_hash = c("old", "new"), n_zones = 20)))

  # an unrecognized geometry is left NA for a human, not silently named
  g2 <- zone_set_group(x, vintage_of = c(old = "2026-01"))
  expect_true(all(is.na(g2$zone_set_key[g2$geom_hash == "new"])))
})

# --- zone_cells: the (zone x grid) intersection --------------------------------
# These pin the ROUNDING and DROP semantics, because relocating this computation
# out of the per-version pipeline must not move a single score.

test_that("zone_cells returns whole-percent coverage per zone", {
  skip_if_no_sf(); skip_if_not_installed("exactextractr"); skip_if_not_installed("terra")

  # 4x4 cell-id raster over [0,4]x[0,4]; pixel VALUE is the cell id (lookup image)
  f <- withr::local_tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4,
                   crs = "EPSG:4326", vals = 1:16)
  terra::writeRaster(r, f, overwrite = TRUE)

  # a polygon covering exactly cells in the top-left 2x2 block
  sq <- sf::st_polygon(list(cbind(c(0, 2, 2, 0, 0), c(2, 2, 4, 4, 2))))
  ply <- sf::st_sf(zk = "A", geometry = sf::st_sfc(sq, crs = 4326))

  z <- zone_cells(ply, f, "zk")
  expect_equal(unique(z$zone_key), "A")
  expect_equal(sort(z$cell_id), c(1L, 2L, 5L, 6L))   # top-left 2x2, row-major ids
  expect_true(all(z$pct_covered == 100L))
})

test_that("partial coverage rounds to whole percent, and 0% slivers are dropped", {
  skip_if_no_sf(); skip_if_not_installed("exactextractr"); skip_if_not_installed("terra")
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                                 crs = "EPSG:4326", vals = 1:4), f, overwrite = TRUE)

  # covers half of the two left-hand cells
  sq  <- sf::st_polygon(list(cbind(c(0, 0.5, 0.5, 0, 0), c(0, 0, 2, 2, 0))))
  ply <- sf::st_sf(zk = "B", geometry = sf::st_sfc(sq, crs = 4326))
  z   <- zone_cells(ply, f, "zk")
  expect_true(all(z$pct_covered == 50L))

  # a sliver thin enough to round to 0% must be DROPPED, not stored as 0
  thin <- sf::st_polygon(list(cbind(c(0, 0.002, 0.002, 0, 0), c(0, 0, 2, 2, 0))))
  z2   <- zone_cells(sf::st_sf(zk = "C", geometry = sf::st_sfc(thin, crs = 4326)), f, "zk")
  expect_true(is.null(z2) || all(z2$pct_covered > 0))
})

test_that("zone_cells rejects a missing key column", {
  skip_if_no_sf(); skip_if_not_installed("exactextractr")
  ply <- sf::st_sf(other = "A", geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1,1,0)))), crs = 4326))
  expect_error(zone_cells(ply, tempfile(), "zk"), "not a column")
})

test_that("zone_key_col picks the type's own key, not merely the first *_key", {
  # the real program-area gpkg carries all three; region_key has 3 values for 20
  # features, so picking it yields 3 zones instead of 20
  nms <- c("programarea_id", "region_key", "planarea_key", "programarea_key", "geom")
  expect_equal(zone_key_col("programarea", nms), "programarea_key")
  expect_equal(zone_key_col("planarea",    nms), "planarea_key")
  # ecoregion layers carry region_key AND ecoregion_key
  expect_equal(zone_key_col("ecoregion", c("region_key", "ecoregion_key")), "ecoregion_key")
  # fall back only when the type's own key is absent
  expect_equal(zone_key_col("subregion", c("region_key")), "region_key")
  expect_error(zone_key_col("programarea", c("id", "geom")), "no *_key column|no `programarea_key`")
})

test_that("zone_geom_hash honours zone_type when several *_key columns exist", {
  skip_if_no_sf()
  x <- mk_sf()
  x$region_key <- "R"                      # a coarser key, first in name order
  x <- x[, c("region_key", "programarea_key", "geometry")]
  # with a non-unique ordering key the tie order depends on feature order, so a
  # shuffled copy can hash differently -- the type's own key makes it total
  y <- x[c(2, 1), ]
  expect_equal(zone_geom_hash(x, zone_type = "programarea")$key_col, "programarea_key")
  expect_equal(zone_geom_hash(x, zone_type = "programarea")$geom_hash,
               zone_geom_hash(y, zone_type = "programarea")$geom_hash)
})

# zone_set_resolve() --------------------------------------------------------
# Regression: only v8 stamps zone_set_key into its own `zone` table, so v1-v7
# manifests were published with no zone_set_key and therefore no zone PMTiles --
# the app could not draw an outline on any release but the newest.

reg <- data.frame(
  zone_set_key = c("ecoregion_2025-06", "programarea_2026-01",
                   "planarea_2025-06", "subregion_2025-08"),
  zone_type    = c("ecoregion", "programarea", "planarea", "subregion"),
  versions     = c("v1 v2 v7 v8", "v2 v7 v8", "v1 v4", "v1"),
  stringsAsFactors = FALSE)

test_that("zone_set_resolve maps a legacy release's fld to its vintage", {
  expect_equal(zone_set_resolve("v7", "programarea_key", reg), "programarea_2026-01")
  expect_equal(zone_set_resolve("v7", c("ecoregion_key", "programarea_key"), reg),
               c("ecoregion_2025-06", "programarea_2026-01"))
})

test_that("a version not listed for that zone type resolves to NA, not a guess", {
  # v7 has no planarea vintage in the registry; drawing v1's outlines over v7
  # scores would look entirely plausible and be wrong
  expect_true(is.na(zone_set_resolve("v7", "planarea_key", reg)))
  expect_true(is.na(zone_set_resolve("v3", "subregion_key", reg)))
})

test_that("an ambiguous registry resolves to NA rather than picking one", {
  amb <- rbind(reg, data.frame(
    zone_set_key = "programarea_2026-06", zone_type = "programarea",
    versions = "v7", stringsAsFactors = FALSE))
  expect_true(is.na(zone_set_resolve("v7", "programarea_key", amb)))
})

test_that("substring version names do not match", {
  # "v1" must not match "v10"/"v1b" via a bare grepl
  r <- data.frame(zone_set_key = "programarea_2026-01", zone_type = "programarea",
                  versions = "v10 v4b", stringsAsFactors = FALSE)
  expect_true(is.na(zone_set_resolve("v1", "programarea_key", r)))
  expect_equal(zone_set_resolve("v4b", "programarea_key", r), "programarea_2026-01")
})
