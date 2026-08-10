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
