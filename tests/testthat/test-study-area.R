# Study-area camera presets (apps#14). The recurring bug these guard against is
# a bounding box meeting the antimeridian: two of the twelve ecoregions cross it,
# and a naive extent reports them as spanning the globe.

test_that("the spherical mean does not fall apart at the antimeridian", {
  # either side of the dateline: the answer is ON the dateline, NOT at lon 0,
  # which is what averaging the raw numbers would give
  s <- sphere_centroid(c(170, -170), c(0, 0))
  expect_equal(abs(s[1]), 180, tolerance = 1e-6)
  expect_equal(s[2], 0, tolerance = 1e-6)
  # and the plain case still behaves
  expect_equal(sphere_centroid(c(-10, 10), c(0, 0)), c(0, 0), tolerance = 1e-6)
  expect_equal(sphere_centroid(c(-100, -100), c(30, 40))[1], -100, tolerance = 1e-6)
})

test_that("sphere_centroid handles degenerate input", {
  expect_true(all(is.na(sphere_centroid(numeric(0), numeric(0)))))
  expect_true(all(is.na(sphere_centroid(c(NA, NaN), c(NA, NaN))))) 
  # antipodal points have no mean direction — say so rather than return a
  # numerically-arbitrary point
  expect_true(all(is.na(sphere_centroid(c(0, 180), c(0, 0)))))
  # NAs are dropped, not propagated
  expect_equal(sphere_centroid(c(-100, NA), c(30, NA))[1], -100, tolerance = 1e-6)
})

test_that("angular distance is great-circle, so the dateline is not a wall", {
  expect_equal(angular_distance(c(179, 0), c(-179), c(0)), 2, tolerance = 1e-6)
  expect_equal(angular_distance(c(0, 0), c(0), c(90)), 90, tolerance = 1e-6)
  expect_equal(angular_distance(c(0, 0), c(0), c(0)), 0, tolerance = 1e-6)
})

test_that("zoom tightens as the radius shrinks, and is clamped", {
  z <- vapply(c(90, 60, 20, 10, 5), view_zoom, 0)
  expect_true(all(diff(z) > 0))              # smaller radius -> higher zoom
  expect_true(all(z >= 1.2 & z <= 5))
  expect_equal(view_zoom(0), 5)              # degenerate: fully zoomed in
  expect_equal(view_zoom(NA), 5)
  expect_equal(view_zoom(180), 1.2)          # whole globe: clamped out
})

test_that("the baked presets are the four ecoregion regions plus the whole", {
  s <- study_areas()
  expect_equal(s$key, c("FULL", "AK", "AT", "GA", "PA"))
  # every ecoregion appears exactly once across the four regions, and FULL is
  # their union -- a rollup that dropped or double-counted one would show here
  regions <- s[s$key != "FULL", ]
  eco <- unlist(strsplit(regions$ecoregions, " "))
  expect_equal(anyDuplicated(eco), 0)
  expect_setequal(eco, unlist(strsplit(s$ecoregions[s$key == "FULL"], " ")))
  expect_length(eco, 12)
  expect_true("PIS" %in% eco)                # the one st_cast used to drop
  expect_true(all(is.finite(c(s$lon, s$lat, s$zoom))))
  expect_true(all(s$zoom >= 1.2 & s$zoom <= 5))
})

test_that("no preset centre sits in the wrong ocean", {
  s <- study_areas()
  ctr <- function(k) unlist(s[s$key == k, c("lon", "lat")])
  # Alaska is north and west; the Gulf is in the Gulf; the Atlantic is east
  expect_true(ctr("AK")[["lat"]] > 55 && ctr("AK")[["lon"]] < -140)
  expect_true(ctr("GA")[["lat"]] > 20 && ctr("GA")[["lat"]] < 35)
  expect_true(ctr("GA")[["lon"]] > -100 && ctr("GA")[["lon"]] < -80)
  expect_true(ctr("AT")[["lon"]] > -85 && ctr("AT")[["lon"]] < -60)
  # "All US waters" must frame North America, not the mid-Pacific. Area
  # weighting put it at (-158.6, 39.6) with the Gulf and east coast behind the
  # horizon; this is the assertion that rejects that.
  expect_true(ctr("FULL")[["lon"]] > -115 && ctr("FULL")[["lon"]] < -85)
  expect_true(ctr("FULL")[["lat"]] > 35 && ctr("FULL")[["lat"]] < 60)
})

test_that("study_area_views reproduces the shape from a synthetic layer", {
  skip_if_not_installed("sf")
  sq <- function(x, y, d = 2) sf::st_polygon(list(cbind(
    c(x-d, x+d, x+d, x-d, x-d), c(y-d, y-d, y+d, y+d, y-d))))
  x <- sf::st_sf(
    region_key    = c("AK", "AK", "GA", "AT"),
    ecoregion_key = c("EBS", "GOA", "WCGOA", "NECS"),
    geometry = sf::st_sfc(sq(-170, 58), sq(-150, 57), sq(-92, 27), sq(-70, 40), crs = 4326))
  v <- study_area_views(x)
  expect_equal(v$key, c("FULL", "AK", "AT", "GA"))
  expect_equal(v$ecoregions[v$key == "AK"], "EBS GOA")
  # AK's centre lands between its two boxes
  expect_true(v$lon[v$key == "AK"] > -172 && v$lon[v$key == "AK"] < -148)
  # a tighter region gets a higher zoom than the whole
  expect_gt(v$zoom[v$key == "GA"], v$zoom[v$key == "FULL"])
})

test_that("study_area_views keeps a MULTIPOLYGON whose first part is EMPTY", {
  skip_if_not_installed("sf")
  # exactly how PIS is stored: st_cast(., "MULTIPOINT") returns nothing for it
  # and the largest ecoregion silently disappeared from the derivation
  sq <- function(x, y, d = 2) list(cbind(
    c(x-d, x+d, x+d, x-d, x-d), c(y-d, y-d, y+d, y+d, y-d)))
  odd <- sf::st_multipolygon(list(list(), sq(-160, 20)))   # EMPTY part first
  x <- sf::st_sf(
    region_key = c("PA", "PA"), ecoregion_key = c("PIS", "CAC"),
    geometry = sf::st_sfc(odd, sf::st_multipolygon(list(sq(-124, 38))), crs = 4326))
  v <- study_area_views(x)
  expect_true(grepl("PIS", v$ecoregions[v$key == "PA"]))
  expect_true(grepl("CAC", v$ecoregions[v$key == "PA"]))
})
