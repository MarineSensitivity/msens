test_that("cells_from_ranges captures the whole range (land + ocean), no mask", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("exactextractr")

  # global cell-id grid: 10x10, cell_id = 1:100 for EVERY cell (no land NA)
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 0.5,
                   ymin = 0, ymax = 0.5, crs = "EPSG:4326")
  terra::values(r) <- 1:100
  tif <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tif, overwrite = TRUE)

  # polygon covering the left half (x in [0, 0.25], all y) -> 5 of 10 columns
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(0.25, 0), c(0.25, 0.5), c(0, 0.5), c(0, 0)))), crs = 4326))

  b <- cells_from_ranges(poly, tif)                 # presence = 100, any overlap
  expect_true(all(b$value == 100))
  expect_true(nrow(b) >= 50)                         # left half (no land masking)
  expect_true(all(b$cell_id %in% 1:100))

  cv <- cells_from_ranges(poly, tif, cover = TRUE)  # coverage-weighted
  expect_true(max(cv$value) <= 100 && min(cv$value) > 0)
})

test_that("cells_pct_marine reports the marine share", {
  expect_equal(cells_pct_marine(c(1, 2, 3, 4), ocean_cell_ids = c(1, 2)), 50)
  expect_true(is.na(cells_pct_marine(integer(), ocean_cell_ids = 1:10)))
  # area-weighted: cells 1,2 ocean with areas 3,1; cells 3,4 land areas 1,1
  expect_equal(
    cells_pct_marine(c(1, 2, 3, 4), ocean_cell_ids = c(1, 2),
                     area_km2 = c(3, 1, 1, 1)), 66.7)
})

test_that("cells_from_aligned_raster maps by position, scales, thresholds, drops land", {
  skip_if_not_installed("terra")
  # 4x4 cell-id grid with two land (NA) pixels; ids are NOT positions (a lookup image)
  rc <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 0.2, ymin = 0, ymax = 0.2, crs = "EPSG:4326")
  terra::values(rc) <- c(101:104, 105:108, NA, 110:112, NA, 114:116)
  tif <- tempfile(fileext = ".tif"); terra::writeRaster(rc, tif, overwrite = TRUE, datatype = "INT4U")
  # source on the same grid, values 0-1000 with NA and a sub-threshold pixel
  r <- terra::rast(rc); terra::values(r) <- c(500, NA, 5, 1000, rep(NA, 4), 300, NA, NA, NA, 700, NA, NA, NA)
  d <- cells_from_aligned_raster(r, tif, scale = 0.1)
  # pixel 1 -> id 101 val 50; pixel 3 (0.5 after scaling) is below min_value; pixel 4 -> 104/100;
  # pixel 9 is LAND in the id raster and is dropped even though the source has a value;
  # pixel 13 is land too
  expect_equal(d$cell_id, c(101L, 104L))
  expect_equal(d$val, c(50, 100))
  # the pre-read id vector path gives the same answer
  ids <- terra::values(rc, mat = FALSE)
  expect_equal(cells_from_aligned_raster(r, ids, scale = 0.1), d)
  # a raster on another grid is refused, never silently resampled
  r2 <- terra::rast(nrows = 4, ncols = 5, xmin = 0, xmax = 0.25, ymin = 0, ymax = 0.2, crs = "EPSG:4326")
  terra::values(r2) <- 1
  expect_error(cells_from_aligned_raster(r2, tif), "not on this grid")
})

test_that("cells_from_raster keeps native-unit densities when told the digits (gm regression)", {
  skip_if_not_installed("terra")
  # a 4x4 cell-id grid and a source ALREADY on it holding densities in animals km^-2 — the
  # values the SEFSC Gulf models carry for beaked, sperm and Rice's whales (0.001-0.01)
  rc <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 0.2, ymin = 0, ymax = 0.2, crs = "EPSG:4326")
  terra::values(rc) <- 1:16
  tif <- tempfile(fileext = ".tif"); terra::writeRaster(rc, tif, overwrite = TRUE, datatype = "INT4U")
  r <- terra::rast(rc); terra::values(r) <- c(0.004, 0.001, 0.012, 0.007, rep(0.003, 12))
  # the default 2-decimal rounding — right for a [0,100] surface — zeroes all but one cell, and
  # the survivor is quantized to 0.01: the field of 0s and 100s the v8 gm ingest published
  d2 <- cells_from_raster(r, tif, min_value = 0, zero_fill = FALSE)
  expect_equal(sort(unique(d2$val)), c(0, 0.01))
  # with the digits kept, every cell survives at its own density
  d6 <- cells_from_raster(r, tif, min_value = 0, zero_fill = FALSE, digits = 6)
  expect_equal(nrow(d6), 16)
  expect_equal(d6$val[d6$cell_id == 2], 0.001)
  expect_equal(d6$val[d6$cell_id == 3], 0.012)
  dn <- cells_from_raster(r, tif, min_value = 0, zero_fill = FALSE, digits = NULL)
  expect_equal(dn$val, d6$val)
})

# area-weighted regrid (poly_cell_overlap + cells_area_mean) ----
# area of a lon/lat quadrangle on the WGS84 ellipsoid (authalic q), independent of terra
wgs84_quad_km2 <- function(lat1, lat2, dlon) {
  e2 <- 0.00669437999014; e <- sqrt(e2); a <- 6378.137
  q  <- function(phi) { s <- sin(phi * pi / 180)
    (1 - e2) * (s / (1 - e2 * s^2) - (1 / (2 * e)) * log((1 - e * s) / (1 + e * s))) }
  (dlon * pi / 180) / 2 * a^2 * (q(lat2) - q(lat1))
}

# 4 x 2 grid of 1-degree cells, lon 0..4, lat 0..2; ids row-major from the top-left:
#   row 1 (lat 1-2): 1 2 3 4     row 2 (lat 0-1): 5 6 7 8
synth_regrid <- function() {
  r <- terra::rast(nrows = 2, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- 1:8
  tif <- tempfile(fileext = ".tif"); terra::writeRaster(r, tif, overwrite = TRUE, datatype = "INT4U")
  sq <- function(x0, x1, y0, y1) sf::st_polygon(list(rbind(
    c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))))
  x <- sf::st_sf(
    d  = c(2, 4, 10, 6),
    cv = c(0.5, NA, 1, 0.2),
    geometry = sf::st_sfc(
      sq(0,   0.5, 1, 2),     # A: west half of cell 1
      sq(0.5, 1,   1, 2),     # B: east half of cell 1 (cv NA)
      sq(1,   2,   0, 2),     # C: all of cells 2 and 6
      sq(2,   2.5, 0, 1)),    # D: west half of cell 7
    crs = 4326)
  list(tif = tif, x = x)
}

test_that("poly_cell_overlap gives exact overlap areas on the ellipsoid", {
  skip_if_not_installed("exactextractr")
  s <- synth_regrid()
  w <- poly_cell_overlap(s$x, s$tif)
  w <- w[order(w$poly_i, w$cell_id), ]
  # one row per (polygon, cell) with positive overlap; edge-touching neighbours are not rows
  expect_equal(w$poly_i,  c(1L, 2L, 3L, 3L, 4L))
  expect_equal(w$cell_id, c(1L, 1L, 2L, 6L, 7L))
  # terra::cellSize() measures a cell as a geodesic polygon, whose east-west edges bow off the
  # parallels: 2.6e-5 relative for these 1-degree cells, 1.5e-8 for a 0.05-degree cell
  a_top <- wgs84_quad_km2(1, 2, 1); a_bot <- wgs84_quad_km2(0, 1, 1)
  expect_equal(w$cell_km2, c(a_top, a_top, a_top, a_bot, a_bot), tolerance = 1e-4)
  expect_equal(w$area_km2, c(a_top / 2, a_top / 2, a_top, a_bot, a_bot / 2), tolerance = 1e-4)
  # overlap areas are coverage x the cell's own area, exactly
  expect_equal(w$area_km2 / w$cell_km2, c(0.5, 0.5, 1, 1, 0.5))
  # the equatorial 1-degree quadrangle is 12,308.46 km2 on WGS84 (a 6371 km sphere: 12,363.7)
  expect_equal(a_bot, 12308.46, tolerance = 1e-6)
  # a 0.05-degree cell at 40 N (the SWFSC CCE case) agrees with the ellipsoid to 1e-6
  r05 <- terra::rast(nrows = 1, ncols = 1, xmin = 0, xmax = 0.05, ymin = 40, ymax = 40.05, crs = "EPSG:4326")
  terra::values(r05) <- 1L
  tif05 <- tempfile(fileext = ".tif"); terra::writeRaster(r05, tif05, overwrite = TRUE, datatype = "INT4U")
  sq05 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 40), c(0.05, 40), c(0.05, 40.05), c(0, 40.05), c(0, 40)))), crs = 4326))
  expect_equal(poly_cell_overlap(sq05, tif05)$cell_km2, wgs84_quad_km2(40, 40.05, 0.05), tolerance = 1e-6)
})

test_that("cells_area_mean is the overlap-area-weighted mean, NA-aware, with covered fraction", {
  skip_if_not_installed("exactextractr")
  s <- synth_regrid()
  w <- poly_cell_overlap(s$x, s$tif)
  m <- cells_area_mean(w, sf::st_drop_geometry(s$x)[c("d", "cv")])
  expect_named(m, c("cell_id", "d", "cv", "area_km2", "frac"))
  expect_equal(m$cell_id, c(1L, 2L, 6L, 7L))           # sorted; untouched cells absent
  expect_equal(m$d,    c(3, 10, 10, 6))                  # cell 1 = (2 + 4) / 2 by equal halves
  expect_equal(m$cv,   c(0.5, 1, 1, 0.2))                # cell 1: B's NA cv drops out of cv only
  expect_equal(m$frac, c(1, 1, 1, 0.5))                  # cell 7 is half covered
  # a density regridded this way conserves abundance: sum(d_c * area_c) == sum(d_p * A_p)
  a_p <- tapply(w$area_km2, w$poly_i, sum)
  expect_equal(sum(m$d * m$area_km2), sum(s$x$d * a_p))
  # an attribute NA in every overlapping polygon is NA in the cell, not 0
  v <- data.frame(d = c(NA, NA, 1, 1))
  expect_true(is.na(cells_area_mean(w, v)$d[1]))
  # an empty overlap table gives an empty, typed result
  e <- cells_area_mean(w[0, ], data.frame(d = 1))
  expect_equal(nrow(e), 0L); expect_named(e, c("cell_id", "d", "area_km2", "frac"))
  # values must line up with the polygons
  expect_error(cells_area_mean(w, data.frame(d = 1:2)))
})
