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
