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
