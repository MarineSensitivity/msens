test_that("cells_from_ranges rasterizes presence + coverage, ocean-masked", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # tiny cell-id COG: 10x10 0.05° grid, cell_id = 1:100, top row = "land" (NA)
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 0.5,
                   ymin = 0, ymax = 0.5, crs = "EPSG:4326")
  terra::values(r) <- 1:100
  r[1, ] <- NA
  tif <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tif, overwrite = TRUE)
  ocean_ids <- terra::values(r)[!is.na(terra::values(r))]

  # polygon covering the left half (x in [0, 0.25], all y)
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(0.25, 0), c(0.25, 0.5), c(0, 0.5), c(0, 0)))), crs = 4326))

  b <- cells_from_ranges(poly, tif)                 # binary presence = 100
  expect_true(all(b$value == 100))
  expect_true(all(b$cell_id %in% ocean_ids))        # land row excluded (ocean-masked)
  expect_true(nrow(b) > 0 && nrow(b) < length(ocean_ids))

  cv <- cells_from_ranges(poly, tif, cover = TRUE)  # coverage-weighted
  expect_true(max(cv$value) <= 100 && min(cv$value) > 0)
  expect_true(all(cv$cell_id %in% ocean_ids))
})
