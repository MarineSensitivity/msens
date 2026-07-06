# tests for the H3 hex helpers in R/hex.R
# these require duckdb + the `h3` community extension; skip where it can't load

skip_if_no_h3 <- function() {
  ok <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbExecute(con, "INSTALL h3 FROM community; LOAD h3;")
    TRUE
  }, error = function(e) FALSE)
  testthat::skip_if_not(ok, "DuckDB h3 extension unavailable")
}

test_that("hex_id_from_lonlat matches a known H3 res-7 index", {
  skip_if_no_h3()
  # San Diego coast; verified against DuckDB h3_h3_to_string
  expect_equal(hex_id_from_lonlat(-117.20, 32.70, res = 7), "8729a411cffffff")
})

test_that("hex_id_from_lonlat is vectorized and order-preserving", {
  skip_if_no_h3()
  ids <- hex_id_from_lonlat(c(-117.2, -70.0), c(32.7, 42.0), res = 7)
  expect_length(ids, 2)
  expect_type(ids, "character")
  expect_equal(ids[1], "8729a411cffffff")
})

test_that("hex_centroids round-trips near the input point", {
  skip_if_no_h3()
  lon <- -117.20; lat <- 32.70
  id  <- hex_id_from_lonlat(lon, lat, res = 7)
  ctr <- hex_centroids(id)
  # res-7 hexes are ~2.5 km edge, so the centroid is within ~0.05 deg
  expect_lt(abs(ctr$lon - lon), 0.05)
  expect_lt(abs(ctr$lat - lat), 0.05)
})

test_that("hex_grid_from_raster explodes coarse pixels into child hexes", {
  skip_if_no_h3()
  skip_if_not_installed("terra")
  # a small 2-layer raster over the SoCal bight, 0.05 deg (400 pixels)
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 2)
  names(r) <- c("depth_mean", "sst_an_mean")
  terra::values(r) <- cbind(seq_len(terra::ncell(r)),           # depth_mean
                            seq_len(terra::ncell(r)) * 0.1)       # sst_an_mean

  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  h <- hex_grid_from_raster(r, con, res = 7, mask_layer = "depth_mean")

  expect_type(h$hex_id, "character")
  expect_true(all(c("depth_mean", "sst_an_mean", "area_km2") %in% names(h)))
  expect_true(all(nchar(h$hex_id) == 15))            # H3 res-7 string length
  expect_true(all(h$area_km2 > 4 & h$area_km2 < 7))  # res-7 ~5.16 km^2
  expect_equal(anyDuplicated(h$hex_id), 0L)          # one row per hex
  # res-7 hexes are smaller than 0.05 deg pixels, so there are MORE hexes than
  # the 400 source pixels (explode, not aggregate)
  expect_gt(nrow(h), terra::ncell(r))
  # each hex inherits one pixel's values, so the sst = 0.1*depth relation holds
  expect_equal(h$sst_an_mean, h$depth_mean * 0.1, tolerance = 1e-6)
})
