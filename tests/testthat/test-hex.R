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

# H3 ids exceed 2^53, so compare via bit64/string, never as double
test_that("hex_id_from_lonlat returns a BIGINT (integer64) matching a known id", {
  skip_if_no_h3()
  id <- hex_id_from_lonlat(-117.20, 32.70, res = 7)
  expect_s3_class(id, "integer64")
  # 0x8729a411cffffff = 608718504098529279
  expect_equal(as.character(id), "608718504098529279")
  # and its hex-string form
  expect_equal(hex_id_to_string(id), "8729a411cffffff")
})

test_that("hex_id_from_lonlat is vectorized and order-preserving", {
  skip_if_no_h3()
  ids <- hex_id_from_lonlat(c(-117.2, -70.0), c(32.7, 42.0), res = 7)
  expect_length(ids, 2)
  expect_s3_class(ids, "integer64")
  expect_equal(hex_id_to_string(ids)[1], "8729a411cffffff")
})

test_that("hex_centroids round-trips near the input point", {
  skip_if_no_h3()
  lon <- -117.20; lat <- 32.70
  id  <- hex_id_from_lonlat(lon, lat, res = 7)
  ctr <- hex_centroids(id)
  expect_s3_class(ctr$hex_id, "integer64")
  expect_equal(as.character(ctr$hex_id), as.character(id))
  # res-7 hexes are ~2.5 km edge, so the centroid is within ~0.05 deg
  expect_lt(abs(ctr$lon - lon), 0.05)
  expect_lt(abs(ctr$lat - lat), 0.05)
})

test_that("hex_grid_from_raster explodes coarse pixels into BIGINT child hexes", {
  skip_if_no_h3()
  skip_if_not_installed("terra")
  # a small 2-layer raster over the SoCal bight, 0.05 deg (400 pixels)
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 2)
  names(r) <- c("depth_mean", "sst_an_mean")
  terra::values(r) <- cbind(seq_len(terra::ncell(r)),           # depth_mean
                            seq_len(terra::ncell(r)) * 0.1)       # sst_an_mean

  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  h <- hex_grid_from_raster(r, con, res = 7, mask_layer = "depth_mean")

  expect_s3_class(h$hex_id, "integer64")               # BIGINT, not string/double
  expect_true(all(c("depth_mean", "sst_an_mean", "area_km2") %in% names(h)))
  expect_true(all(h$area_km2 > 4 & h$area_km2 < 7))    # res-7 ~5.16 km^2
  expect_equal(anyDuplicated(as.character(h$hex_id)), 0L)  # one row per hex
  # res-7 hexes are smaller than 0.05 deg pixels -> more hexes than 400 pixels
  expect_gt(nrow(h), terra::ncell(r))
  # each hex inherits one pixel's values, so the sst = 0.1*depth relation holds
  expect_equal(h$sst_an_mean, h$depth_mean * 0.1, tolerance = 1e-6)
})

test_that("hex_add_membership flags in-polygon hexes via polyfill (BIGINT semijoin)", {
  skip_if_no_h3()
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  # build a tiny hex grid over a 1x1 deg box, then a polygon covering its west half
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 1)
  names(r) <- "depth_mean"; terra::values(r) <- 1
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  hex_grid_from_raster(r, con, res = 7, mask_layer = "depth_mean", out_tbl = "hex")

  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(-118, 32.5), c(-117.5, 32.5), c(-117.5, 33.5), c(-118, 33.5), c(-118, 32.5)))),
    crs = 4326))
  gpkg <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(poly, gpkg, quiet = TRUE)

  hex_add_membership(con, "hex", gpkg, "in_west", res = 7)
  n <- DBI::dbGetQuery(con, "SELECT
    count(*) n, count(*) FILTER (WHERE in_west) n_in FROM hex")
  expect_gt(n$n_in, 0)            # some hexes flagged
  expect_lt(n$n_in, n$n)          # but not all (only the west half)
  # hex_id stored as BIGINT
  expect_equal(DBI::dbGetQuery(con, "SELECT typeof(hex_id) t FROM hex LIMIT 1")$t, "BIGINT")
})
