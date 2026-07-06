# tests for the hex interpolation machinery in R/interp.R

test_that("cell_i_grid gives stable, distinct indices per 0.5deg cell", {
  # two points in the same 0.5deg cell -> same index; adjacent cell -> +1 in col
  expect_equal(cell_i_grid(-179.9, 89.9), cell_i_grid(-179.6, 89.6))  # same top-left cell
  expect_equal(cell_i_grid(-179.4, 89.9) - cell_i_grid(-179.9, 89.9), 1L)  # next col east
  expect_type(cell_i_grid(0, 0), "integer")
  # a point and the same point map identically (determinism)
  expect_equal(cell_i_grid(-120.2, 34.7), cell_i_grid(-120.2, 34.7))
})

test_that("hex_grid_weights + model_hex_from_weights interpolate a shared-grid model", {
  skip_if_not_installed("terra"); skip_if_not_installed("FNN")
  ok <- tryCatch({ con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown=TRUE))
    DBI::dbExecute(con, "INSTALL h3 FROM community; LOAD h3;"); TRUE }, error = function(e) FALSE)
  skip_if_not(ok, "DuckDB h3 unavailable")

  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # a small ocean hex grid over a 1x1 deg box
  r <- terra::rast(xmin = -118, xmax = -117, ymin = 32.5, ymax = 33.5,
                   resolution = 0.05, crs = "EPSG:4326", nlyrs = 1)
  names(r) <- "depth_mean"; terra::values(r) <- 1
  hex_ocean(r, con, res = 7, mask_layer = "depth_mean", out_tbl = "hex")

  # source grid = a coarse 0.5deg set of points spanning the box, value linear in lon
  g  <- expand.grid(lon = seq(-118.25, -116.75, 0.5), lat = seq(32.25, 33.75, 0.5))
  src <- data.frame(src_i = cell_i_grid(g$lon, g$lat), lon = g$lon, lat = g$lat)
  hex_grid_weights(con, "hex", src, k = 4L, power = 2, out_tbl = "w")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(DISTINCT hex_id) n FROM w")$n,
               DBI::dbGetQuery(con, "SELECT count(*) n FROM hex")$n)          # every hex weighted
  expect_true(DBI::dbGetQuery(con, "SELECT count(*) n FROM w_sum")$n > 0)

  mv <- data.frame(src_i = src$src_i, value = g$lon)                          # field = lon
  mh <- model_hex_from_weights(con, mv, mdl_seq = 42L, threshold = -999)
  expect_setequal(names(mh), c("mdl_seq", "hex_id", "value"))
  expect_true(all(mh$mdl_seq == 42L))
  expect_s3_class(mh$hex_id, "integer64")
  # interpolated value ~ hex-centroid lon (IDW of a linear field)
  ctr <- hex_centroids(mh$hex_id, con)
  expect_gt(cor(mh$value, ctr$lon), 0.99)
})
