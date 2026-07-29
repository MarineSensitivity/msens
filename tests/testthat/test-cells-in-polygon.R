# cells_in_polygon() — the drawn-area entry point for the /report endpoint and
# the scores app's Report tab. Grid correctness here is invisible downstream:
# a wrong cell_id is still a VALID cell_id, so a mismatch yields an empty
# report rather than an error.

# a tiny v8-shaped `cell` table: 0.05-degree centres on the global [-180,180]
# grid, cell_id = row * 7200 + col + 1 (row-major from the top-left)
v8_cell_db <- function(lon_min, lon_max, lat_min, lat_max) {
  cols <- seq(floor((lon_min + 180) / 0.05), floor((lon_max + 180) / 0.05))
  rows <- seq(floor((90 - lat_max) / 0.05), floor((90 - lat_min) / 0.05))
  g <- expand.grid(col = cols, row = rows)
  d <- data.frame(
    cell_id = as.integer(g$row * 7200 + g$col + 1),
    lon     = -180 + g$col * 0.05 + 0.025,
    lat     =   90 - g$row * 0.05 - 0.025)
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "cell", d)
  con
}

sq <- function(x0, x1, y0, y1) sf::st_sf(geometry = sf::st_sfc(
  sf::st_polygon(list(cbind(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0)))),
  crs = 4326))

test_that("cells_in_polygon uses the v8 lon/lat grid when given a connection", {
  con <- v8_cell_db(-121.2, -118.8, 33.8, 35.7); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # a polygon snapped to cell edges: exactly 2.0 x 0.1 degrees = 40 x 2 cells
  d <- cells_in_polygon(sq(-121, -119, 34, 34.1), con)

  expect_equal(nrow(d), 80)
  expect_true(all(d$pct_covered == 100))
  # every returned id must decode back to a lon/lat inside the polygon
  lon <- -180 + ((d$cell_id - 1L) %% 7200L) * 0.05 + 0.025
  lat <-   90 - ((d$cell_id - 1L) %/% 7200L) * 0.05 - 0.025
  expect_true(all(lon > -121 & lon < -119))
  expect_true(all(lat >   34 & lat <  34.1))
})

test_that("cells_in_polygon reports partial coverage on edge cells", {
  con <- v8_cell_db(-120.3, -119.7, 34.7, 35.3); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # half a cell wide, one cell tall, aligned to the left half of one column
  d <- cells_in_polygon(sq(-120.05, -120.025, 34.95, 35.0), con)

  expect_equal(nrow(d), 1)
  expect_equal(d$pct_covered, 50)   # pct_covered weights area_km2/avg_suit
})

test_that("cells_in_polygon returns nothing for a polygon off the grid", {
  con <- v8_cell_db(-121, -119, 34, 35); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- cells_in_polygon(sq(10, 11, 10, 11), con)
  expect_equal(nrow(d), 0)
  expect_named(d, c("cell_id", "pct_covered"))
})

test_that("REGRESSION: a v8 connection never yields v7 0-360 raster ids", {
  # The bug: cell_id_raster() is the v7 raster — regional, 0-360 longitudes,
  # holding v7 cell_ids. Used against v8 it returned ids 2,924,984-3,015,011 for
  # a polygon off Santa Barbara; id 2,928,088 is lon 64.375 / lat 69.675 in v8's
  # `cell` — the ARCTIC. Those ids exist, so nothing errored: the drawn-area
  # report just came back with zero species.
  con <- v8_cell_db(-121.2, -118.8, 33.8, 35.7); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  d <- cells_in_polygon(sq(-121, -119, 34, 35.5), con)

  expect_gt(nrow(d), 0)
  # the v7 ids that the raster path produced for this very polygon
  expect_false(any(d$cell_id %in% c(2924984L, 2928087L, 2928088L, 3015011L)))
  # ids must land in the row band the polygon actually occupies
  row <- (d$cell_id - 1L) %/% 7200L
  expect_true(all(row >= floor((90 - 35.5) / 0.05) & row <= floor((90 - 34) / 0.05)))
})

test_that("the raster path reads only the polygon window and keeps coverage", {
  # v7 grid shape: 0-360 longitudes, pixel values ARE the cell ids
  r <- terra::rast(
    xmin = 230, xmax = 250, ymin = 30, ymax = 40, resolution = 1, crs = "EPSG:4326")
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_id"

  # 2x2-degree square on cell boundaries, given in [-180,180] as callers do
  # (-122 -> 238); the function shifts it to 0-360 internally
  d <- .cells_in_polygon_raster(sq(-122, -120, 34, 36), r)

  expect_equal(nrow(d), 4)
  expect_true(all(d$pct_covered == 100))
  # ids must match what the raster itself reports for those centres
  expect_setequal(
    d$cell_id,
    terra::extract(r, cbind(c(238.5, 239.5, 238.5, 239.5),
                            c(34.5, 34.5, 35.5, 35.5)))[[1]])
})

test_that("the raster path reports partial coverage and drops non-overlaps", {
  r <- terra::rast(
    xmin = 230, xmax = 250, ymin = 30, ymax = 40, resolution = 1, crs = "EPSG:4326")
  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_id"

  # half of one cell (238..239 x 34..35) -> 50%
  d <- .cells_in_polygon_raster(sq(-122, -121.5, 34, 35), r)
  expect_equal(nrow(d), 1)
  expect_equal(d$pct_covered, 50)

  # entirely off the raster -> empty, correctly shaped
  d0 <- .cells_in_polygon_raster(sq(-10, -9, 0, 1), r)
  expect_equal(nrow(d0), 0)
  expect_named(d0, c("cell_id", "pct_covered"))
})

test_that(".lon_ranges splits an antimeridian-crossing span", {
  expect_equal(.lon_ranges(-121, -119), list(c(-121, -119)))
  expect_equal(.lon_ranges(170, 190),   list(c(170, 180), c(-180, -170)))
  expect_equal(.lon_ranges(-190, -170), list(c(170, 180), c(-180, -170)))
  expect_equal(.lon_ranges(-180, 180),  list(c(-180, 180)))
})

test_that("cells_in_polygon falls back to the raster when `cell` has no lon/lat", {
  # v7's `cell` table has no lon/lat, so a connection must NOT take the SQL path
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "cell", data.frame(cell_id = 1:3, area_km2 = 1))
  expect_false(.cell_has_lonlat(con))

  # prove the RASTER leg is the one taken (not the SQL leg silently returning
  # nothing), by stubbing cell_id_raster() with a sentinel
  testthat::local_mocked_bindings(
    cell_id_raster = function() stop("raster-path-taken"))
  expect_error(cells_in_polygon(sq(-121, -119, 34, 35), con), "raster-path-taken")
})
