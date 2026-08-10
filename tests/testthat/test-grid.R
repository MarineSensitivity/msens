# Two grids, and `cell_id` means a different place on each. Getting this wrong
# does not error — it scatters a model's cells into the wrong ocean — so these
# assertions use REAL cell ids sampled from the live v7 cell-id raster and the
# coordinates they were sampled at.

test_that("the registry knows both grids and their sizes", {
  g <- grid_registry()
  expect_setequal(g$grid_id, c("usa05", "global05"))
  expect_equal(g$ncell[g$grid_id == "usa05"],    6224618)   # 3103 * 2006
  expect_equal(g$ncell[g$grid_id == "global05"], 25920000)  # 7200 * 3600
  expect_true(g$lon360[g$grid_id == "usa05"])               # 0-360 frame
  expect_false(g$lon360[g$grid_id == "global05"])
})

test_that("each release maps to the grid its cell_id actually indexes", {
  for (v in c("v1", "v2", "v3", "v4", "v4b", "v5", "v6", "v7"))
    expect_equal(grid_for_ver(v), "usa05", info = v)
  expect_equal(grid_for_ver("v8"), "global05")
  # never guess: an unregistered version must fail rather than inherit the newest grid
  expect_error(grid_for_ver("v99"), "no grid registered")
})

test_that("grid_spec_for rejects an unknown grid and carries lon360", {
  expect_error(grid_spec_for("h3r7"), "unknown grid_id")
  expect_true(grid_spec_for("usa05")$lon360)
  expect_equal(grid_spec_for("usa05")$nc, 3103L)
})

test_that("cell_lonlat inverts real v7 cell ids to where they were sampled", {
  # measured against /share/data/derived/r_cellid.tif on the server:
  #   (-90.5, 27.5) Gulf of Mexico -> 3418971 ; (-120.5, 33.5) SoCal -> 3046011
  #   (-140.0, 57.0) Gulf of Alaska -> 1587211 ; (179.5, 51.8) Aleutians -> 1909113
  g  <- grid_spec_for("usa05")
  ll <- cell_lonlat(c(3418971, 3046011, 1587211, 1909113), g)
  expect_equal(ll$lon, c(-90.525, -120.525, -140.025, 179.475), tolerance = 1e-6)
  expect_equal(ll$lat, c( 27.525,   33.525,   57.025,  51.825), tolerance = 1e-6)
  # the antimeridian case is the whole reason for the 0-360 frame: it must come
  # back as +179.475, not as some out-of-domain 539.475
  expect_true(all(ll$lon >= -180 & ll$lon < 180))
})

test_that("publish_cog writes a lon360 grid into a valid -180..180 COG", {
  skip_if_not_installed("terra")
  g   <- grid_spec_for("usa05")
  ids <- c(3418971, 1909113)          # Gulf of Mexico + Aleutians: straddles 180
  out <- withr::local_tempfile(fileext = ".tif")
  expect_silent(publish_cog(ids, c(50, 60), out, g, overview = FALSE))

  r <- terra::rast(out)
  e <- as.vector(terra::ext(r))
  # out of domain here is the bug: x > 180 makes web tilers place it nowhere
  expect_gte(e[["xmin"]], -180); expect_lte(e[["xmax"]], 180)

  # each value must land at the coordinate cell_lonlat predicts
  ll <- cell_lonlat(ids, g)
  expect_equal(as.numeric(terra::extract(r, ll)[, 2]), c(50, 60))
})

test_that("publish_cog leaves a -180..180 grid alone", {
  skip_if_not_installed("terra")
  g   <- grid_spec_for("global05")
  # global05 cell 1 is the top-left corner; pick something mid-ocean instead
  ids <- c(1000L * g$nc + 3000L)
  out <- withr::local_tempfile(fileext = ".tif")
  publish_cog(ids, 42, out, g, overview = FALSE)
  ll <- cell_lonlat(ids, g)
  expect_equal(as.numeric(terra::extract(terra::rast(out), ll)[, 2]), 42)
})
