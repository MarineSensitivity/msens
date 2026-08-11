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

test_that("cell_lonlat(wrap = FALSE) keeps a 0-360 grid contiguous", {
  g <- grid_spec_for("usa05")           # 141.10 E eastward across the antimeridian
  # a column east of the antimeridian: wrapped it reads negative, unwrapped it
  # stays > 180 so an extent spanning the dateline does not blow up to the globe
  far <- 3000                            # near the east edge of the usa05 frame
  w   <- cell_lonlat(far, g, wrap = TRUE)$lon
  u   <- cell_lonlat(far, g, wrap = FALSE)$lon
  expect_lt(w, 0)
  expect_gt(u, 180)
  expect_equal(u - 360, w)
  # a bbox over cells either side of the dateline stays narrow when unwrapped
  cells <- c(700, 3000)                 # west and east of 180
  lw <- cell_lonlat(cells, g, wrap = TRUE)$lon
  lu <- cell_lonlat(cells, g, wrap = FALSE)$lon
  # 176.08 E and 291.08 E: unwrapped they are 115 deg apart, wrapped the eastern
  # one becomes -68.93 and the pair reads as 245 deg -- a bbox twice the truth
  expect_equal(round(diff(range(lu)), 1), 115.0)
  expect_equal(round(diff(range(lw)), 1), 245.0)
  expect_gt(diff(range(lw)), diff(range(lu)))
})

test_that("wrap has no effect on a -180..180 grid", {
  g <- grid_spec_for("global05")
  expect_equal(cell_lonlat(12345, g, wrap = TRUE), cell_lonlat(12345, g, wrap = FALSE))
})

test_that("cell_from_lonlat round-trips with cell_lonlat on both grids", {
  for (gid in c("usa05", "global05")) {
    g <- grid_spec_for(gid)
    ids <- c(1, 5000, 123456, g$nc + 7)
    ll  <- cell_lonlat(ids, g)                    # wrapped to -180..180
    back <- cell_from_lonlat(ll$lon, ll$lat, g)
    expect_equal(back, as.numeric(ids), info = gid)
  }
})

test_that("a point outside the grid is NA, not a wrapped-around cell", {
  g <- grid_spec_for("usa05")                     # 141.10 E eastward, 82.6 N down
  expect_true(is.na(cell_from_lonlat(0, 0, g)))   # mid-Atlantic: outside usa05
  expect_true(is.na(cell_from_lonlat(-140, 89, g)))  # north of the grid
})

test_that("a -180..180 click resolves on a 0-360 grid", {
  g <- grid_spec_for("usa05")
  # -138.5 E is 221.5 in the grid's own frame, well inside it
  expect_false(is.na(cell_from_lonlat(-138.5, 55, g)))
})
