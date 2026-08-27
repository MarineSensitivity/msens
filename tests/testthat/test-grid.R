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
  expect_equal(grid_for_ver("v9"), "global05")   # AquaX: same grid, position-mapped
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

# lon_span: the antimeridian fit rule (apps#9) ------------------------------
#
# One fixture per branch. The regression case is the FIRST one: Least Auklet's
# v8 merged surface really does report min lon -179.975 / max lon 179.975, and
# the app fitted that whole-globe box, putting the camera off Iceland.

test_that("an antimeridian-crossing range spans the short way, not the globe", {
  # Least Auklet (Aethia pusilla), v8 ms_merge|BOTW:22694921: cells at
  # 160-180 E and 180-150 W. -180..180 says 360 deg; the truth is ~50 deg.
  lon <- c(-179.975, -160.0, -150.025, 160.025, 170.0, 179.975)
  expect_equal(lon_span(lon), c(160.025, 209.975))
  expect_lt(diff(lon_span(lon)), 51)
  # and the naive answer, which this replaces, really was the whole globe
  expect_gt(diff(range(lon)), 359)
})

test_that("a range that does not cross the antimeridian is left in -180..180", {
  # North Atlantic, straddling the prime meridian: 0..360 would be the wide one
  expect_equal(lon_span(c(-30, -5, 0, 20)), c(-30, 20))
  # entirely western hemisphere (Gulf of Mexico) — no wrap, no shift
  expect_equal(lon_span(c(-97.5, -90, -81.2)), c(-97.5, -81.2))
  # entirely eastern hemisphere
  expect_equal(lon_span(c(12, 90, 140)), c(12, 140))
})

test_that("a truly circumglobal range keeps the -180..180 frame", {
  # straddles BOTH cut points, so neither frame is narrow: answer unchanged
  lon <- seq(-180, 180, by = 10)
  expect_equal(lon_span(lon), c(-180, 180))
})

test_that("lon_span returns xmax > 180 so fitBounds crosses the dateline", {
  s <- lon_span(c(170, -170))
  expect_gt(s[2], 180)          # NOT wrapped back: west must stay west of east
  expect_lt(s[1], s[2])
})

test_that("lon_span handles degenerate input", {
  expect_equal(lon_span(numeric(0)), c(NA_real_, NA_real_))
  expect_equal(lon_span(c(NA, NaN)),  c(NA_real_, NA_real_))
  expect_equal(lon_span(c(-120, NA)), c(-120, -120))   # a single point is a point
})

test_that("lon_span_agg is the same rule as lon_span", {
  for (lon in list(c(-179.9, 179.9), c(-30, 20), c(12, 140), seq(-180, 180, 20))) {
    l360 <- ifelse(lon < 0, lon + 360, lon)
    expect_equal(
      lon_span_agg(min(lon), max(lon), min(l360), max(l360)),
      lon_span(lon))
  }
})

test_that("lon_span_agg degrades to the -180..180 frame when the 0-360 pair is missing", {
  expect_equal(lon_span_agg(-30, 20, NA, NA), c(-30, 20))
  expect_equal(lon_span_agg(NA, NA, 10, 20),  c(NA_real_, NA_real_))
})

test_that("a whole-world bbox is rejected as a camera target", {
  # what native_asset stores for a wraparound COG: correct for the ASSET,
  # useless for the CAMERA
  expect_true (bbox_spans_globe(c(-180, 38.55, 180, 66.45)))
  expect_true (bbox_spans_globe(NULL))
  expect_true (bbox_spans_globe(c(-180, NA, 180, 66)))
  expect_false(bbox_spans_globe(c(160.025, 47.975, 209.975, 66.175)))  # the fixed span
  expect_false(bbox_spans_globe(c(-97.5, 18, -81.2, 31)))
})

test_that("cell_lonlat(wrap = FALSE) already satisfies the lon_span rule on usa05", {
  # usa05's own frame runs 141.10 E east across the dateline, so an unwrapped
  # Aleutian extent needs no further correction — lon_span is a no-op on it.
  g  <- grid_spec_for("usa05")
  ll <- cell_lonlat(c(700, 3000), g, wrap = FALSE)$lon
  expect_equal(lon_span(ifelse(ll >= 180, ll - 360, ll)), range(ll))
})
