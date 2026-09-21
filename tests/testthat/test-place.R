# cells_in_polygon_grid() — the ONE rule for "which cells does this polygon cover".
#
# The fixtures in inst/fixtures/places/ are the cross-language contract: each holds
# a polygon, the grid spec it was computed on and the expected (cell_id, pct). The
# TypeScript twin in the atlas app reads the SAME files, so a rounding or
# antimeridian rule that drifts on one side fails on both.

fx_all <- place_fixture_ids()

test_that("every place fixture reproduces exactly, cell for cell", {
  expect_gt(length(fx_all), 0)
  for (id in fx_all) {
    fx <- place_fixture(id)
    # a `normalize-*` fixture carries a WRAPPED ring on purpose: its `expected` is
    # the coverage AFTER the unwrap rule, which runs at the input boundary
    g  <- if (is.null(fx$unwrapped)) fx$geometry else unwrap_polygon(fx$geometry)
    d  <- cells_in_polygon_grid(g, fx$grid)
    expect_identical(d$cell_id, fx$expected$cell_id,
                     info = paste(id, "-", fx$rule))
    expect_identical(as.numeric(d$pct_covered), fx$expected$pct,
                     info = paste(id, "-", fx$rule))
  }
})

test_that("a fixture's stored grid spec is the registry's, not a copy that drifted", {
  for (id in fx_all) {
    fx <- place_fixture(id)
    g  <- grid_spec_for(fx$grid[["grid_id"]])
    expect_equal(as.integer(fx$grid[["nc"]]),  as.integer(g$nc),  info = id)
    expect_equal(as.integer(fx$grid[["nr"]]),  as.integer(g$nr),  info = id)
    expect_equal(as.numeric(fx$grid[["xmin"]]), as.numeric(g$xmin), info = id)
    expect_equal(as.numeric(fx$grid[["ymax"]]), as.numeric(g$ymax), info = id)
    expect_equal(as.numeric(fx$grid[["resx"]]), as.numeric(g$resx), info = id)
    expect_identical(isTRUE(fx$grid[["lon360"]]), isTRUE(g$lon360), info = id)
  }
})

test_that("REGRESSION: percent rounds half-to-EVEN in BOTH directions, not half-up", {
  # JavaScript's Math.round is half-UP. A twin using it reports 13 for a cell
  # covered 12.5 %, which is one cell's worth of weight wrong in every edge cell
  # of every drawn place — invisible in aggregate and wrong in every report.
  # One direction is not enough: half-up AGREES with half-even on 3.5 -> 4 and
  # disagrees on 2.5 -> 2, so both are pinned.
  d <- cells_in_polygon_grid(place_fixture("half_cell_even")$geometry, "global05")
  expect_identical(as.numeric(d$pct_covered), c(12, 38))          # 12.5 DOWN, 37.5 UP
  u <- cells_in_polygon_grid(place_fixture("half_cell_even_up")$geometry, "global05")
  expect_identical(as.numeric(u$pct_covered), 4)                  # 3.5 UP, to even

  # the same values under half-up, spelled out so the difference is the assertion
  half_up <- function(x) floor(x + 0.5)
  expect_identical(half_up(c(12.5, 37.5, 3.5, 0.5)), c(13, 38, 4, 1))
  expect_identical(round(c(12.5, 37.5, 3.5, 0.5)),   c(12, 38, 4, 0))
})

test_that("the percent is SNAPPED to 9 decimals before it is rounded", {
  # A drawn rectangle produces exact halves that arrive from polygon clipping as
  # 2.4999999999 or 2.5000000001. Without the snap a knife-edge cell is a float
  # coin toss and R and TypeScript disagree at random, which is worse than either
  # rule because it cannot be reproduced.
  expect_identical(.pct_round(2.4999999999), 2)
  expect_identical(.pct_round(2.5000000001), 2)
  expect_identical(.pct_round(3.4999999999), 4)
  expect_identical(.pct_round(0.4999999999), 0)
  # below the snap the rule is ordinary half-even
  expect_identical(.pct_round(2.500001), 3)
})

test_that("the TypeScript twin's shared fixtures are present and asserted here", {
  # These files are byte-identical on both sides. Losing one silently halves the
  # cross-language contract, so their presence is itself an assertion.
  shared <- c("dateline-global05", "dateline-usa05", "gulf-rect-global05",
              "gulf-rect-usa05", "half-even-2p5-global05", "hole-global05",
              "knife-edge-0p5-global05", "knife-edge-2p5-global05",
              "knife-edge-2p5-usa05", "knife-edge-3p5-global05",
              "multipolygon-global05", "overlap-cap-global05",
              "partial-edge-global05", "sliver-corner-half-global05")
  expect_true(all(shared %in% place_fixture_ids()))
})

test_that("overlapping parts of one place count ONCE, not twice", {
  # a hand-drawn two-stroke place is an INVALID multipolygon; GEOS refuses to
  # intersect it and the error reads like a bug in the cell arithmetic
  fx <- place_fixture("overlap-cap-global05")
  d  <- cells_in_polygon_grid(fx$geometry, fx$grid)
  expect_equal(nrow(d), 1)
  expect_equal(d$pct_covered, 100)     # 1.0 + 0.5 of a cell is still one cell
})

test_that("REGRESSION: a polygon across 180 stays itself, not its complement", {
  # Authored as 179.9 -> -179.9 in [-180,180], which read planar is a 358-degree
  # edge the wrong way round the world. Unwrapped it is a 0.2 x 0.05 box.
  fx <- place_fixture("antimeridian_global05")
  d  <- cells_in_polygon_grid(fx$geometry, fx$grid)

  expect_equal(nrow(d), 4)
  col <- ((d$cell_id - 1L) %% 7200L) + 1L
  expect_setequal(col, c(1L, 2L, 7199L, 7200L))   # both ends of one row band
  expect_true(all(d$pct_covered == 100))
})

test_that("the same polygon on usa05 is contiguous in the 0-360 frame", {
  fx <- place_fixture("antimeridian_usa05")
  d  <- cells_in_polygon_grid(fx$geometry, fx$grid)
  col <- ((d$cell_id - 1L) %% 3103L) + 1L
  expect_equal(diff(sort(col)), c(1L, 1L, 1L))    # no fold: usa05 is a window
  expect_true(all(d$pct_covered == 100))
})

test_that("a windowed grid drops columns outside it instead of folding them", {
  # usa05 runs 141.10 E eastward to 296.25 E. A polygon at 100 E is off the grid;
  # folding a column index instead of dropping it would land it on the US east coast.
  p <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(100, 100.2, 100.2, 100, 100), c(10, 10, 10.2, 10.2, 10)))), crs = 4326)
  expect_equal(nrow(cells_in_polygon_grid(p, "usa05")), 0)
  expect_gt(nrow(cells_in_polygon_grid(p, "global05")), 0)
})

test_that("a hole is removed, not ignored", {
  fx <- place_fixture("polygon_hole")
  d  <- cells_in_polygon_grid(fx$geometry, fx$grid)
  expect_equal(nrow(d), 8)                        # 3x3 block minus the centre
  expect_true(all(d$pct_covered == 100))
})

test_that("a sliver whose percent rounds to 0 is dropped", {
  fx <- place_fixture("sliver_corner")
  d  <- cells_in_polygon_grid(fx$geometry, fx$grid)
  expect_equal(nrow(d), 1)                        # 3 of the 4 touched cells drop
  expect_equal(d$pct_covered, 2)
})

test_that("cell ids round-trip through the grid's own lon/lat arithmetic", {
  fx <- place_fixture("gulf_rectangle")
  g  <- grid_spec_for("global05")
  ll <- cell_lonlat(fx$expected$cell_id, g)
  expect_true(all(ll$lon > -90.0  & ll$lon < -89.85))
  expect_true(all(ll$lat >  26.99 & ll$lat <  27.10))
  expect_identical(
    as.integer(cell_from_lonlat(ll$lon, ll$lat, g)), fx$expected$cell_id)
})

test_that("an unknown or malformed grid is an error, never a default", {
  p <- sf::st_sfc(sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0)))),
                  crs = 4326)
  expect_error(cells_in_polygon_grid(p, "nope05"), "unknown grid_id")
  expect_error(cells_in_polygon_grid(p, list(nc = 7200, nr = 3600)), "missing")
})

# grid_for_con / cells_in_study_area -------------------------------------------

test_that("grid_for_con prefers the stored cell_grid over the lon/lat guess", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "cell", data.frame(cell_id = 1L, lon = 0, lat = 0))
  expect_identical(grid_for_con(con)$grid_id, "global05")   # lon/lat present

  cell_grid_write(con, "usa05")
  expect_identical(grid_for_con(con)$grid_id, "usa05")      # the record wins
})

test_that("cells_in_study_area drops cells outside the release, and land", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "cell", data.frame(
    cell_id = 1:4, in_usa = c(TRUE, FALSE, NA, TRUE)))
  cells <- tibble::tibble(cell_id = c(1L, 2L, 3L, 4L, 99L), pct_covered = c(100, 100, 50, 25, 100))

  d <- cells_in_study_area(con, cells)

  # 2 is foreign, 99 is not in the release; 3 is NA -> coalesce(., TRUE) keeps it
  expect_identical(d$cell_id, c(1L, 3L, 4L))
  expect_identical(d$pct_covered, c(100, 50, 25))          # weights preserved
})

test_that("a release with no in_usa column is its own study area", {
  # v1-v7 `cell` has no in_usa; treating its absence as FALSE would empty every place
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "cell", data.frame(cell_id = 1:3, area_km2 = 1))
  d <- cells_in_study_area(con, tibble::tibble(cell_id = 1:3, pct_covered = 100))
  expect_equal(nrow(d), 3)
})


# The D8 addendum: coverage is literal, unwrapping is one explicit rule ---------

test_that("REGRESSION: a WRAPPED ring read literally is the complement, not the box", {
  # The ruling (master plan D8 addendum, 2026-09-21, from a measured R-vs-TypeScript
  # disagreement): both coverage twins read coordinates literally and NEITHER guesses
  # at the antimeridian. `179.9 -> -179.9` is a 359.8-degree edge the long way round,
  # so it means the complement — 7,196 cells, which is exactly what the TypeScript
  # twin returns. The old R heuristic quietly answered 4 and the two languages
  # disagreed by three orders of magnitude on the same file.
  r <- cbind(c(179.9, -179.9, -179.9, 179.9, 179.9), c(50, 50, 50.05, 50.05, 50))
  p <- sf::st_sfc(sf::st_polygon(list(r)), crs = 4326)

  expect_equal(nrow(cells_in_polygon_grid(p, "global05")), 7196)

  # ...and through the rule it is the 4-cell box again
  u <- cells_in_polygon_grid(unwrap_polygon(p), "global05")
  expect_equal(nrow(u), 4)
  expect_setequal(((u$cell_id - 1L) %% 7200L) + 1L, c(1L, 2L, 7199L, 7200L))
  expect_true(all(u$pct_covered == 100))
})

test_that("unwrap_ring carries -/+360 onward and never moves the first vertex", {
  ring <- function(lon) unname(cbind(lon, seq_along(lon)))
  expect_equal(unwrap_ring(ring(c(179.9, -179.9, -179.9, 179.9)))[, 1],
               c(179.9, 180.1, 180.1, 179.9))
  # crossing twice: the carry must come BACK to zero, not accumulate
  expect_equal(unwrap_ring(ring(c(178, -178, 178, -178)))[, 1],
               c(178, 182, 178, 182))
  # already unwrapped, and near zero: unchanged. The rule fires on a 180-degree
  # step, not on a sign change.
  expect_equal(unwrap_ring(ring(c(179.9, 180.1, 180.1)))[, 1], c(179.9, 180.1, 180.1))
  expect_equal(unwrap_ring(ring(c(-0.1, 0.1, 0.1)))[, 1], c(-0.1, 0.1, 0.1))
  # the first vertex keeps whatever frame it arrived in
  expect_equal(unwrap_ring(ring(c(350, 10)))[, 1], c(350, 370))
  expect_equal(unwrap_ring(ring(c(-10, 350)))[, 1], c(-10, -10))
  # a single vertex, and an empty ring, are not special cases to crash on
  expect_equal(unwrap_ring(ring(179.9))[, 1], 179.9)
})

test_that("every normalize-* fixture unwraps to its stored ring, vertex for vertex", {
  norm <- grep("^normalize-", fx_all, value = TRUE)
  expect_gte(length(norm), 6)
  for (id in norm) {
    fx <- place_fixture(id)
    expect_false(is.null(fx$unwrapped), info = id)
    got  <- sf::st_coordinates(unwrap_polygon(fx$geometry))[, c("X", "Y")]
    want <- sf::st_coordinates(fx$unwrapped)[, c("X", "Y")]
    expect_equal(unname(got), unname(want), info = paste(id, "-", fx$rule))
    # and the fixture records what reading it literally would have given, so the
    # difference the rule makes is in the file rather than in a commit message
    lit <- nrow(cells_in_polygon_grid(fx$geometry, fx$grid))
    expect_equal(lit, fx$cells_if_read_literally, info = id)
  }
})

test_that("unwrap_polygon treats every ring independently", {
  # a hole is a closed ring of its own; a multipolygon part on each side of the line
  # must stay on its own side rather than being dragged across by its neighbour
  fx <- place_fixture("normalize-hole-outer-crosses-global05")
  u  <- sf::st_coordinates(unwrap_polygon(fx$geometry))
  expect_true(all(u[u[, "L2"] == 1, "X"] >= 179.8))    # outer ring unwrapped
  expect_true(all(u[u[, "L2"] == 2, "X"] >= 179.8))    # hole untouched, still 179.9x

  mp <- place_fixture("normalize-multipolygon-split-global05")
  v  <- sf::st_coordinates(unwrap_polygon(mp$geometry))
  expect_true(any(v[, "X"] > 179) && any(v[, "X"] < -179))
  expect_equal(nrow(cells_in_polygon_grid(unwrap_polygon(mp$geometry), mp$grid)), 2)
})

test_that("cells_in_polygon unwraps at the sf boundary, so a drawn place still works", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  want <- cells_in_polygon_grid(
    sf::st_sfc(sf::st_polygon(list(cbind(
      c(179.9, 180.1, 180.1, 179.9, 179.9), c(50, 50, 50.05, 50.05, 50)))), crs = 4326),
    "global05")
  DBI::dbWriteTable(con, "cell", data.frame(
    cell_id = want$cell_id, lon = 0, lat = 0))

  # the SAME place, drawn and therefore delivered WRAPPED
  wrapped <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(179.9, -179.9, -179.9, 179.9, 179.9), c(50, 50, 50.05, 50.05, 50)))), crs = 4326)
  d <- cells_in_polygon(wrapped, con)
  expect_setequal(d$cell_id, want$cell_id)
  expect_true(all(d$pct_covered == 100))
})
