# Guards the coverage rule (coverage_sql / coverage_floor) -- how much of a zone a component
# metric actually covers, and whether that clears the reporting floor kappa. The rule decides
# whether a component is PUBLISHED for a Program Area, so every branch gets an exact assertion:
# a dropped `WHERE tbl =`, a dropped metric filter, a lost `IS NOT NULL`, a GROUP BY that forgets
# metric_seq, or a pct_covered that stops weighting all fail here.
#
# Fixture, in the v1-v7 scoring schema (zone / zone_cell / cell_metric / metric / cell):
#
#   zone 1  "PA-A"  100 cells, all fully covered, areas 10 (cells 1-5) and 5 (cells 6-100)
#           comp_a: cells 1-4 valued + cell 5 present-but-NULL -> 4 of 100 -> 0.04  (NOT reportable)
#           comp_b: cells 1-5 valued                           -> 5 of 100 -> 0.05  (reportable, >=)
#   zone 2  "PA-B"  3 cells: 201, 202 fully covered; 203 HALF covered (pct 50)
#           comp_a: cell 203 only -> 50/250 = 0.20 by pct_covered (1/3 if the half were ignored)
#           comp_b: cell 201 only -> 100/250 = 0.40
#   zone 3  "PA-C"  2 cells, neither carrying comp_a or comp_b -> NO ROW AT ALL (absent, not 0)
#   zone 9  an ecoregion zone over zone 1's very cells, in a DIFFERENT zone.tbl -> never returned
#   metric 12 "other_metric" is scored but never requested -> never returned

skip_if_not_installed("duckdb")

# `val_col` builds the same fixture under either spelling of the measurement column, since
# coverage_sql() takes the name rather than assuming one (v1-v7 say `value`, v8 says `val`).
coverage_fixture_con <- function(val_col = "value", null_cell_5 = TRUE) {
  con <- DBI::dbConnect(duckdb::duckdb())

  zone <- data.frame(
    zone_seq = c(1L, 2L, 3L, 9L),
    tbl      = c(rep("ply_programareas_2026_v7", 3), "ply_ecoregions_2025"),
    fld      = c(rep("programarea_key", 3), "ecoregion_key"),
    value    = c("PA-A", "PA-B", "PA-C", "ECO-1"))

  zone_cell <- rbind(
    data.frame(zone_seq = 1L, cell_id =     1:100, pct_covered = 100L),
    data.frame(zone_seq = 2L, cell_id = 201:203,   pct_covered = c(100L, 100L, 50L)),
    data.frame(zone_seq = 3L, cell_id = 301:302,   pct_covered = 100L),
    data.frame(zone_seq = 9L, cell_id =     1:100, pct_covered = 100L))

  # unequal areas so `area` and `pct_covered` cannot accidentally agree
  cell <- data.frame(
    cell_id  = c(1:100, 201:203, 301:302),
    area_km2 = c(rep(10, 5), rep(5, 95), 8, 8, 12, 7, 7))

  metric <- data.frame(
    metric_seq = c(10L, 11L, 12L),
    metric_key = c("comp_a", "comp_b", "other_metric"))

  cell_metric <- rbind(
    data.frame(cell_id = 1:4,       metric_seq = 10L, v = 50),
    data.frame(cell_id = 5L,        metric_seq = 10L, v = if (null_cell_5) NA_real_ else 50),
    data.frame(cell_id = 1:5,       metric_seq = 11L, v = 50),
    data.frame(cell_id = 203L,      metric_seq = 10L, v = 30),
    data.frame(cell_id = 201L,      metric_seq = 11L, v = 30),
    data.frame(cell_id = c(1L, 301L, 302L), metric_seq = 12L, v = 1))
  names(cell_metric)[3] <- val_col

  DBI::dbWriteTable(con, "zone",        zone)
  DBI::dbWriteTable(con, "zone_cell",   zone_cell)
  DBI::dbWriteTable(con, "cell",        cell)
  DBI::dbWriteTable(con, "metric",      metric)
  DBI::dbWriteTable(con, "cell_metric", cell_metric)
  con
}

coverage_of <- function(con, weight = "pct_covered", val_col = "value") {
  DBI::dbGetQuery(con, coverage_sql(
    "ply_programareas_2026_v7", c("comp_a", "comp_b"),
    weight = weight, val_col = val_col))
}

test_that("pct_covered coverage reproduces v7's pct_area, exactly, per zone AND per metric", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- coverage_of(con)

  # the WHOLE result: four rows, ordered -- a second zone and a second metric are present so a
  # wrong join or a GROUP BY missing metric_seq blends them and fails
  expect_equal(d$zone_seq,   c(1L, 1L, 2L, 2L))
  expect_equal(d$metric_key, c("comp_a", "comp_b", "comp_a", "comp_b"))
  expect_equal(d$coverage,   c(0.04, 0.05, 0.20, 0.40))
})

test_that("4 of 100 equal cells is 0.04 and NOT reportable; 5 is 0.05 and IS reportable", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- coverage_floor(coverage_of(con), kappa = 0.05)

  expect_equal(d$coverage[d$zone_seq == 1 & d$metric_key == "comp_a"], 0.04)
  expect_false(d$reportable[d$zone_seq == 1 & d$metric_key == "comp_a"])
  # the boundary is >=, and 5 of 100 equal cells must land ON it
  expect_equal(d$coverage[d$zone_seq == 1 & d$metric_key == "comp_b"], 0.05)
  expect_true(d$reportable[d$zone_seq == 1 & d$metric_key == "comp_b"])
})

test_that("a NULL value does not count as scored", {
  # cell 5 carries a comp_a ROW whose value is NULL -> 4 scored, not 5
  con_null <- coverage_fixture_con(null_cell_5 = TRUE)
  con_val  <- coverage_fixture_con(null_cell_5 = FALSE)
  on.exit({
    DBI::dbDisconnect(con_null, shutdown = TRUE)
    DBI::dbDisconnect(con_val,  shutdown = TRUE)
  })

  a_null <- coverage_of(con_null)
  a_val  <- coverage_of(con_val)
  expect_equal(a_null$coverage[a_null$zone_seq == 1 & a_null$metric_key == "comp_a"], 0.04)
  # filling that one NULL is the ONLY difference, and it moves the pair across the floor
  expect_equal(a_val$coverage[a_val$zone_seq == 1 & a_val$metric_key == "comp_a"], 0.05)
  expect_false(coverage_floor(a_null)$reportable[1])
  expect_true(coverage_floor(a_val)$reportable[1])
})

test_that("a half-covered edge cell weighs half, under both weights", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # zone 2 = cells 201 (100 %), 202 (100 %), 203 (50 %); comp_a is on 203 alone.
  # pct_covered: 50 / 250 = 0.2  -- NOT 1/3, which is what counting cells would give
  d_pct <- coverage_of(con)
  expect_equal(d_pct$coverage[d_pct$zone_seq == 2 & d_pct$metric_key == "comp_a"], 0.2)

  # area: 203 contributes 12 km2 x 50 % = 6 of 8 + 8 + 6 = 22 km2 in-zone area
  d_area <- coverage_of(con, weight = "area")
  expect_equal(d_area$coverage[d_area$zone_seq == 2 & d_area$metric_key == "comp_a"], 6 / 22)
  expect_equal(d_area$coverage[d_area$zone_seq == 2 & d_area$metric_key == "comp_b"], 8 / 22)
})

test_that("weight = 'area' weighs a big cell more than a small one", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- coverage_of(con, weight = "area")

  # zone 1: the 5 scored cells are 10 km2 each, the other 95 are 5 km2
  expect_equal(d$coverage[d$zone_seq == 1 & d$metric_key == "comp_a"], 40 / 525)
  expect_equal(d$coverage[d$zone_seq == 1 & d$metric_key == "comp_b"], 50 / 525)
  # and it genuinely differs from the pct_covered answer (0.04 / 0.05)
  expect_false(isTRUE(all.equal(50 / 525, 0.05)))
})

test_that("a zone with no scored cell returns NO ROW (absent, never 0)", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- coverage_of(con)

  expect_false(3L %in% d$zone_seq)
  expect_equal(nrow(d[d$zone_seq == 3L, ]), 0L)
  # zone 3 DOES carry other_metric, so a dropped metric filter would give it a row
  expect_true(all(d$metric_key %in% c("comp_a", "comp_b")))
})

test_that("zone_tbl selects the zone set and metric_keys selects the metrics", {
  con <- coverage_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # zone 9 covers zone 1's exact cells but belongs to another zone.tbl
  d <- coverage_of(con)
  expect_true(all(d$zone_seq %in% c(1L, 2L)))

  # asking for the ecoregion table instead returns that zone, and only it
  d9 <- DBI::dbGetQuery(con, coverage_sql("ply_ecoregions_2025", c("comp_a", "comp_b")))
  expect_equal(d9$zone_seq, c(9L, 9L))
  expect_equal(d9$coverage, c(0.04, 0.05))

  # one metric requested -> one metric returned
  d1 <- DBI::dbGetQuery(con, coverage_sql("ply_programareas_2026_v7", "comp_b"))
  expect_equal(unique(d1$metric_key), "comp_b")
})

test_that("val_col names the measurement column rather than assuming one", {
  con <- coverage_fixture_con(val_col = "val"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_equal(coverage_of(con, val_col = "val")$coverage, c(0.04, 0.05, 0.20, 0.40))
  expect_equal(sdm_val_col(con, "cell_metric"), "val")
  # the default spelling is wrong for this fixture, and fails loudly rather than silently
  expect_error(coverage_of(con))
})

test_that("coverage_floor is inclusive at the boundary and tolerant of float wobble", {
  d <- data.frame(
    zone_seq   = 1:5,
    metric_key = "m",
    coverage   = c(0.04, 0.05, 0.05 - 1e-12, 0.05 - 1e-6, 0.999))

  f <- coverage_floor(d, kappa = 0.05)
  expect_equal(f$reportable, c(FALSE, TRUE, TRUE, FALSE, TRUE))
  # the tolerance is a half-ulp guard, not a second floor: 1e-6 below kappa still fails
  expect_named(f, c("zone_seq", "metric_key", "coverage", "reportable"))

  # kappa is a parameter, not a constant
  expect_equal(coverage_floor(d, kappa = 0.04)$reportable, c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(coverage_floor(d, kappa = 1.00)$reportable, rep(FALSE, 5))
})

test_that("bad arguments error rather than building nonsense SQL", {
  expect_error(coverage_sql("z", character(0)))
  expect_error(coverage_sql(c("a", "b"), "m"))
  expect_error(coverage_sql("z", "m", weight = "count"))
  expect_error(coverage_floor(data.frame(x = 1)))
  expect_error(coverage_floor(data.frame(coverage = 0.1), kappa = 2))
})
