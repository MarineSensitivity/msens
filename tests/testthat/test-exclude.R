# Guards the v7c taxon x zone exclusion (cell_zone_major_sql / zone_exclude_sql): a taxon is
# dropped from a zone where the evidence table says it does not occur. The rule DELETES model
# cells, so every branch gets an exact assertion: a straddling cell must go with its majority
# zone, a tie must resolve the same way every time, critical habitat must survive, and an
# exclusion must never leak to another model or to a model with no exclusion.
#
# Fixture, in the v1-v7 scoring schema (zone / zone_cell / model_cell):
#
#   zones (fld = ecoregion_key): zone_seq 1 = "B", 2 = "A", 3 = "C" -- seq order is NOT key order,
#   so a tie broken by zone_seq would pick "B" and fail. zone_seq 9 = a Program Area (another fld)
#   covering every cell 100 %, which must never be read.
#
#   cell 1  A 100            -> A
#   cell 2  A 60 / B 40      -> A   (straddler, majority A)
#   cell 3  A 40 / B 60      -> B   (straddler, majority B)
#   cell 4  A 50 / B 50      -> A   (tie: the first key alphabetically)
#   cell 5  B 100            -> B
#   cell 6  C 100            -> C
#   cell 7  no ecoregion     -> no row (never excludable)
#
#   model_cell: mdl_seq 10, 20, 30 each on cells 1-7
#   exclusions: (10, A), (20, B); model 30 has none
#   critical habitat: (10, 1) inside excluded A; (20, 5) inside excluded B; (30, 6) irrelevant

skip_if_not_installed("duckdb")

exclude_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())

  zone <- data.frame(
    zone_seq = c(1L, 2L, 3L, 9L),
    tbl      = c(rep("ply_ecoregions_2025", 3), "ply_programareas_2026_v7"),
    fld      = c(rep("ecoregion_key", 3), "programarea_key"),
    value    = c("B", "A", "C", "PA-X"))

  zone_cell <- rbind(
    data.frame(zone_seq = 2L, cell_id = 1L, pct_covered = 100L),
    data.frame(zone_seq = 2L, cell_id = 2L, pct_covered =  60L),
    data.frame(zone_seq = 1L, cell_id = 2L, pct_covered =  40L),
    data.frame(zone_seq = 2L, cell_id = 3L, pct_covered =  40L),
    data.frame(zone_seq = 1L, cell_id = 3L, pct_covered =  60L),
    data.frame(zone_seq = 1L, cell_id = 4L, pct_covered =  50L),  # "B" first by seq and by row
    data.frame(zone_seq = 2L, cell_id = 4L, pct_covered =  50L),
    data.frame(zone_seq = 1L, cell_id = 5L, pct_covered = 100L),
    data.frame(zone_seq = 3L, cell_id = 6L, pct_covered = 100L),
    data.frame(zone_seq = 9L, cell_id = 1:7, pct_covered = 100L))

  model_cell <- expand.grid(cell_id = 1:7, mdl_seq = c(10L, 20L, 30L))[, c("mdl_seq", "cell_id")]
  model_cell$value <- 50

  ch_cells <- data.frame(mdl_seq = c(10L, 20L, 30L), cell_id = c(1L, 5L, 6L))

  DBI::dbWriteTable(con, "zone",       zone)
  DBI::dbWriteTable(con, "zone_cell",  zone_cell)
  DBI::dbWriteTable(con, "model_cell", model_cell)
  DBI::dbWriteTable(con, "ch_cells",   ch_cells)
  DBI::dbExecute(con, paste("CREATE TEMP TABLE cell_zone AS", cell_zone_major_sql()))
  con
}

exclusions_fx <- data.frame(mdl_seq = c(10L, 20L), zone_key = c("A", "B"))

pairs <- function(d) paste(d$mdl_seq, d$cell_id, sep = ":")

test_that("each cell goes with its majority ecoregion; other fields are ignored", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- DBI::dbGetQuery(con, cell_zone_major_sql())

  # the WHOLE result: one row per zoned cell, cell 7 (no ecoregion) absent, PA-X never chosen
  expect_equal(d$cell_id,  1:6)
  expect_equal(d$zone_key, c("A", "A", "B", "A", "B", "C"))
})

test_that("a 60/40 straddling cell goes with the 60 side and is excluded only with THAT zone", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cz <- DBI::dbGetQuery(con, "SELECT * FROM cell_zone ORDER BY cell_id")
  expect_equal(cz$zone_key[cz$cell_id == 2L], "A")   # A 60 / B 40
  expect_equal(cz$zone_key[cz$cell_id == 3L], "B")   # A 40 / B 60

  d <- DBI::dbGetQuery(con, zone_exclude_sql(exclusions_fx))
  # model 10 excludes A: cell 2 (60 % A) goes, cell 3 (40 % A) stays
  expect_true("10:2"  %in% pairs(d))
  expect_false("10:3" %in% pairs(d))
  # model 20 excludes B: cell 3 (60 % B) goes, cell 2 (40 % B) stays
  expect_true("20:3"  %in% pairs(d))
  expect_false("20:2" %in% pairs(d))
})

test_that("a 50/50 tie resolves to the alphabetically first zone key, not the first zone_seq", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cz <- DBI::dbGetQuery(con, "SELECT * FROM cell_zone WHERE cell_id = 4")

  # "B" is zone_seq 1 and its zone_cell row comes first; the key order must still pick "A"
  expect_equal(cz$zone_key, "A")

  d <- DBI::dbGetQuery(con, zone_exclude_sql(exclusions_fx))
  expect_true("10:4"  %in% pairs(d))   # A excluded for 10 -> the tied cell goes
  expect_false("20:4" %in% pairs(d))   # B excluded for 20 -> the tied cell is not B's
})

test_that("critical habitat inside an excluded zone survives", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  without_ch <- DBI::dbGetQuery(con, zone_exclude_sql(exclusions_fx))
  with_ch    <- DBI::dbGetQuery(con, zone_exclude_sql(exclusions_fx, ch_src = "ch_cells"))

  # without the guard, CH cells (10:1 in A, 20:5 in B) are excluded with their zone ...
  expect_equal(pairs(without_ch), c("10:1", "10:2", "10:4", "20:3", "20:5"))
  # ... with it, exactly those two are spared and nothing else moves
  expect_equal(pairs(with_ch),    c("10:2", "10:4", "20:3"))
})

test_that("an exclusion for one model never touches another model's rows in the same cell", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # only model 10 excluded from A: models 20 and 30 share cells 1, 2, 4 and must not appear
  d <- DBI::dbGetQuery(con, zone_exclude_sql(data.frame(mdl_seq = 10L, zone_key = "A")))
  expect_equal(unique(d$mdl_seq), 10L)
  expect_equal(d$cell_id, c(1L, 2L, 4L))
})

test_that("a model with no exclusion row is untouched", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- DBI::dbGetQuery(con, zone_exclude_sql(exclusions_fx, ch_src = "ch_cells"))
  expect_false(30L %in% d$mdl_seq)
})

test_that("the pair applied end to end leaves exactly the expected rows", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  n_del <- DBI::dbExecute(con, paste0(
    "DELETE FROM model_cell WHERE (mdl_seq, cell_id) IN (",
    zone_exclude_sql(exclusions_fx, ch_src = "ch_cells"), ")"))
  expect_equal(n_del, 3)

  d <- DBI::dbGetQuery(con, "SELECT mdl_seq, cell_id FROM model_cell ORDER BY mdl_seq, cell_id")
  expect_equal(pairs(d), c(
    paste0("10:", c(1, 3, 5, 6, 7)),     # A cells 2, 4 gone; CH cell 1 kept; 7 has no zone
    paste0("20:", c(1, 2, 4, 5, 6, 7)),  # B cell 3 gone; CH cell 5 kept
    paste0("30:", 1:7)))                 # no exclusion: untouched
})

test_that("mdl_key ids work the same way (v8+ schema)", {
  con <- exclude_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE mc_key AS
    SELECT 'ms_merge|WORMS:' || mdl_seq AS mdl_key, cell_id FROM model_cell")

  d <- DBI::dbGetQuery(con, zone_exclude_sql(
    data.frame(mdl_key = "ms_merge|WORMS:10", zone_key = "A"),
    src = "mc_key", id_col = "mdl_key"))
  expect_equal(d$mdl_key, rep("ms_merge|WORMS:10", 3))
  expect_equal(d$cell_id, c(1L, 2L, 4L))
})

test_that("bad arguments error", {
  ok <- exclusions_fx
  expect_error(zone_exclude_sql(ok[0, ]),                                      "at least one row")
  expect_error(zone_exclude_sql(data.frame(mdl_seq = NA_integer_, zone_key = "A")), "no NA")
  expect_error(zone_exclude_sql(data.frame(mdl_seq = 10L, zone_key = NA_character_)), "zone_key")
  expect_error(zone_exclude_sql(data.frame(mdl_seq = 10L)),                    "lacks column")
  expect_error(zone_exclude_sql(data.frame(zone_key = "A")),                   "lacks column")
  expect_error(zone_exclude_sql(data.frame(mdl_seq = 10.5, zone_key = "A")),   "integer-like")
  expect_error(zone_exclude_sql(data.frame(mdl_seq = 10L, zone_key = 1L)),     "zone_key")
  expect_error(zone_exclude_sql(ok, id_col = "mdl_key"),                       "lacks column")
  expect_error(zone_exclude_sql(ok, id_col = "taxon_id"),                      "id_col")
  expect_error(zone_exclude_sql(ok, src = ""),                                 "src")
  expect_error(zone_exclude_sql(ok, cell_zone = NA_character_),                "cell_zone")
  expect_error(zone_exclude_sql(ok, ch_src = ""),                              "ch_src")
  expect_error(zone_exclude_sql(list(mdl_seq = 10L, zone_key = "A")),          "data frame")
  expect_error(cell_zone_major_sql(zone_fld = ""))
})
