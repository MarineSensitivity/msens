# Guards the occurrence (vagrant) rule (occurrence_counts_sql / occurrence_rule / occurrence_exclusions),
# v11 Q2: one rule for every species, the two v7c anchors falling out of it, the overrides as data.
#
# Toy grid: nc = 10 columns, block = 2 cells -> 0.5-degree-like blocks of 2 x 2 cells; cell ids
# 1..100 (10 rows). Zones (majority): EBS = rows 1-2 (cells 1-20), GOA = rows 3-4 (21-40),
# GULF = rows 5-6 (41-60); rows 7-10 (61-100) are "elsewhere" (no zone row). Blocks: 25 of them,
# five per 2-row band.
#
# Species:
#   turtle   present in every cell 1-100 (a wide suitability surface). Records: none in EBS (rows 1-2),
#            one block in GOA (cells 21-22), and all 10 blocks of rows 7-10 (the stronghold; cells 99-100
#            carry none but share their block with 89-90).
#            Effort: every block searched (a megafauna record somewhere in each) EXCEPT the GULF band
#            (rows 5-6), where nobody looked.
#            -> EBS dropped (searched, 0 of 5 against 90 % elsewhere); GOA kept (1 of 5 = 20 %, not
#               below 1/4 of ~69 %... see the arithmetic in the test); GULF unsearched.
#   narrow   present in cells 1-6 only (6 cells, < min_cells): 0 records, searched -> never dropped.
#   ch_sp    like turtle but with critical habitat at cell 3 (EBS) -> EBS kept, override "protected".
#   buffered like turtle, but its only record near EBS sits in cell 21 (row 3, GOA): with region_rec
#            dilated to include row 3 for EBS, the record counts for EBS and EBS is no longer "0 of".
#   lonely   present in GULF only (the unsearched band) with one record of its own at cell 41: that
#            block is searched by definition, so the verdict is kept, not unsearched.

skip_if_not_installed("duckdb")

NC <- 10L; BLOCK <- 2L
cells_in_rows <- function(rows) unlist(lapply(rows, function(r) ((r - 1) * NC + 1):(r * NC)))

occ_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  region <- rbind(
    data.frame(zone_key = "EBS",  cell_id = cells_in_rows(1:2)),
    data.frame(zone_key = "GOA",  cell_id = cells_in_rows(3:4)),
    data.frame(zone_key = "GULF", cell_id = cells_in_rows(5:6)))
  strong_cells <- setdiff(cells_in_rows(7:10), c(99, 100))   # every stronghold block holds a record (99-100 share block 24 with 89-90)
  present <- rbind(
    data.frame(mdl_key = "lonely",   cell_id = cells_in_rows(5:6)),   # GULF only, where nobody looked ...
    data.frame(mdl_key = "turtle",   cell_id = 1:100),
    data.frame(mdl_key = "narrow",   cell_id = 1:6),
    data.frame(mdl_key = "ch_sp",    cell_id = 1:100),
    data.frame(mdl_key = "buffered", cell_id = 1:100))
  occ <- rbind(
    data.frame(mdl_key = "lonely",   cell_id = 41L, records = 1L),      # ... except for one record of its own
    data.frame(mdl_key = "turtle",   cell_id = c(21, 22, strong_cells), records = 1L),
    data.frame(mdl_key = "ch_sp",    cell_id = c(21, 22, strong_cells), records = 1L),
    data.frame(mdl_key = "buffered", cell_id = c(21, strong_cells), records = 1L))
  # effort: one megafauna record per block in every band but GULF (rows 5-6)
  searched_cells <- c(cells_in_rows(1:4), cells_in_rows(7:10))
  effort <- rbind(
    data.frame(mdl_key = "turtle",   cell_id = searched_cells),
    data.frame(mdl_key = "narrow",   cell_id = searched_cells),
    data.frame(mdl_key = "ch_sp",    cell_id = searched_cells),
    data.frame(mdl_key = "buffered", cell_id = searched_cells))
  ch <- data.frame(mdl_key = "ch_sp", cell_id = 3L)
  for (nm in c("region", "present", "occ", "effort", "ch")) DBI::dbWriteTable(con, paste0("occ_", nm), get(nm))
  DBI::dbExecute(con, "CREATE TABLE occ_present_v AS SELECT * FROM occ_present")
  con
}
run <- function(con, ...) occurrence_rule(con, present = "occ_present", occ = "occ_occ", effort = "occ_effort",
                                          region = "occ_region", nc = NC, block = BLOCK, min_cells = 20L, ...)
row_of <- function(d, key, zone) d[d$mdl_key == key & d$zone_key == zone, ]

test_that("counts: blocks, searched-by-record, records and totals are what the toy says", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- DBI::dbGetQuery(con, occurrence_counts_sql("occ_present", "occ_occ", "occ_effort", "occ_region", nc = NC, block = BLOCK))
  t <- d[d$mdl_key == "turtle", ]
  expect_equal(nrow(t), 3)                                   # EBS, GOA, GULF (elsewhere has no zone)
  expect_equal(row_of(t, "turtle", "EBS")$n_cells, 20)
  expect_equal(row_of(t, "turtle", "EBS")$n_searched, 5); expect_equal(row_of(t, "turtle", "EBS")$n_occupied, 0)
  expect_equal(row_of(t, "turtle", "GOA")$n_searched, 5); expect_equal(row_of(t, "turtle", "GOA")$n_occupied, 1)
  expect_equal(row_of(t, "turtle", "GULF")$n_searched, 0)   # nobody looked
  expect_equal(row_of(t, "turtle", "EBS")$n_records, 0); expect_equal(row_of(t, "turtle", "GOA")$n_records, 1)
  # all searched units: 5 (EBS) + 5 (GOA) + 10 (rows 7-10) = 20; occupied: 1 + 10 = 11
  expect_equal(unique(t$n_searched_all), 20); expect_equal(unique(t$n_occupied_all), 11)
})

test_that("the anchor: a zone searched with zero records against a strong rate elsewhere is dropped; unsearched is kept", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- run(con)
  ebs <- row_of(d, "turtle", "EBS"); goa <- row_of(d, "turtle", "GOA"); gulf <- row_of(d, "turtle", "GULF")
  # EBS: p_core = (11 - 0) / (20 - 5) = 11/15; P(0 of 5 | 11/15) = (4/15)^5 = 0.0014 < 0.05; rate 0 < 1/4 p_core -> dropped
  expect_equal(ebs$p_core, 11/15); expect_equal(ebs$verdict, "dropped"); expect_true(is.na(ebs$label)); expect_true(is.na(ebs$override))
  # GOA: p_core = (11 - 1) / (20 - 5) = 2/3; 1 of 5 = 0.2 > 0.25 * 2/3 = 0.167 -> the material guard keeps it
  expect_equal(goa$p_core, 2/3); expect_equal(goa$rate, 0.2); expect_equal(goa$verdict, "kept")
  # GULF: nobody looked -> unsearched, kept and flagged, never dropped
  expect_equal(gulf$verdict, "unsearched")
  ex <- occurrence_exclusions(d)
  expect_equal(ex[ex$mdl_key == "turtle", "zone_key"], "EBS")
  # the exclusion rows are what zone_exclude_sql() takes
  expect_no_error(zone_exclude_sql(ex, id_col = "mdl_key", src = "occ_present", cell_zone = "cz"))
})

test_that("a dropped zone with a record of the species is labelled 'documented occurrence only'", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # one vagrant record in EBS (cell 1): 1 of 5 = 0.2 is NOT below 1/4 of 11/15 = 0.183 at the default
  # ratio (kept: the guard), so the point is made at ratio = 0.5: 0.2 < 0.367 and
  # P(<= 1 of 5 | 11/15) = 0.020 < 0.05 -> dropped, with the record on file
  DBI::dbExecute(con, "INSERT INTO occ_occ VALUES ('turtle', 1, 1)")
  d <- run(con, ratio = 0.5)
  ebs <- row_of(d, "turtle", "EBS")
  expect_equal(ebs$n_records, 1); expect_equal(ebs$verdict, "dropped"); expect_equal(ebs$label, "documented occurrence only")
})

test_that("overrides: a narrow-range species is never dropped; protected cells keep a zone", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- run(con, ch = "occ_ch")
  n <- row_of(d, "narrow", "EBS")
  # narrow: 6 present cells in 3 searched blocks, 0 records, and NO stronghold elsewhere (p_core NA):
  # the test has nothing to compare with -> "unsearched" (kept and flagged), never "dropped"
  expect_equal(n$verdict, "unsearched"); expect_true(is.na(n$p_core))
  # force the test's own verdict to "dropped" by giving it a stronghold outside EBS, still < 20 cells
  DBI::dbExecute(con, "INSERT INTO occ_present SELECT 'narrow', cell_id FROM (SELECT unnest([61,62,63,64,65,66,67,68,69,70]) AS cell_id)")
  DBI::dbExecute(con, "INSERT INTO occ_occ SELECT 'narrow', cell_id, 1 FROM (SELECT unnest([61,62,63,64,65,66,67,68,69,70]) AS cell_id)")
  d2 <- run(con, ch = "occ_ch"); n2 <- row_of(d2, "narrow", "EBS")
  expect_equal(n2$verdict_raw, "dropped"); expect_equal(n2$override, "narrow range"); expect_equal(n2$verdict, "kept")
  c1 <- row_of(d, "ch_sp", "EBS")
  expect_equal(c1$verdict_raw, "dropped"); expect_equal(c1$override, "protected"); expect_equal(c1$verdict, "kept")
  # ... and the override is per zone: ch_sp has no protected cell in GOA, which the test keeps anyway
  expect_true(is.na(row_of(d, "ch_sp", "GOA")$override))
  expect_equal(nrow(occurrence_exclusions(d)[occurrence_exclusions(d)$mdl_key == "ch_sp", ]), 0)
  # without the ch relation the same zone is dropped: the override is data, not a hard-coded place
  d0 <- run(con)
  expect_equal(row_of(d0, "ch_sp", "EBS")$verdict, "dropped")
})

test_that("the buffer: a record just outside the zone counts for it (region_rec), presence does not move", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # buffered's record near EBS sits in row 3 (cell 21, GOA). Plain regions: EBS 0 of 5 -> dropped
  d0 <- run(con)
  expect_equal(row_of(d0, "buffered", "EBS")$n_records, 0); expect_equal(row_of(d0, "buffered", "EBS")$verdict, "dropped")
  # dilate EBS by one row for records/effort: row 3 joins EBS's buffered footprint
  DBI::dbExecute(con, "CREATE TABLE occ_region_rec AS SELECT * FROM occ_region
    UNION ALL SELECT 'EBS' AS zone_key, cell_id FROM occ_region WHERE zone_key = 'GOA' AND cell_id <= 30")
  d1 <- run(con, region_rec = "occ_region_rec")
  b <- row_of(d1, "buffered", "EBS")
  expect_equal(b$n_records, 1)             # the record now counts for EBS ...
  expect_equal(b$n_cells, 20)              # ... while presence is still the zone proper
})

test_that("a unit with a record of the species is searched by definition, even where no effort is listed", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- run(con)
  l <- row_of(d, "lonely", "GULF")
  expect_equal(l$n_searched, 1); expect_equal(l$n_occupied, 1); expect_equal(l$n_records, 1)
  # with no stronghold elsewhere the test has nothing to compare with: "unsearched" is its honest
  # verdict here; the point of this fixture is the counts above (a dropped record = 0 searched)
  expect_true(is.na(l$p_core)); expect_equal(l$verdict, "unsearched")
  # the turtle in the same band has no record there and no effort: unsearched
  expect_equal(row_of(d, "turtle", "GULF")$verdict, "unsearched")
})

test_that("a species present nowhere in a zone gets no row; bad arguments error", {
  con <- occ_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- run(con)
  expect_equal(nrow(row_of(d, "narrow", "GOA")), 0)
  expect_error(occurrence_counts_sql(nc = 0), "nc")
  expect_error(occurrence_rule(con, ch = ""), "ch")
  expect_error(occurrence_exclusions(data.frame(a = 1)), "mdl_key")
})
