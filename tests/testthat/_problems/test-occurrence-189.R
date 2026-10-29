# Extracted from test-occurrence.R:189

# prequel ----------------------------------------------------------------------
skip_if_not_installed("duckdb")
NC <- 10L
BLOCK <- 2L
cells_in_rows <- function(rows) unlist(lapply(rows, function(r) ((r - 1) * NC + 1):(r * NC)))
occ_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  region <- rbind(
    data.frame(zone_key = "EBS",  cell_id = cells_in_rows(1:2)),
    data.frame(zone_key = "GOA",  cell_id = cells_in_rows(3:4)),
    data.frame(zone_key = "GULF", cell_id = cells_in_rows(5:6)),
    data.frame(zone_key = "ATL",  cell_id = cells_in_rows(7:10)))   # the stronghold IS a zone (as NECS is)
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

# test -------------------------------------------------------------------------
con <- occ_fixture_con()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
DBI::dbExecute(con, "DELETE FROM occ_region WHERE zone_key = 'ATL'")
DBI::dbExecute(con, "INSERT INTO occ_present SELECT 'whale', cell_id FROM (SELECT unnest(range(1, 101)) AS cell_id)")
DBI::dbExecute(con, "INSERT INTO occ_effort  SELECT 'whale', cell_id FROM (SELECT unnest(range(1, 101)) AS cell_id)")
DBI::dbExecute(con, "INSERT INTO occ_occ SELECT 'whale', cell_id, 1 FROM (SELECT unnest([21,23,25,27, 61]) AS cell_id)")
d_range <- occurrence_rule(con, present = "occ_present", occ = "occ_occ", effort = "occ_effort", region = "occ_region",
                             nc = NC, block = BLOCK, baseline = "range")
d_zones <- occurrence_rule(con, present = "occ_present", occ = "occ_occ", effort = "occ_effort", region = "occ_region",
                             nc = NC, block = BLOCK, baseline = "zones")
ebs_r <- row_of(d_range, "whale", "EBS")
ebs_z <- row_of(d_zones, "whale", "EBS")
expect_equal(ebs_r$p_core, 0.25)
expect_equal(ebs_r$verdict, "kept")
expect_equal(ebs_z$p_core, 0.4)
expect_equal(ebs_z$n_searched_zones, 15)
expect_equal(ebs_z$n_occupied_zones, 4)
DBI::dbExecute(con, "INSERT INTO occ_occ VALUES ('whale', 29, 1)")
d_range <- occurrence_rule(con, present = "occ_present", occ = "occ_occ", effort = "occ_effort", region = "occ_region",
                             nc = NC, block = BLOCK, baseline = "range")
d_zones <- occurrence_rule(con, present = "occ_present", occ = "occ_occ", effort = "occ_effort", region = "occ_region",
                             nc = NC, block = BLOCK, baseline = "zones")
expect_equal(row_of(d_range, "whale", "EBS")$verdict, "kept")
