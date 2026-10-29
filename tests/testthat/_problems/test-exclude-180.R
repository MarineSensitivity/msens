# Extracted from test-exclude.R:180

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
con <- exclude_fixture_con()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
w <- data.frame(mdl_seq = c(10L, 20L), zone_key = c("A", "B"), w = c(0.25, 0.5))
d <- DBI::dbGetQuery(con, zone_weight_sql(w, ch_src = "ch_cells"))
