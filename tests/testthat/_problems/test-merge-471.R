# Extracted from test-merge.R:471

# prequel ----------------------------------------------------------------------
skip_if_not_installed("duckdb")
SUIT <- c("am", "ax")
merge_fixture_con <- function(supersede = TRUE) {
  con <- DBI::dbConnect(duckdb::duckdb())
  mc <- data.frame(
    ms_merge_key = c(
      "T_range","T_range","T_range",
      "T_both_mask","T_both_mask", "T_both_mask","T_both_mask","T_both_mask","T_both_mask",
      "T_noeez","T_noeez", "T_noeez","T_noeez",
      "T_am_single","T_am_single","T_am_single",
      "T_am_multi","T_am_multi","T_am_multi",
      "T_ax_both","T_ax_both","T_ax_both","T_ax_both", "T_ax_both","T_ax_both","T_ax_both","T_ax_both","T_ax_both", "T_ax_both","T_ax_both","T_ax_both",
      "T_ax_only","T_ax_only","T_ax_only","T_ax_only", "T_ax_only","T_ax_only",
      "T_ax_new","T_ax_new"),
    ds_key = c(
      "rng","rng","rng",
      "rng","rng", "am","am","am","am",
      "rng","rng", "am","am",
      "am","am","am",
      "am","am","am",
      "rng","rng","rng","rng", "am","am","am","am","am", "ax","ax","ax",
      "am","am","am","am", "ax","ax",
      "ax","ax"),
    cell_id = c(
      1L,2L,100L,
      1L,2L, 1L,2L,3L,101L,
      100L,101L, 1L,2L,
      1L,2L,3L,
      1L,1L,2L,
      1L,2L,4L,100L, 1L,2L,3L,4L,100L, 1L,2L,3L,
      1L,2L,4L,100L, 1L,3L,
      1L,2L),
    val = c(
      1,1,1,
      1,1, 60,40,90,95,
      1,1, 70,80,
      55,65,75,
      50,60,70,
      1,1,1,1, 60,40,90,70,95, 80,20,50,
      55,65,75,10, 30,20,
      40,50),
    stringsAsFactors = FALSE)
  taxon <- data.frame(
    ms_merge_key = c("T_range","T_both_mask","T_noeez","T_am_single","T_am_multi",
                     "T_ax_both","T_ax_only","T_ax_new"),
    er_score = c(50, 30, 20, 0, 0, 30, 0, 0), stringsAsFactors = FALSE)
  taxon_flags <- data.frame(
    ms_merge_key = c("T_range","T_both_mask","T_noeez","T_am_single","T_am_multi",
                     "T_ax_both","T_ax_only","T_ax_new"),
    has_suit  = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    has_range = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)
  us_cells  <- data.frame(cell_id = 1:5)
  ax_mask   <- data.frame(cell_id = 1:3)                       # where AquaX was modeled
  supersede_tbl <- data.frame(ms_merge_key = c("T_ax_both", "T_ax_only", "T_ax_new"))
  DBI::dbWriteTable(con, "mc", mc)
  DBI::dbWriteTable(con, "taxon", taxon)
  DBI::dbWriteTable(con, "taxon_flags", taxon_flags)
  DBI::dbWriteTable(con, "us_cells", us_cells)
  DBI::dbWriteTable(con, "ax_mask", ax_mask)
  DBI::dbWriteTable(con, "supersede", supersede_tbl)
  DBI::dbExecute(con, paste("CREATE TABLE b AS SELECT * FROM mc",
                            if (supersede) paste("WHERE", supersede_sql(src = "mc")) else ""))
  msq <- merge_sql(SUIT)
  DBI::dbExecute(con, msq$b_range)
  DBI::dbExecute(con, msq$b_am_rng)
  con
}
key_set <- function(df, k) {
  d <- df[df$mdl_key == k, ]
  d <- d[order(d$cell_id, d$val), ]
  paste0(d$cell_id, ":", d$val)
}
turtle_v7b_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  ts  <- data.frame(
    ms_merge_key = "T_turtle",
    ds_key  = c(rep("turtles", 6),        rep("am", 5),        rep("ch", 3)),
    cell_id = c(1L, 2L, 3L, 4L, 5L, 7L,   1L, 2L, 4L, 5L, 7L,  3L, 6L, 7L),
    val     = c(100, 100, 100, 50, 50, 100, 80, 49, 51, 53, 10, 100, 100, 50),
    stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "turtle_src", ts)
  con
}

# test -------------------------------------------------------------------------
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
mc <- data.frame(
    ms_merge_key = c(rep("T_shelf", 4), rep("T_ocean", 4), rep("T_nodens", 2)),
    ds_key  = c("am", "am", "gm", "gm",   "am", "am", "gm", "gm",   "am", "am"),
    cell_id = c(1L, 2L, 1L, 3L,          1L, 2L, 2L, 3L,           1L, 2L),
    val     = c(50, 40, 90, 10,          50, 40, 70, 30,           50, 40), stringsAsFactors = FALSE)
DBI::dbWriteTable(con, "mc", mc)
DBI::dbWriteTable(con, "dens_dom", data.frame(ms_merge_key = c("T_shelf", "T_shelf", "T_ocean", "T_ocean"),
                                                cell_id = c(1L, 3L, 2L, 3L)))
DBI::dbWriteTable(con, "dens_taxa", data.frame(ms_merge_key = c("T_shelf", "T_ocean")))
kept <- DBI::dbGetQuery(con, paste("SELECT * FROM mc WHERE", supersede_sql("am", "dens_taxa", "dens_dom", "mc", by_taxon = TRUE)))
expect_equal(key_set(kept[kept$ds_key == "am", ], "T_shelf"), "2:40")
