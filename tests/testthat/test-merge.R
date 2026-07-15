# Guards the v8 merge rules (merge_sql / turtle_sql) against silent breakage. Each fixture taxon
# exercises one rule; the asserts encode the EXACT expected merged cells. If a future tweak changes
# a rule (wrong join, dropped condition, added GROUP BY, US-vs-global scope slip), a test fails.
#
# US cells = {1,2,3,4,5}; non-US = {100,101}. Categories:
#   T_range      range-only, range in+out of US            -> er over range∩US
#   T_both_mask  am+range, am extends BEYOND range in US    -> am MASKED to range footprint
#   T_noeez      am+range, range wholly OUTSIDE US          -> EXCLUDED from US (Sotalia case)
#   T_am_single  am-only, one model                         -> raw am∩US
#   T_am_multi   am-only, TWO models (dup cell)             -> raw am∩US, duplicates PRESERVED

skip_if_not_installed("duckdb")

merge_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  b <- data.frame(
    ms_merge_key = c(
      "T_range","T_range","T_range",
      "T_both_mask","T_both_mask", "T_both_mask","T_both_mask","T_both_mask",
      "T_noeez","T_noeez", "T_noeez","T_noeez",
      "T_am_single","T_am_single","T_am_single",
      "T_am_multi","T_am_multi","T_am_multi"),
    ds_key = c(
      "rng","rng","rng",
      "rng","rng", "am","am","am",
      "rng","rng", "am","am",
      "am","am","am",
      "am","am","am"),
    cell_id = c(
      1L,2L,100L,
      1L,2L, 1L,2L,3L,
      100L,101L, 1L,2L,
      1L,2L,3L,
      1L,1L,2L),
    val = c(
      1,1,1,
      1,1, 60,40,90,
      1,1, 70,80,
      55,65,75,
      50,60,70),
    stringsAsFactors = FALSE)
  taxon <- data.frame(
    ms_merge_key = c("T_range","T_both_mask","T_noeez","T_am_single","T_am_multi"),
    er_score = c(50, 30, 20, 0, 0), stringsAsFactors = FALSE)
  taxon_flags <- data.frame(
    ms_merge_key = c("T_range","T_both_mask","T_noeez","T_am_single","T_am_multi"),
    has_am   = c(FALSE, TRUE, TRUE, TRUE, TRUE),
    has_range = c(TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors = FALSE)
  us_cells <- data.frame(cell_id = 1:5)
  DBI::dbWriteTable(con, "b", b)
  DBI::dbWriteTable(con, "taxon", taxon)
  DBI::dbWriteTable(con, "taxon_flags", taxon_flags)
  DBI::dbWriteTable(con, "us_cells", us_cells)
  msq <- merge_sql()
  DBI::dbExecute(con, msq$b_range)
  DBI::dbExecute(con, msq$b_am_all)
  DBI::dbExecute(con, msq$b_am_rng)
  con
}

# collapse a taxon's result to "cell:val" strings ordered by numeric cell then val (intuitive)
key_set <- function(df, k) {
  d <- df[df$mdl_key == k, ]
  d <- d[order(d$cell_id, d$val), ]
  paste0(d$cell_id, ":", d$val)
}

test_that("US scoring surface applies every merge rule correctly", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  us <- DBI::dbGetQuery(con, merge_sql()$us)

  # range-only: er over range∩US (cell 100 dropped, not in US)
  expect_equal(key_set(us, "T_range"), c("1:50", "2:50"))

  # both, am beyond range: am MASKED to range footprint -> cell 3 (am-only) EXCLUDED
  expect_equal(key_set(us, "T_both_mask"), c("1:60", "2:40"))

  # no_eez: range wholly outside US -> NO US presence at all (the Sotalia guard)
  expect_equal(nrow(us[us$mdl_key == "T_noeez", ]), 0L)

  # am-only single model: raw am∩US
  expect_equal(key_set(us, "T_am_single"), c("1:55", "2:65", "3:75"))

  # am-only TWO models: raw am∩US, duplicate cell 1 PRESERVED (no dedup) -> 3 rows
  am_multi <- us[us$mdl_key == "T_am_multi", ]
  expect_equal(nrow(am_multi), 3L)
  expect_equal(sort(am_multi$val), c(50, 60, 70))
  expect_equal(sum(am_multi$cell_id == 1L), 2L)   # cell 1 appears twice
})

test_that("no_eez species are excluded from US but PRESENT in the global viz surface", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  us <- DBI::dbGetQuery(con, merge_sql()$us)
  gl <- DBI::dbGetQuery(con, merge_sql()$global)

  # excluded from scoring...
  expect_equal(nrow(us[us$mdl_key == "T_noeez", ]), 0L)
  # ...but its whole-range am∪range IS in the global surface (range outside US + am in US)
  expect_equal(key_set(gl, "T_noeez"), c("1:70", "2:80", "100:20", "101:20"))
})

test_that("global viz surface = am ∪ range for has_range taxa; am-only taxa omitted", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  gl <- DBI::dbGetQuery(con, merge_sql()$global)

  # both: FULL OUTER keeps am BEYOND the range (cell 3) unlike the US surface
  expect_equal(key_set(gl, "T_both_mask"), c("1:60", "2:40", "3:90"))
  # range-only: er over the whole range footprint (incl. non-US cell 100)
  expect_equal(key_set(gl, "T_range"), c("1:50", "2:50", "100:50"))
  # am-only taxa are OMITTED from the global surface (they reuse am COGs)
  expect_equal(nrow(gl[gl$mdl_key %in% c("T_am_single", "T_am_multi"), ]), 0L)
})

test_that("turtle multiplicative rule: greatest(1, round(er*suit/100)) then ch max-override", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ts <- data.frame(
    ms_merge_key = "T_turtle",
    ds_key  = c("turtles", "turtles", "am", "ch"),
    cell_id = c(1L, 2L, 1L, 1L),
    val     = c(80, 60, 50, 90), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "turtle_src", ts)
  res <- DBI::dbGetQuery(con, turtle_sql("turtles", "am", "ch", src = "turtle_src"))
  # cell1: round(80*50/100)=40, then ch 90 overrides -> 90
  # cell2: no suit -> greatest(1, round(60*1/100)=1) = 1, no ch -> 1
  expect_equal(key_set(res, "T_turtle"), c("1:90", "2:1"))
})
