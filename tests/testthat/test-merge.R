# Guards the v8 merge rules (merge_sql / turtle_sql) against silent breakage. Each fixture taxon
# exercises one rule; the asserts encode the EXACT expected merged cells. If a future tweak changes
# a rule (wrong join, dropped condition, added GROUP BY, US-vs-global scope slip), a test fails.
#
# US cells = {1,2,3,4,5}; non-US = {100,101}. Categories:
#   T_range      range-only, range in+out of US            -> er over range∩US
#   T_both_mask  am+range, am beyond range INSIDE and       -> am MASKED to the range footprint on
#                OUTSIDE the US                                BOTH surfaces (apps#8)
#   T_noeez      am+range, range wholly OUTSIDE US          -> EXCLUDED from US (Sotalia case)
#   T_am_single  am-only, one model                         -> raw am∩US
#   T_am_multi   am-only, TWO models (dup cell)             -> raw am∩US, duplicates PRESERVED
#
# v9 adds AquaX (`ax`), a second suitability dataset delivered only inside a MASK (ax_mask =
# {1,2,3} here), which SUPERSEDES am per taxon inside that mask (supersede_sql on the merge
# input) and nowhere else:
#   T_ax_both    range + am + ax: inside the mask suit = ax (am dropped, even where ax is lower);
#                outside it (cell 4, non-US 100) suit = am
#   T_ax_only    no range, am + ax: raw ax∩US inside the mask (am dropped there, INCLUDING cell 2
#                where ax has no value -- AquaX's absence is an absence), raw am∩US outside (cell 4)
#   T_ax_new     ax only (a taxon v8 never had): raw ax∩US
# The ORIGINAL five fixtures are untouched and asserted against merge_sql(c("am","ax")) -- the
# regression guard that generalizing suit_ds changes nothing for the ~6,000 taxa that keep AquaMaps.
#
# T_both_mask carries am at cell 3 (US) and cell 101 (non-US), both OUTSIDE its range {1,2}. Those
# two cells are the regression guard for apps#8: when the global surface was a FULL OUTER union of
# the range with the whole am footprint, they leaked into it -- and no assertion here looked.

skip_if_not_installed("duckdb")

SUIT <- c("am", "ax")   # v9 suitability datasets; every test below runs the generalized rule

# supersede = TRUE applies supersede_sql() to the raw input (the v9 path); FALSE is the control
# run (AX_SUPERSEDE=0): ax registered, nothing superseded, am and ax simply max() at a cell.
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

# collapse a taxon's result to "cell:val" strings ordered by numeric cell then val (intuitive)
key_set <- function(df, k) {
  d <- df[df$mdl_key == k, ]
  d <- d[order(d$cell_id, d$val), ]
  paste0(d$cell_id, ":", d$val)
}

test_that("US scoring surface applies every merge rule correctly", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  us <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)

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
  us <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)
  gl <- DBI::dbGetQuery(con, merge_sql(SUIT)$global)

  # excluded from scoring...
  expect_equal(nrow(us[us$mdl_key == "T_noeez", ]), 0L)
  # ...but its range IS drawn globally, at er. Its am cells 1,2 sit in the US, OUTSIDE the range,
  # and are masked away here too -- the over-prediction the US exclusion exists to reject must not
  # reappear on the map just because the map is global.
  expect_equal(key_set(gl, "T_noeez"), c("100:20", "101:20"))
})

test_that("global viz surface masks am to the range footprint; am-only taxa omitted", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  gl <- DBI::dbGetQuery(con, merge_sql(SUIT)$global)

  # both: am BEYOND the range is masked -- cell 3 (US) and cell 101 (non-US) both dropped
  expect_equal(key_set(gl, "T_both_mask"), c("1:60", "2:40"))
  # range-only: er over the whole range footprint (incl. non-US cell 100)
  expect_equal(key_set(gl, "T_range"), c("1:50", "2:50", "100:50"))
  # am-only taxa are OMITTED from the global surface (they reuse am COGs)
  expect_equal(nrow(gl[gl$mdl_key %in% c("T_am_single", "T_am_multi"), ]), 0L)
})

test_that("NO global cell falls outside the taxon's range footprint (apps#8 regression)", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # the invariant, asserted over every fixture taxon at once rather than per expected-value list:
  # whatever the rule, the global surface may never introduce a (taxon, cell) the range does not
  # have. A FULL OUTER against the whole am footprint returns 4 rows here.
  leaked <- DBI::dbGetQuery(con, paste(
    "SELECT g.mdl_key, g.cell_id FROM (", merge_sql(SUIT)$global, ") g",
    "LEFT JOIN b_range br ON br.ms_merge_key = g.mdl_key AND br.cell_id = g.cell_id",
    "WHERE br.cell_id IS NULL"))
  expect_equal(nrow(leaked), 0L)

  # and the global surface is the SAME rule as the US one, differing only by the US trim: for a
  # taxon with a range, global ∩ us_cells must equal the US surface exactly.
  gl <- DBI::dbGetQuery(con, merge_sql(SUIT)$global)
  us <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)
  ks <- c("T_range", "T_both_mask", "T_noeez", "T_ax_both")
  gl_in_us <- gl[gl$cell_id %in% 1:5, ]
  expect_equal(
    sapply(ks, function(k) key_set(gl_in_us, k), simplify = FALSE),
    sapply(ks, function(k) key_set(us,       k), simplify = FALSE))
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

test_that("v9 supersession: ax replaces am per taxon INSIDE the AquaX mask only, both surfaces", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  us <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)
  gl <- DBI::dbGetQuery(con, merge_sql(SUIT)$global)

  # T_ax_both (range {1,2,4,100}, er 30): inside the mask suit = ax -> cell 1 max(30,80)=80,
  # cell 2 max(30, ax 20)=30 (am's 40 is GONE even though it is higher); cell 3 (ax beyond the
  # range) masked; outside the mask am survives -> cell 4 max(30,70)=70, cell 100 max(30,95)=95
  expect_equal(key_set(us, "T_ax_both"), c("1:80", "2:30", "4:70"))
  expect_equal(key_set(gl, "T_ax_both"), c("1:80", "2:30", "4:70", "100:95"))

  # T_ax_only (no range): raw suit ∩ US -> ax at 1 and 3; am at 2 is dropped (in the mask, AquaX
  # absent there = absent); am at 4 survives (outside the mask); non-US 100 trimmed
  expect_equal(key_set(us, "T_ax_only"), c("1:30", "3:20", "4:75"))
  expect_equal(nrow(gl[gl$mdl_key == "T_ax_only", ]), 0L)     # suit-only taxa are not drawn globally

  # T_ax_new (never in v8): raw ax ∩ US
  expect_equal(key_set(us, "T_ax_new"), c("1:40", "2:50"))

  # the AquaMaps-only fixtures are byte-identical under the generalized rule
  expect_equal(key_set(us, "T_both_mask"), c("1:60", "2:40"))
  expect_equal(key_set(us, "T_am_single"), c("1:55", "2:65", "3:75"))
  expect_equal(nrow(us[us$mdl_key == "T_am_multi", ]), 3L)
})

test_that("control run (no supersession): am and ax max() at a cell -- the difference IS the rule", {
  con <- merge_fixture_con(supersede = FALSE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  us <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)
  # cell 2: max(er 30, am 40, ax 20) = 40 -- the am value the supersession removes
  expect_equal(key_set(us, "T_ax_both"), c("1:80", "2:40", "4:70"))
  # T_ax_only keeps am at 1 and 2 as well (duplicates preserved, raw suit-only branch)
  expect_equal(key_set(us, "T_ax_only"), c("1:30", "1:55", "2:65", "3:20", "4:75"))
})

test_that("supersede_sql only ever removes (superseded ds, superseded taxon, masked cell) rows", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  gone <- DBI::dbGetQuery(con, "SELECT * FROM mc EXCEPT SELECT * FROM b")
  expect_true(all(gone$ds_key == "am"))
  expect_true(all(gone$ms_merge_key %in% c("T_ax_both", "T_ax_only")))
  expect_true(all(gone$cell_id %in% 1:3))
  # exactly: T_ax_both am at 1,2,3 and T_ax_only am at 1,2 -> 5 rows
  expect_equal(nrow(gone), 5L)
  # a taxon NOT in `supersede` keeps am inside the mask (T_am_single at cells 1..3 untouched)
  kept <- DBI::dbGetQuery(con, "SELECT count(*) n FROM b WHERE ms_merge_key = 'T_am_single'")$n
  expect_equal(kept, 3L)
})

test_that("turtle rule accepts several suitability datasets (ax supersedes am inside the mask)", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mc <- data.frame(
    ms_merge_key = "T_turtle",
    ds_key  = c("turtles", "turtles", "am", "am", "ax", "ch"),
    cell_id = c(1L, 2L, 1L, 2L, 1L, 2L),
    val     = c(80, 60, 50, 40, 20, 90), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "mc", mc)
  DBI::dbWriteTable(con, "ax_mask", data.frame(cell_id = 1L))
  DBI::dbWriteTable(con, "supersede", data.frame(ms_merge_key = "T_turtle"))
  DBI::dbExecute(con, paste("CREATE TABLE turtle_src AS SELECT * FROM mc WHERE", supersede_sql(src = "mc")))
  res <- DBI::dbGetQuery(con, turtle_sql("turtles", SUIT, "ch", src = "turtle_src"))
  # cell 1: am 50 dropped (masked + superseded) -> suit = ax 20 -> round(80*20/100) = 16, no ch
  # cell 2: outside the mask -> suit = am 40 -> round(60*40/100) = 24, then ch 90 overrides
  expect_equal(key_set(res, "T_turtle"), c("1:16", "2:90"))
})

test_that("spatial-ER rule accepts several ER datasets (turtles + NMFS DPS species): ER varies by cell", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # a humpback-like taxon: IUCN-range baseline 21 (LC + MMPA) everywhere, an Endangered DPS polygon
  # at cell 1 (100) and a Threatened one at cell 2 (50); suitability 80 / 80 / 40
  src <- data.frame(
    ms_merge_key = "T_dps",
    ds_key  = c("dps_nmfs", "dps_nmfs", "dps_nmfs", "ax", "ax", "ax"),
    cell_id = c(1L, 2L, 3L, 1L, 2L, 3L),
    val     = c(100, 50, 21, 80, 80, 40), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "turtle_src", src)
  res <- DBI::dbGetQuery(con, turtle_sql(c("turtles", "dps_nmfs"), c("am", "ax"), character(0), src = "turtle_src"))
  # cell 1: 100*80/100 = 80; cell 2: 50*80/100 = 40; cell 3: round(21*40/100) = 8 -- NOT a flat 100
  expect_equal(key_set(res, "T_dps"), c("1:80", "2:40", "3:8"))
})
