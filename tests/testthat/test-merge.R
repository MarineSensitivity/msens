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
  DBI::dbExecute(con, msq$b_fill)
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

test_that("turtle rule accepts several ER datasets (generic): ER varies by cell", {
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

test_that("NMFS DPS rule (dps_sql): merged val is the SUITABILITY masked to the ER footprint; ER rides beside it", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # a humpback-like taxon: ER 100 (EN critical habitat) at cell 1, 50 (TN) at cell 2, 20 (MMPA
  # baseline over the IUCN range) at cells 3-4; AquaX 80/40 at cells 1-2, AquaMaps 30 at cell 2
  # (max with ax -> 40), nothing at cell 3, and AquaMaps 60 at cell 5 which is OUTSIDE the ER
  # footprint and must be masked away like any am cell beyond the range
  src <- data.frame(
    ms_merge_key = "T_dps",
    ds_key  = c("dps_nmfs", "dps_nmfs", "dps_nmfs", "dps_nmfs", "ax", "ax", "am", "am", "am"),
    cell_id = c(1L, 2L, 3L, 4L, 1L, 2L, 2L, 4L, 5L),
    val     = c(100, 50, 20, 20, 80, 40, 30, 5, 60), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "dps_src", src)
  res <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src"))
  res <- res[order(res$cell_id), ]
  # the model is the distribution: suitability where modeled (cells 1, 2, 4 -- NOT max(er, suit):
  # cell 4 keeps AquaMaps' 5 under an ER of 20), the ER as the fitting point where nothing models
  # the cell (cell 3), and nothing beyond the ER footprint (cell 5)
  expect_equal(key_set(res, "T_dps"), c("1:80", "2:40", "3:20", "4:5"))
  expect_equal(res$er, c(100, 50, 20, 20))
  # ...and scoring recovers exactly what the turtle rule would have baked in, one step later
  expect_equal(res$er * res$val / 100, c(80, 20, 4, 1))
  # regression: the turtle rule on the same input is what painted the humpback 1 outside habitat
  DBI::dbExecute(con, "CREATE TABLE turtle_src AS SELECT * FROM dps_src")
  old <- DBI::dbGetQuery(con, turtle_sql("dps_nmfs", SUIT, character(0), src = "turtle_src"))
  # (cell 3: round(20 * coalesce(NULL, 1) / 100) = 0 -> floored to 1; cell 4: round(20 * 5 / 100) = 1)
  expect_equal(key_set(old, "T_dps"), c("1:80", "2:20", "3:1", "4:1"))
})

# v7.1 core habitat: turtle_sql(suit_min, fill, half_even, ch_outer). ONE fixture, four parameter
# sets, so a change to any knob shows as a value change rather than a missing category.
#   ER   {1:100, 2:100, 3:100, 4:50, 5:50, 7:100}   the SWOT/DPS footprint
#   suit {1:80, 2:49, 4:51, 5:53, 7:10}             AquaMaps; cells 3 and 6 have NONE
#   CH   {3:100, 6:100, 7:50}                       3 = CH with no suitability at all,
#                                                   6 = CH OUTSIDE the ER footprint,
#                                                   7 = CH whose suitability is below the cut
# Cells 3 and 7 are the guard for "the trap": with fill = NA they have no `mult` row, so a CH join
# hung off `mult` (v8/v9's LEFT JOIN) drops them -- critical habitat silently trimmed.
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

test_that("v7.1 core habitat: suit_min drops sub-threshold suitability, fill = NA drops the cell", {
  skip_if_not_installed("glue")
  con <- turtle_v7b_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  res <- DBI::dbGetQuery(con, turtle_sql(
    "turtles", "am", "ch", src = "turtle_src",
    suit_min = 50, fill = NA, half_even = TRUE, ch_outer = TRUE))

  # 1 -> 80 (100*80/100), 2 dropped (suit 49 < 50, no fill, no CH), 3 -> 100 (CH, no suitability),
  # 4 -> 26 (25.5 half-even), 5 -> 26 (26.5 half-even), 6 -> 100 (CH outside ER), 7 -> 50 (CH)
  expect_equal(key_set(res, "T_turtle"), c("1:80", "3:100", "4:26", "5:26", "6:100", "7:50"))

  # PERMANENT REGRESSION "critical habitat is never trimmed": cells 3 and 7 carry no retained
  # suitability, so they have no multiplied value -- and must still be present at the
  # designation's own value. Joining CH to `mult` instead of the ER footprint loses both.
  ch <- res[res$cell_id %in% c(3L, 6L, 7L), ]
  expect_equal(nrow(ch), 3L)
  expect_equal(ch$val[order(ch$cell_id)], c(100, 100, 50))

  # PERMANENT REGRESSION "half-even rounding: 26.5 -> 26" (DuckDB round() gives 27; R, and every
  # stored v1-v7 value, gives 26). Cell 4 (25.5 -> 26) agrees either way, so cell 5 is the test.
  expect_equal(res$val[res$cell_id == 5L], 26)

  # the threshold is a filter on suitability, never on the ER footprint: cell 2 is dropped only
  # because fill = NA leaves it nothing, and no CH covers it
  expect_equal(nrow(res[res$cell_id == 2L, ]), 0L)
})

test_that("v7.1 core habitat: ch_outer = FALSE drops CH outside the ER footprint, keeps CH inside", {
  skip_if_not_installed("glue")
  con <- turtle_v7b_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  res <- DBI::dbGetQuery(con, turtle_sql(
    "turtles", "am", "ch", src = "turtle_src",
    suit_min = 50, fill = NA, half_even = TRUE, ch_outer = FALSE))

  # cell 6 (CH, no ER) is gone; cells 3 and 7 (CH inside the footprint) are NOT -- ch_outer is
  # about the footprint union, never about trimming designated habitat
  expect_equal(key_set(res, "T_turtle"), c("1:80", "3:100", "4:26", "5:26", "7:50"))
})

test_that("v7.1 core habitat: defaults reproduce the v8/v9 rule byte-for-byte", {
  skip_if_not_installed("glue")
  con <- turtle_v7b_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  res <- DBI::dbGetQuery(con, turtle_sql("turtles", "am", "ch", src = "turtle_src"))

  # today's values on the same inputs: nothing thresholded (2 -> 49, 7 -> 10 then CH 50), the
  # floor-of-1 fill on cell 3 overridden by CH, DuckDB round() (5 -> 27), no CH union (6 absent)
  expect_equal(key_set(res, "T_turtle"), c("1:80", "2:49", "3:100", "4:26", "5:27", "7:50"))
})

test_that("v7.1 core habitat: suit_min with fill = 1 re-creates the fringe as a field of 1s", {
  skip_if_not_installed("glue")
  con <- turtle_v7b_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  res <- DBI::dbGetQuery(con, turtle_sql(
    "turtles", "am", "ch", src = "turtle_src",
    suit_min = 50, fill = 1, half_even = TRUE, ch_outer = TRUE))

  # cell 2's suitability (49) is thresholded away and comes straight back at the floor: the
  # footprint does NOT shrink, which is why threshold-with-fill raised the Aleutian Arc
  # (70.9 -> 74.5) under the ecoregional rescale. Asserted so the behaviour is explicit.
  expect_equal(res$val[res$cell_id == 2L], 1)
  expect_equal(key_set(res, "T_turtle"),
               c("1:80", "2:1", "3:100", "4:26", "5:26", "6:100", "7:50"))
})

test_that("turtle_sql validates its arguments", {
  skip_if_not_installed("glue")
  expect_error(turtle_sql("turtles", "am", "ch", suit_min = -1))
  expect_error(turtle_sql("turtles", "am", "ch", suit_min = c(0, 50)))
  expect_error(turtle_sql("turtles", "am", "ch", fill = c(1, 2)))
  expect_error(turtle_sql("turtles", "am", "ch", half_even = NA))
  expect_error(turtle_sql("turtles", "am", "ch", ch_outer = "yes"))
  expect_error(turtle_sql(character(0), "am", "ch"))
  expect_error(turtle_sql("turtles", "am", "ch", src = c("a", "b")))
})

# v11 Q3 (plan 2026-10-29 §4.2, D8, D9): the sea turtles leave their carve-out. TURTLE_RULE=dps merges
# them like every DPS-listed species (dps_sql: the value is the SUITABILITY masked to the ER footprint,
# the per-cell ER rides beside it) with the critical-habitat datasets EXTENDING the footprint at their
# own ER (D8: "CH extends the footprint and keeps the ER value, fill = ER"); TURTLE_SUIT=ogm swaps the
# turtles' suitability source to our own OBIS-method models (`og`) by superseding am (and ax) inside
# the og footprint -- the same supersede_sql() mechanism AquaX uses, applied twice.
test_that("TURTLE_RULE=dps: a turtle through dps_sql with critical habitat extending the ER footprint", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # a leatherback-like taxon: SWOT DPS ER 100 at cells 1-2 (an Endangered DPS), 50 at cell 3 (Threatened);
  # critical habitat (species-level ER 100) at cells 3 and 4 -- cell 4 is OUTSIDE the DPS footprint;
  # suitability 80 / 0 / 40 / 60 at cells 1-4 and 70 at cell 5 (no ER at all: outside the range)
  src <- data.frame(
    ms_merge_key = "T_turtle",
    ds_key  = c("rng_turtle_swot_dps","rng_turtle_swot_dps","rng_turtle_swot_dps", "ch_nmfs","ch_nmfs", "am","am","am","am"),
    cell_id = c(1L, 2L, 3L, 3L, 4L, 1L, 3L, 4L, 5L),
    val     = c(100, 100, 50, 100, 100, 80, 40, 60, 70), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "dps_src", src)
  d <- DBI::dbGetQuery(con, dps_sql(c("rng_turtle_swot_dps", "ch_nmfs"), "am", src = "dps_src"))
  d <- d[order(d$cell_id), ]
  expect_equal(d$cell_id, 1:4)                          # cell 5 (no ER, no CH) is outside the footprint
  expect_equal(d$val, c(80, 100, 40, 60))               # suitability where modelled; fill = ER where not (cell 2)
  expect_equal(d$er,  c(100, 100, 100, 100))            # cell 3: max(DPS 50, CH 100) -- CH keeps its ER; cell 4 extends the footprint
  # the old turtle rule on the same input is a different surface (ER multiplied in, floored at 1)
  t <- DBI::dbGetQuery(con, turtle_sql("rng_turtle_swot_dps", "am", "ch_nmfs", src = "dps_src"))
  expect_false(isTRUE(all.equal(sort(t$val), sort(d$val))))
})

test_that("TURTLE_SUIT=ogm: our og model supersedes am (and ax) for a turtle inside the og footprint only", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mc <- data.frame(
    ms_merge_key = c(rep("T_turtle", 7), rep("T_fish", 3)),
    ds_key  = c("am","am","am", "ax","ax", "og","og",  "am","am","og"),
    cell_id = c(1L, 2L, 3L,    1L, 2L,    1L, 2L,     1L, 2L, 1L),
    val     = c(50, 40, 30,    55, 45,    80, 20,     60, 70, 90), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "mc", mc)
  DBI::dbWriteTable(con, "og_mask", data.frame(cell_id = 1:2))          # where the og ensemble has a value
  DBI::dbWriteTable(con, "turtle_og", data.frame(ms_merge_key = "T_turtle"))
  keep <- paste(supersede_sql("am", taxa = "turtle_og", mask = "og_mask", src = "mc"),
                "AND", supersede_sql("ax", taxa = "turtle_og", mask = "og_mask", src = "mc"))
  b <- DBI::dbGetQuery(con, paste("SELECT * FROM mc WHERE", keep, "ORDER BY ms_merge_key, ds_key, cell_id"))
  tt <- b[b$ms_merge_key == "T_turtle", ]
  # inside the og footprint (cells 1-2) am and ax are gone; am survives at cell 3 (outside); og stays
  expect_equal(paste(tt$ds_key, tt$cell_id), c("am 3", "og 1", "og 2"))
  # a taxon not in the turtle list keeps everything, og included (it would merge by max -- which is why
  # merge_models_prep excludes og entirely when TURTLE_SUIT=am)
  expect_equal(nrow(b[b$ms_merge_key == "T_fish", ]), 3)
  # with the og rows superseding, the DPS turtle merge reads og as the suitability
  DBI::dbExecute(con, paste0("CREATE TABLE dps_src AS SELECT * FROM mc WHERE ms_merge_key = 'T_turtle' AND (", keep, ")
    UNION ALL SELECT 'T_turtle', 'rng_turtle_swot_dps', cell_id, 100 FROM (SELECT unnest([1, 2, 3]) AS cell_id)"))
  d <- DBI::dbGetQuery(con, dps_sql("rng_turtle_swot_dps", c("am", "ax", "og"), src = "dps_src"))
  d <- d[order(d$cell_id), ]
  expect_equal(d$val, c(80, 20, 30))                    # og at 1-2, the surviving am at 3
})

# v11 Q6: density replaces suitability INSIDE each species' own survey domain (supersede_sql(by_taxon = TRUE)),
# the domain is admitted to the footprint, and the taxon merges through dps_sql with a constant per-cell ER
# (er_mode = 'cell') so the survey shows for an Endangered species too (max(er 100, suit) would hide it).

test_that("supersede_sql(by_taxon = TRUE): a per-taxon mask drops am only inside THAT taxon's domain", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mc <- data.frame(
    ms_merge_key = c(rep("T_shelf", 4), rep("T_ocean", 4), rep("T_nodens", 2)),
    ds_key  = c("am", "am", "gm", "gm",   "am", "am", "gm", "gm",   "am", "am"),
    cell_id = c(1L, 2L, 1L, 3L,          1L, 2L, 2L, 3L,           1L, 2L),
    val     = c(50, 40, 90, 10,          50, 40, 70, 30,           50, 40), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "mc", mc)
  # the domains differ per taxon: shelf model covers cells 1,3; oceanic covers 2,3; T_nodens has none
  DBI::dbWriteTable(con, "dens_dom", data.frame(ms_merge_key = c("T_shelf", "T_shelf", "T_ocean", "T_ocean"),
                                                cell_id = c(1L, 3L, 2L, 3L)))
  DBI::dbWriteTable(con, "dens_taxa", data.frame(ms_merge_key = c("T_shelf", "T_ocean")))
  kept <- DBI::dbGetQuery(con, paste("SELECT * FROM mc WHERE", supersede_sql("am", "dens_taxa", "dens_dom", "mc", by_taxon = TRUE)))
  kept$mdl_key <- kept$ms_merge_key   # key_set() reads mdl_key (the merged output's name)
  # T_shelf: am at cell 1 gone (inside its domain), am at 2 kept (outside its domain, though inside T_ocean's)
  expect_equal(key_set(kept[kept$ds_key == "am", ], "T_shelf"), "2:40")
  # T_ocean: am at cell 2 gone, am at 1 kept
  expect_equal(key_set(kept[kept$ds_key == "am", ], "T_ocean"), "1:50")
  # a taxon with no density keeps everything; density rows are never touched
  expect_equal(key_set(kept, "T_nodens"), c("1:50", "2:40"))
  expect_equal(sum(kept$ds_key == "gm"), 4L)
  # the one-mask-for-all form would have dropped T_shelf's am at cell 2 as well (cell 2 is in SOME domain)
  DBI::dbExecute(con, "CREATE TABLE dens_any AS SELECT DISTINCT cell_id FROM dens_dom")
  kept_global <- DBI::dbGetQuery(con, paste("SELECT * FROM mc WHERE", supersede_sql("am", "dens_taxa", "dens_any", "mc")))
  expect_equal(nrow(kept_global[kept_global$ds_key == "am" & kept_global$ms_merge_key == "T_shelf", ]), 0L)
})

test_that("density + ER: through dps_sql the survey value shows under a constant per-cell ER, inside and outside the domain", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # an Endangered whale (er 100): range cells 1-4 at the ER; am at 1,2,3; survey (gm) domain = cells 2,3,4 with
  # density-suitability 80, 5, 0 (0 = surveyed, none seen); cell 4 is in the domain but outside the am
  mc <- data.frame(
    ms_merge_key = "T_whale",
    ds_key  = c("er_range", "er_range", "er_range", "er_range", "am", "am", "am", "gm", "gm", "gm"),
    cell_id = c(1L, 2L, 3L, 4L,                                  1L, 2L, 3L,   2L, 3L, 4L),
    val     = c(100, 100, 100, 100,                              60, 70, 50,   80, 5, 0), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "mc", mc)
  DBI::dbWriteTable(con, "dens_dom",  data.frame(ms_merge_key = "T_whale", cell_id = c(2L, 3L, 4L)))
  DBI::dbWriteTable(con, "dens_taxa", data.frame(ms_merge_key = "T_whale"))
  DBI::dbExecute(con, paste("CREATE TABLE dps_src AS SELECT * FROM mc WHERE",
                            supersede_sql("am", "dens_taxa", "dens_dom", "mc", by_taxon = TRUE)))
  res <- DBI::dbGetQuery(con, dps_sql("er_range", c("am", "gm"), src = "dps_src"))
  # cell 1: outside the domain -> am 60; cells 2,3,4: the survey (80, 5, 0), am replaced; ER = 100 beside every cell
  expect_equal(key_set(res, "T_whale"), c("1:60", "2:80", "3:5", "4:0"))
  expect_equal(unique(res$er), 100)
  # the old max(er, suit) rule would have painted the whale a flat 100 and hidden the survey entirely
  flat <- DBI::dbGetQuery(con, "SELECT cell_id, max(val) val FROM dps_src GROUP BY 1 ORDER BY 1")
  expect_equal(unique(flat$val), 100)
})

test_that("dps_sql(er_floor): a non-listed taxon merged through the dps rule keeps its governing er_score outside its per-cell ER rows (v11 fold-in regression)", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # a LC murre (IUCN 1) whose governing er_score is 10 (MBTA floor): BirdLife range rows carry 1 at cells 1-3,
  # the survey domain row (dens_dom, written at the governing er_score) carries 10 at cell 3, the survey (nc) 40 at cell 3
  DBI::dbWriteTable(con, "dps_src", data.frame(
    ms_merge_key = "T", ds_key = c("bl", "bl", "bl", "dens_dom", "nc"), cell_id = c(1L, 2L, 3L, 3L, 3L), val = c(1, 1, 1, 10, 40)))
  g <- function(...) DBI::dbGetQuery(con, paste(dps_sql(c("bl", "dens_dom"), c("am", "nc"), ...), "ORDER BY cell_id"))
  no_floor <- g()
  expect_equal(no_floor$er,  c(1, 1, 10))        # the bug: cells 1-2 at the IUCN value, the floor only where dens_dom covers
  expect_equal(no_floor$val, c(1, 1, 40))
  floored <- g(er_floor = 10)
  expect_equal(floored$er,  c(10, 10, 10))       # the governing er_score everywhere
  expect_equal(floored$val, c(10, 10, 40))       # a range-only cell is valued at the ER; the survey cell keeps the survey
  higher <- DBI::dbGetQuery(con, paste(dps_sql(c("bl", "dens_dom"), c("am", "nc"), er_floor = 5), "ORDER BY cell_id"))
  expect_equal(higher$er, c(5, 5, 10))           # a per-cell ER above the floor is kept (critical habitat, a listed DPS)
  expect_error(dps_sql("bl", "am", er_floor = -1))
  expect_error(dps_sql("bl", "am", er_floor = c(1, 2)))
})

# v11 SHARE_BASIS / RANGE_FILL (2026-11-02): single counting. basis = "taxon" is the v4-v10 rule
# (max(er, suit) on range cells; the risk is multiplied in again at scoring); basis = "cell" values a
# range cell by its suitability alone and by `fill` where no suitability model covers it. The
# discriminating fixture is T_ax_both at cell 2: er 30, suitability (ax) 20 -> taxon 30, cell 20.
test_that("SHARE_BASIS=cell: range cells take the suitability alone, the fill where nothing models them; the defaults reproduce the taxon rule", {
  con <- merge_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # the default IS the taxon basis, byte for byte
  expect_identical(merge_sql(SUIT), merge_sql(SUIT, basis = "taxon"))
  expect_identical(merge_sql(SUIT), merge_sql(SUIT, basis = "taxon", fill = "presence"))   # fill is moot
  us_t  <- DBI::dbGetQuery(con, merge_sql(SUIT)$us)
  us_c  <- DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell")$us)                        # fill = er
  us_p  <- DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell", fill = "presence")$us)
  gl_c  <- DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell")$global)
  gl_p  <- DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell", fill = "presence")$global)
  # T_ax_both (er 30): suitability 80 / 20 at cells 1-2 (ax inside the mask), 70 at cell 4 (am)
  expect_equal(key_set(us_t, "T_ax_both"), c("1:80", "2:30", "4:70"))   # taxon: max(30, 20) = 30 at cell 2
  expect_equal(key_set(us_c, "T_ax_both"), c("1:80", "2:20", "4:70"))   # cell: the suitability, 20
  expect_equal(key_set(us_p, "T_ax_both"), c("1:80", "2:20", "4:70"))   # every range cell is modeled: fill moot
  expect_equal(key_set(gl_c, "T_ax_both"), c("1:80", "2:20", "4:70", "100:95"))
  # T_range (range-only, er 50): no suitability anywhere -> the fill decides
  expect_equal(key_set(us_c, "T_range"), c("1:50", "2:50"))              # fill = er: as before
  expect_equal(key_set(us_p, "T_range"), c("1:100", "2:100"))            # fill = presence
  expect_equal(key_set(gl_p, "T_range"), c("1:100", "2:100", "100:100"))
  # T_both_mask (er 30, am 60 / 40 at the range cells): suitability above the ER, identical under both bases;
  # am beyond the range (cells 3, 101) stays masked under the cell basis too
  expect_equal(key_set(us_c, "T_both_mask"), c("1:60", "2:40"))
  expect_equal(key_set(gl_p, "T_both_mask"), c("1:60", "2:40"))
  # the am-only branch and the no-EEZ exclusion are untouched by the basis
  expect_equal(key_set(us_p, "T_am_single"), key_set(us_t, "T_am_single"))
  expect_equal(nrow(us_p[us_p$mdl_key == "T_noeez", ]), 0L)
  # single counting: scoring multiplies the governing er in ONCE. T_range under presence scores er x 100 / 100 = er
  # per cell; under the taxon basis the same cell scored er x er / 100 (25 for er 50)
  expect_equal(50 * us_p$val[us_p$mdl_key == "T_range"] / 100, c(50, 50))
  expect_equal(50 * us_t$val[us_t$mdl_key == "T_range"] / 100, c(25, 25))
  expect_error(merge_sql(SUIT, basis = "both"))
  expect_error(merge_sql(SUIT, basis = "cell", fill = "zero"))
})

test_that("dps_sql(fill = 'presence'): a footprint cell with no suitability model is 100, not its ER; the ER beside it is unchanged", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  src <- data.frame(
    ms_merge_key = "T_dps",
    ds_key  = c("dps_nmfs", "dps_nmfs", "dps_nmfs", "dps_nmfs", "ax", "ax", "am", "am", "am"),
    cell_id = c(1L, 2L, 3L, 4L, 1L, 2L, 2L, 4L, 5L),
    val     = c(100, 50, 20, 20, 80, 40, 30, 5, 60), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "dps_src", src)
  er_fill <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src"))
  pr_fill <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src", fill = "presence"))
  expect_equal(key_set(er_fill, "T_dps"), c("1:80", "2:40", "3:20", "4:5"))    # the default, as before
  expect_equal(key_set(pr_fill, "T_dps"), c("1:80", "2:40", "3:100", "4:5"))   # cell 3: nothing models it -> 100
  expect_equal(pr_fill[order(pr_fill$cell_id), "er"], c(100, 50, 20, 20))      # the ER layer does not move
  # with er_floor the floor still applies to the ER (and so to an er fill), never to a presence fill
  fl <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src", er_floor = 30, fill = "presence"))
  expect_equal(key_set(fl, "T_dps"), c("1:80", "2:40", "3:100", "4:5"))
  expect_equal(fl[order(fl$cell_id), "er"], c(100, 50, 30, 30))
  expect_error(dps_sql("dps_nmfs", SUIT, fill = "none"))
})

# RANGE_FILL=mean (the v11 decision, 2026-11-02): a range cell no suitability model covers is worth the mean of
# the taxon's MODELLED range cells over the WHOLE range; a taxon with none takes presence (100). The fixture
# taxon has a range of four cells, AquaMaps at three of them (60, 20 inside the US; 4 OUTSIDE it, cell 100;
# mean 28), and AquaMaps beyond the range (cell 9, masked away and NOT in the mean). The US-only mean would be
# 40: the window that produced it (2026-11-06) painted the scored surface with a different fill than the global
# one, so this fixture is one the per-surface window FAILS.
test_that("RANGE_FILL=mean: unmodelled range cells take the whole-range mean of the modelled range cells on BOTH surfaces; a taxon with none takes presence", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mc <- data.frame(
    ms_merge_key = c(rep("T_part", 8), rep("T_rngonly", 2)),
    ds_key  = c("rng", "rng", "rng", "rng", "am", "am", "am", "am",   "rng", "rng"),
    cell_id = c(1L, 2L, 3L, 100L, 1L, 2L, 100L, 9L,   1L, 100L),
    val     = c(1, 1, 1, 1, 60, 20, 4, 90,   1, 1), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "b", mc)
  DBI::dbWriteTable(con, "taxon", data.frame(ms_merge_key = c("T_part", "T_rngonly"), er_score = c(30, 50)))
  DBI::dbWriteTable(con, "taxon_flags", data.frame(ms_merge_key = c("T_part", "T_rngonly"), has_suit = c(TRUE, FALSE), has_range = c(TRUE, TRUE)))
  DBI::dbWriteTable(con, "us_cells", data.frame(cell_id = 1:5))
  msq <- merge_sql(SUIT, basis = "cell", fill = "mean")
  DBI::dbExecute(con, msq$b_range); DBI::dbExecute(con, msq$b_am_rng); DBI::dbExecute(con, msq$b_fill)
  gl <- DBI::dbGetQuery(con, msq$global); us <- DBI::dbGetQuery(con, msq$us)
  # modelled cells keep their suitability; cell 3 takes the mean of 60, 20 and 4 = 28 on BOTH surfaces (the
  # modelled cell outside the US counts); cell 9 never enters
  expect_equal(key_set(gl, "T_part"), c("1:60", "2:20", "3:28", "100:4"))
  expect_equal(key_set(us, "T_part"), c("1:60", "2:20", "3:28"))
  # a range-only taxon: no modelled cell, so presence
  expect_equal(key_set(gl, "T_rngonly"), c("1:100", "100:100"))
  # the same input under the er and presence fills, for contrast
  expect_equal(key_set(DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell", fill = "er")$global), "T_part"), c("1:60", "2:20", "3:30", "100:4"))
  expect_equal(key_set(DBI::dbGetQuery(con, merge_sql(SUIT, basis = "cell", fill = "presence")$global), "T_part"), c("1:60", "2:20", "3:100", "100:4"))
  # the mean is per taxon: a second partially modelled taxon in the same batch does not share it
  DBI::dbExecute(con, "INSERT INTO b VALUES ('T_part2', 'rng', 1, 1), ('T_part2', 'rng', 2, 1), ('T_part2', 'am', 1, 10)")
  DBI::dbExecute(con, "INSERT INTO taxon VALUES ('T_part2', 20)")
  DBI::dbExecute(con, "INSERT INTO taxon_flags VALUES ('T_part2', TRUE, TRUE)")
  DBI::dbExecute(con, msq$b_range); DBI::dbExecute(con, msq$b_am_rng); DBI::dbExecute(con, msq$b_fill)
  gl2 <- DBI::dbGetQuery(con, msq$global); us2 <- DBI::dbGetQuery(con, msq$us)
  expect_equal(key_set(gl2, "T_part2"), c("1:10", "2:10"))
  expect_equal(key_set(gl2, "T_part"), c("1:60", "2:20", "3:28", "100:4"))
  expect_equal(key_set(us2, "T_part"), c("1:60", "2:20", "3:28"))
})

test_that("dps_sql(fill = 'mean'): an unmodelled footprint cell takes the mean of the modelled footprint cells, per taxon", {
  skip_if_not_installed("glue")
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  src <- data.frame(
    ms_merge_key = c(rep("T_dps", 9), rep("T_dps2", 3)),
    ds_key  = c("dps_nmfs", "dps_nmfs", "dps_nmfs", "dps_nmfs", "ax", "ax", "am", "am", "am",   "dps_nmfs", "dps_nmfs", "am"),
    cell_id = c(1L, 2L, 3L, 4L, 1L, 2L, 2L, 4L, 5L,   1L, 2L, 2L),
    val     = c(100, 50, 20, 20, 80, 40, 30, 5, 60,   20, 20, 10), stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, "dps_src", src)
  res <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src", fill = "mean"))
  # T_dps: modelled footprint cells 1 (80), 2 (40), 4 (5): mean 41.667 at cell 3; cell 5 (am 60) is beyond the
  # footprint and masked away, so it is NOT in the mean
  r1 <- res[res$mdl_key == "T_dps", ]; r1 <- r1[order(r1$cell_id), ]
  expect_equal(r1$val, c(80, 40, (80 + 40 + 5) / 3, 5))
  expect_equal(r1$er, c(100, 50, 20, 20))
  # T_dps2: its own mean (10), not T_dps's
  r2 <- res[res$mdl_key == "T_dps2", ]; r2 <- r2[order(r2$cell_id), ]
  expect_equal(r2$val, c(10, 10))
  # a taxon with no modelled footprint cell takes presence
  DBI::dbExecute(con, "INSERT INTO dps_src VALUES ('T_dps3', 'dps_nmfs', 1, 50)")
  r3 <- DBI::dbGetQuery(con, dps_sql("dps_nmfs", SUIT, src = "dps_src", fill = "mean"))
  expect_equal(r3$val[r3$mdl_key == "T_dps3"], 100)
  expect_error(dps_sql("dps_nmfs", SUIT, fill = "median"))
})
