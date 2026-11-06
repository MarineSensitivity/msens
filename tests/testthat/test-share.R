# Guards the v10 distribution-share rule (share_sql / share_run / denom_sql) -- one fixture per rule
# of the method page (artifact 773b9b71 rev 14, §2 formulas, §6 floor, §9b fixtures, §13a species
# weights) and of plan 2026-10-29 §2 "Acceptance". A dropped WHERE, a lost pct_covered, a
# denominator taken on the wrong surface or over ocean cells only, a floor that returns 0 instead of
# no row, a species counted per model instead of once, or a second ER weight on a turtle all fail
# here with an exact expected value.
#
# The toy study area: 100 US cells (ids 1-100), every cell 1 km2 except cell 61 (0.98 km2). The
# grid is 100 columns wide (nc = 100), so rows are ids 1-100, 101-200, 201-300, 301-400. Program Areas:
#   PA-A cells 1-10   PA-B 11-20   PA-C 21-40   PA-K 41-60   PA-L 61-80   REM 81-100 (the remainder)
# plus the subregion pseudo-zone USA (cells 1-100) and one ecoregion-field zone ECO-H over cells 1-2
# at pct_covered 50 (a half-covered edge), so a roll-up that forgets the coverage weight cannot pass.
# Cells 101-310 lie outside US waters and exist only in the species' GLOBAL surfaces (mc_global /
# mc_am), never in model_cell (the US scoring surface). Cells 290-299 are LAND: absent from `cell`,
# present in a bird's range -- a denominator that inner-joins `cell` loses them.
#
# Species (ms_merge_key, category, er_score, er_mode):
#   endemic  A  50  taxon          val 100 in cells 1-10 only, worldwide            -> all of it is PA-A
#   wide     A 100  taxon          val 100 in cell 1 + 99 cells abroad              -> 1 % of its range in the US
#   wide2    A 100  taxon          val 100 in cell 11 + 199 ocean cells abroad      -> 0.5 % of its range in PA-B
#   dup      A 100  taxon          am-only with TWO models at cell 13 (+ 98 cells abroad): duplicate rows kept
#   hi       A 100  taxon          am-only, cells 50-51 only (nothing abroad)       -> an EEZ endemic as modelled
#   low      A 100  taxon          val 1 at cell 12 (a genuine low value, taxon mode: never stripped)
#   exclu    A 100  taxon          val 100 in cells 1-10 but is_valid_usa = FALSE    -> never scored
#   rept     R 100  taxon          val 100 in cells 1-10, category not scored        -> never scored
#   even     B 100  taxon          val 50 in cells 1-10 and 21-40                   -> same intensity, PA-C twice the area
#   uniform  C  20  taxon          val 100 in every US cell, nothing abroad         -> every zone at the national average
#   nat      D 100  taxon          val 100 in cells 1-10, FWS-range only; literature fraction f = 0.25
#   cov      E 100  taxon          val 100 at cell 41 (5.0 % of PA-K) and cell 61 (4.9 % of PA-L)
#   turtle   turtle 100 premultiplied  val 80 at cell 1, floored val 1 at cell 2, val 50 at cell 101 abroad
#   dps      F 100  cell           val 80 at cells 1, 2 (per-cell ER 100, 20) and at cell 101 abroad
#   dps20    F 100  cell           val 80 at cells 3, 4, ER 20 at both, nothing abroad -> total weight 0.2
#   bird     G  10  taxon          val 100 at cell 5 + the 10 LAND cells 290-299     -> 1/11 of its range in the US

skip_if_not_installed("duckdb")

NC <- 100L
LAND <- 290:299
AREA <- rep(1, 310); AREA[61] <- 0.98   # the area of every cell of the toy grid, land included

share_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())

  cell <- data.frame(cell_id = setdiff(1:310, LAND), in_usa = FALSE)
  cell$area_km2 <- AREA[cell$cell_id]; cell$in_usa <- cell$cell_id <= 100

  zone <- data.frame(
    zone_seq = 1:8,
    fld      = c(rep("programarea_key", 6), "subregion_key", "ecoregion_key"),
    val      = c("PA-A", "PA-B", "PA-C", "PA-K", "PA-L", "REM", "USA", "ECO-H"))
  zone_cell <- rbind(
    data.frame(zone_seq = 1L, cell_id =  1:10,  pct_covered = 100),
    data.frame(zone_seq = 2L, cell_id = 11:20,  pct_covered = 100),
    data.frame(zone_seq = 3L, cell_id = 21:40,  pct_covered = 100),
    data.frame(zone_seq = 4L, cell_id = 41:60,  pct_covered = 100),
    data.frame(zone_seq = 5L, cell_id = 61:80,  pct_covered = 100),
    data.frame(zone_seq = 6L, cell_id = 81:100, pct_covered = 100),
    data.frame(zone_seq = 7L, cell_id =  1:100, pct_covered = 100),
    data.frame(zone_seq = 8L, cell_id =  1:2,   pct_covered = 50))

  taxon <- data.frame(
    ms_merge_key = c("endemic", "wide", "wide2", "dup", "hi", "low", "exclu", "rept", "even", "uniform", "nat", "cov", "turtle", "dps", "dps20", "bird"),
    sp_cat       = c("A", "A", "A", "A", "A", "A", "A", "R", "B", "C", "D", "E", "turtle", "F", "F", "G"),
    er_score     = c(50, 100, 100, 100, 100, 100, 100, 100, 100, 20, 100, 100, 100, 100, 100, 10),
    er_mode      = c(rep("taxon", 12), "premultiplied", "cell", "cell", "taxon"),
    is_valid_usa = TRUE, is_marine = TRUE, stringsAsFactors = FALSE)
  taxon$is_valid_usa[taxon$ms_merge_key == "exclu"] <- FALSE

  mc <- function(key, cells, val) data.frame(mdl_key = key, cell_id = cells, val = val)
  # the US scoring surface (dup: two AquaMaps models -> its cell appears twice, as the merge keeps it)
  model_cell <- rbind(
    mc("endemic", 1:10, 100), mc("wide", 1, 100), mc("wide2", 11, 100), mc("dup", c(13, 13), 100),
    mc("hi", 50:51, 100), mc("low", 12, 1), mc("exclu", 1:10, 100), mc("rept", 1:10, 100),
    mc("even", c(1:10, 21:40), 50), mc("uniform", 1:100, 100), mc("nat", 1:10, 100),
    mc("cov", c(41, 61), 100), mc("turtle", 1:2, c(80, 1)), mc("dps", 1:2, 80), mc("dps20", 3:4, 80),
    mc("bird", 5, 100))
  # the whole-range global surfaces of the range-constrained taxa (am-only taxa have none)
  mc_global <- rbind(
    mc("endemic", 1:10, 100), mc("low", 12, 1), mc("exclu", 1:10, 100), mc("rept", 1:10, 100),
    mc("even", c(1:10, 21:40), 50), mc("uniform", 1:100, 100), mc("nat", 1:10, 100),
    mc("cov", c(41, 61), 100), mc("turtle", c(1, 2, 101), c(80, 1, 50)), mc("dps", c(1, 2, 101), 80),
    mc("dps20", 3:4, 80), mc("bird", c(5, LAND), 100))
  # the AquaMaps surfaces of the am-only taxa (wide, wide2, dup x2, hi), plus one for `endemic` to
  # prove the merged global surface takes precedence over AquaMaps
  mc_am <- rbind(
    mc("am|wide",  c(1, 101:199), 100), mc("am|wide2", c(11, 101:289, 300:309), 100),
    mc("am|dupa",  c(13, 101:198), 100), mc("am|dupb", 13, 100),
    mc("am|hi",    50:51, 100), mc("am|endemic", c(1:10, 101:110), 100))
  model_cell_er <- data.frame(mdl_key = c("dps", "dps", "dps20", "dps20"), cell_id = c(1, 2, 3, 4), er = c(100, 20, 20, 20))
  taxon_model <- data.frame(
    mdl_key = c("rng|endemic", "am|endemic", "am|wide", "am|wide2", "am|dupa", "am|dupb", "am|hi", "rng|low", "rng|exclu", "rng|rept",
                "rng|even", "rng|uniform", "rng_fws|nat", "rng|cov", "rng_turtle_swot_dps|turtle", "dps_nmfs|dps", "dps_nmfs|dps20", "bl|bird"),
    ms_merge_key = c("endemic", "endemic", "wide", "wide2", "dup", "dup", "hi", "low", "exclu", "rept",
                     "even", "uniform", "nat", "cov", "turtle", "dps", "dps20", "bird"),
    ds_key = c("rng_iucn", "am", "am", "am", "am", "am", "am", "rng_iucn", "rng_iucn", "rng_iucn",
               "rng_iucn", "rng_iucn", "rng_fws", "rng_iucn", "rng_turtle_swot_dps", "dps_nmfs", "dps_nmfs", "bl"),
    stringsAsFactors = FALSE)
  fallback <- data.frame(ms_merge_key = "nat", fraction = 0.25, method = "literature", stringsAsFactors = FALSE)

  # the denominators as the share tests expect them (denom_sql() is tested separately against these):
  # sum of v/100 * A over the species' whole range, land included
  gl <- function(key, strip = FALSE) {
    g <- mc_global[mc_global$mdl_key == key, ]
    if (nrow(g) == 0) return(NA_real_)
    v <- g$val; if (strip && key == "turtle") v[v <= 1] <- 0
    sum(v / 100 * AREA[g$cell_id])
  }
  taxon_denom <- data.frame(
    ms_merge_key       = taxon$ms_merge_key,
    total_global       = vapply(taxon$ms_merge_key, gl, 0),
    total_global_strip = vapply(taxon$ms_merge_key, gl, 0, strip = TRUE),
    source             = "merged_global", stringsAsFactors = FALSE)
  set_d <- function(key, tot, src) {
    i <- taxon_denom$ms_merge_key == key
    taxon_denom$total_global[i] <<- tot; taxon_denom$total_global_strip[i] <<- tot; taxon_denom$source[i] <<- src
  }
  set_d("wide",  100, "aquamaps"); set_d("wide2", 200, "aquamaps"); set_d("dup", 98 + 2, "aquamaps"); set_d("hi", 2, "aquamaps")
  set_d("nat", 10 / 0.25, "fallback:literature")

  for (nm in c("cell", "zone", "zone_cell", "taxon", "model_cell", "mc_global", "mc_am", "model_cell_er",
               "taxon_model", "fallback", "taxon_denom"))
    DBI::dbWriteTable(con, nm, get(nm))
  con
}

CATS <- c("A", "B", "C", "D", "E", "turtle", "F", "G")
A_USA <- 99.98
zrow <- function(r, zone_seq, cat) r$zone[r$zone$zone_seq == zone_seq & r$zone$sp_cat == cat, ]
ztax <- function(r, zone_seq, key) r$zone_taxon[r$zone_taxon$zone_seq == zone_seq & r$zone_taxon$ms_merge_key == key, ]
crow <- function(r, cat, cell) r$cell[r$cell$sp_cat == cat & r$cell$cell_id == cell, ]

test_that("an endemic confined to one zone has share 1 there and no row elsewhere; a half-covered zone holds half", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  expect_equal(ztax(r, 1, "endemic")$share_global, 1)
  expect_equal(ztax(r, 1, "endemic")$share_usa, 1)
  expect_equal(nrow(ztax(r, 2, "endemic")), 0)          # absent, never zero
  expect_equal(ztax(r, 7, "endemic")$share_global, 1)   # the nation holds all of it
  # the endemic's contribution to PA-A's category-A total is its weight (er 50) times 1
  expect_equal(ztax(r, 1, "endemic")$contrib, 0.5)
  # a half-covered edge zone holds half of each of its two cells: 2 x 0.5 of 10
  expect_equal(ztax(r, 8, "endemic")$share_global, 0.1)
  expect_equal(ztax(r, 8, "endemic")$contrib, 0.05)
  expect_equal(zrow(r, 8, "A")$area_km2, 1)
})

test_that("only valid, marine species of the scored categories enter", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  expect_false("exclu" %in% r$taxon$ms_merge_key)       # is_valid_usa = FALSE
  expect_false("rept"  %in% r$taxon$ms_merge_key)       # category R is not scored
  expect_false("R" %in% r$zone$sp_cat)
  # PA-A's category-A total is endemic 0.5 + wide 0.01 and nothing from `exclu`
  expect_equal(zrow(r, 1, "A")$share, 0.5 + 0.01)
  expect_equal(zrow(r, 1, "A")$n_species, 2L)
  # dropping the validity clause would admit `exclu`: say so by running without it
  r_all <- share_run(con, cats = CATS, denom = "global", taxa_where = "TRUE")
  expect_equal(zrow(r_all, 1, "A")$share, 0.5 + 0.01 + 1)
})

test_that("0.5 % of a species' range in a zone is 0.005; 1 % in the study area is usa share 1, global 0.01", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  expect_equal(ztax(r, 2, "wide2")$share_global, 0.005)
  # the sliver case (page §9b): all of the species' US distribution is in one cell, but that cell
  # is 1 % of the world's
  expect_equal(ztax(r, 1, "wide")$share_usa, 1)
  expect_equal(ztax(r, 1, "wide")$share_global, 0.01)
  expect_equal(r$taxon$f_us[r$taxon$ms_merge_key == "wide"], 0.01)
  # under the global denominator the species counts w * f_us in the category total ...
  expect_equal(ztax(r, 1, "wide")$contrib, 1.0 * 0.01)
  # ... and under the usa denominator it counts w: the two differ only in that weight (page §13a)
  ru <- share_run(con, cats = CATS, denom = "usa")
  expect_equal(ztax(ru, 1, "wide")$contrib, 1.0)
  expect_equal(ztax(ru, 1, "wide")$share_global, 0.01)  # the per-species columns are reported either way
  expect_equal(ru$zone_taxon$contrib, r$zone_taxon$contrib / r$taxon$f_us[match(ru$zone_taxon$ms_merge_key, r$taxon$ms_merge_key)])
})

test_that("a species with several models keeps its duplicate rows but is counted once", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  # dup: two models at cell 13 -> US total 2, global 100 (98 abroad + 2 inside)
  expect_equal(r$taxon$total_usa[r$taxon$ms_merge_key == "dup"], 2)
  expect_equal(ztax(r, 2, "dup")$share_usa, 1)
  expect_equal(ztax(r, 2, "dup")$share_global, 0.02)
  expect_equal(crow(r, "A", 13)$n_species, 1L)          # distinct species, not rows
  expect_equal(zrow(r, 2, "A")$n_species, 3L)           # wide2, dup, low
})

test_that("every species' cell shares sum to 1 (+-1e-9) over its denominator's cells, per denominator", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # usa: the nation is the whole denominator -> exactly 1 for every species
  ru <- share_run(con, cats = CATS, denom = "usa")
  usa <- ru$zone_taxon[ru$zone_taxon$zone_seq == 7, ]
  expect_equal(usa$share_usa, rep(1, nrow(usa)), tolerance = 1e-9)
  # global: the US cells sum to f_us, and the cells abroad make up the rest -- checked on the global
  # surfaces directly, land cells (absent from `cell`) at their grid area
  rg <- share_run(con, cats = CATS, denom = "global")
  tot <- DBI::dbGetQuery(con, "
    SELECT g.mdl_key, SUM(g.val / 100.0 * coalesce(c.area_km2, 1.0)) / d.total_global AS s
    FROM mc_global g LEFT JOIN cell c USING (cell_id) JOIN taxon_denom d ON d.ms_merge_key = g.mdl_key
    WHERE d.source = 'merged_global' AND g.mdl_key IN (SELECT ms_merge_key FROM taxon WHERE is_valid_usa)
    GROUP BY g.mdl_key, d.total_global")
  expect_true(nrow(tot) >= 8)
  expect_equal(tot$s, rep(1, nrow(tot)), tolerance = 1e-9)
  expect_equal(rg$taxon$f_us[rg$taxon$ms_merge_key == "bird"], 1 / 11)
  # the sum of the US cells of every species is f_us by construction (and 1 under usa)
  nat <- rg$zone_taxon[rg$zone_taxon$zone_seq == 7, ]
  expect_equal(nat$share_global, rg$taxon$f_us[match(nat$ms_merge_key, rg$taxon$ms_merge_key)], tolerance = 1e-9)
})

test_that("two zones of equal intensity and unequal area score the same v10 but hold unequal totals", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  a <- zrow(r, 1, "B"); cc <- zrow(r, 3, "B")
  # even: 15 units of value-area worldwide, 5 in PA-A (10 km2) and 10 in PA-C (20 km2); USA holds 1
  expect_equal(a$share, 1/3); expect_equal(cc$share, 2/3)
  expect_equal(a$v10,  100 * (1/3) / (10 / A_USA))
  expect_equal(cc$v10, 100 * (2/3) / (20 / A_USA))
  expect_equal(a$v10, cc$v10)
  # the companion grows with the area: percent of the world's category-B importance (sum_w = 1)
  expect_equal(a$pct_world, 100 * (1/3)); expect_equal(cc$pct_world, 100 * (2/3))
  expect_equal(a$pct_nation, 100 * (1/3)); expect_equal(cc$pct_nation, 100 * (2/3))
})

test_that("the percent of the world divides by the category's summed species weights, not its species count", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  # category A: endemic 50, wide 100, wide2 100, dup 100, hi 100, low 100 -> sum_w / 100 = 5.5 (6 species)
  expect_equal(zrow(r, 1, "A")$pct_world, 100 * (0.5 + 0.01) / 5.5)
  # a turtle counts 100 whatever its er_score; a DPS species counts its er_score
  expect_equal(r$taxon$w_s[r$taxon$ms_merge_key == "turtle"], 100)
  expect_equal(r$taxon$w_s[r$taxon$ms_merge_key == "dps20"], 100)
  expect_equal(r$taxon$w_s[r$taxon$ms_merge_key == "bird"], 10)
})

test_that("a zone at the national average concentration scores exactly 100, under every denominator", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (d in c("global", "usa", "pooled")) {
    r <- share_run(con, cats = CATS, denom = d)
    z <- r$zone[r$zone$sp_cat == "C", ]
    expect_equal(z$v10, rep(100, nrow(z)), info = d)     # incl. the half-covered ECO-H
    expect_equal(nrow(z), 8, info = d)
    # and the Program Areas plus the remainder add up to the nation (shares and areas alike)
    pa <- z$zone_seq %in% 1:6
    expect_equal(sum(z$share[pa]), z$share[z$zone_seq == 7], info = d)
    expect_equal(sum(z$area_km2[pa]), z$area_km2[z$zone_seq == 7], info = d)
  }
  # the cell-level score is the same quotient per cell: every uniform cell is exactly 100
  r <- share_run(con, cats = CATS, denom = "global")
  expect_equal(r$cell$v10[r$cell$sp_cat == "C"], rep(100, 100))
})

test_that("a literature fraction f gives global share = usa share x f, and the species is flagged", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  expect_equal(ztax(r, 1, "nat")$share_usa, 1)
  expect_equal(ztax(r, 1, "nat")$share_global, 0.25)
  expect_equal(r$taxon$source[r$taxon$ms_merge_key == "nat"], "fallback:literature")
  expect_equal(r$taxon$f_us[r$taxon$ms_merge_key == "nat"], 0.25)
})

test_that("coverage 4.9 % is not reportable (no score, row kept), 5.0 % is; the composite skips it", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global", kappa = 0.05)
  k <- zrow(r, 4, "E"); l <- zrow(r, 5, "E")
  expect_equal(k$coverage, 1 / 20);            expect_true(k$reportable);  expect_false(is.na(k$v10))
  expect_equal(l$coverage, 0.98 / 19.98);       expect_false(l$reportable); expect_true(is.na(l$v10))
  expect_equal(l$share, 0.98 / 1.98)            # the zone total is still reported
  # PA-L's composite averages only C (uniform, 100): E is skipped, never counted as 0
  comp <- r$composite
  expect_equal(comp$score[comp$zone_seq == 5], 100); expect_equal(comp$n_components[comp$zone_seq == 5], 1L)
  # PA-K's composite averages A (hi), C (100) and E (reportable)
  expect_equal(comp$n_components[comp$zone_seq == 4], 3L)
  expect_equal(comp$score[comp$zone_seq == 4], mean(c(zrow(r, 4, "A")$v10, 100, k$v10)))
  # kappa is a parameter: at 4 % both are reportable
  r4 <- share_run(con, cats = CATS, denom = "global", kappa = 0.04)
  expect_true(zrow(r4, 5, "E")$reportable)
})

test_that("a floored turtle cell (val 1) is kept by default and removed under the floor strip, both sides", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  # turtle: premultiplied -> w = 100 pass-through, no second ER weight; D = 0.8 + 0.01 + 0.5 = 1.31
  expect_equal(crow(r, "turtle", 2)$share, 0.01 / 1.31)
  expect_equal(crow(r, "turtle", 1)$share, 0.80 / 1.31)
  expect_equal(ztax(r, 1, "turtle")$contrib, 0.81 / 1.31)
  expect_equal(zrow(r, 8, "turtle")$coverage, 1)             # ECO-H: both cells carry a turtle
  rs <- share_run(con, cats = CATS, denom = "global", floor_strip = TRUE)
  expect_equal(nrow(crow(rs, "turtle", 2)), 0)                # removed: no row, so no coverage, no count
  expect_equal(crow(rs, "turtle", 1)$share, 0.80 / 1.30)      # the denominator is stripped too
  expect_equal(rs$taxon$total_global[rs$taxon$ms_merge_key == "turtle"], 1.30)
  expect_equal(rs$taxon$total_usa[rs$taxon$ms_merge_key == "turtle"], 0.80)
  expect_equal(zrow(rs, 8, "turtle")$coverage, 0.5)           # ECO-H: only cell 1 now
  # a zone whose only turtle cells are floored is ABSENT under the strip, never a zero
  expect_equal(nrow(zrow(rs, 8, "turtle")), 1)                # cell 1 keeps it present
  # the strip touches premultiplied taxa only: `low` (taxon mode, val 1) is untouched
  expect_equal(crow(rs, "A", 12)$share, crow(r, "A", 12)$share)
  expect_equal(nrow(crow(rs, "A", 12)), 1)
})

test_that("a DPS species' per-cell ER weights the share of its plain suitability total (er_mode = 'cell')", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  # dps: suitability 80 at cells 1, 2, 101 -> D = 2.4 (plain); ER 100 at cell 1, 20 at cell 2
  expect_equal(crow(r, "F", 1)$share, 100 * (0.8 / 2.4) / 100)
  expect_equal(crow(r, "F", 2)$share, 20  * (0.8 / 2.4) / 100)
  expect_equal(r$taxon$f_us[r$taxon$ms_merge_key == "dps"], 1.6 / 2.4)
  # NOT the premultiplied reading (0.8/1.12 at cell 1), which would give every DPS species total weight 1
  expect_false(isTRUE(all.equal(crow(r, "F", 1)$share, 0.8 / 1.12)))
  # a cell-mode species with uniform ER 20 contributes exactly 0.2 of itself, like a taxon-mode
  # species with er_score 20 -- the modes agree
  expect_equal(ztax(r, 7, "dps20")$contrib, 0.2)
  expect_equal(ztax(r, 7, "uniform")$contrib, 0.2)
  # a duplicated ER row would double a share through the join: a hard stop
  DBI::dbExecute(con, "INSERT INTO model_cell_er VALUES ('dps', 1, 100)")
  expect_error(share_run(con, cats = CATS, denom = "global"), "duplicate")
  DBI::dbExecute(con, "DELETE FROM model_cell_er WHERE mdl_key = 'dps' AND cell_id = 1")
  DBI::dbExecute(con, "INSERT INTO model_cell_er VALUES ('dps', 1, 100)")
  # a scored cell without its per-cell ER is a hard stop, not a silent zero
  DBI::dbExecute(con, "DELETE FROM model_cell_er WHERE mdl_key = 'dps' AND cell_id = 2")
  expect_error(share_run(con, cats = CATS, denom = "global"), "per-cell ER")
  # and the working temporaries are gone after a stop
  expect_false(any(c("share_taxon", "share_cat_cell") %in% DBI::dbListTables(con)))
})

test_that("the pooled denominator sums value-area with no per-species normalization", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "pooled")
  # category A in PA-A: endemic 10 cells x 1.0 x w 0.5 = 5; wide 1 cell x 1.0 x w 1 = 1 -> 6 km2-equivalents
  expect_equal(zrow(r, 1, "A")$share, 6)
  # the nation: endemic 5 + wide 1 + wide2 1 + dup 2 + hi 2 + low 0.01 = 11.01
  expect_equal(zrow(r, 7, "A")$share, 11.01)
  expect_equal(zrow(r, 1, "A")$v10, 100 * (6 / 11.01) / (10 / A_USA))
  expect_true(is.na(zrow(r, 1, "A")$pct_world))   # no "percent of the world" without a global denominator
  expect_equal(zrow(r, 1, "A")$pct_nation, 100 * 6 / 11.01)
})

test_that("primprod_v10 is 100 x the zone's area-weighted mean over the national mean, with its own floor", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # npp 10 in cells 1-10 (PA-A), 30 in cells 21-40 (PA-C), nothing elsewhere
  DBI::dbWriteTable(con, "primprod_cell", data.frame(cell_id = c(1:10, 21:40), npp = c(rep(10, 10), rep(30, 20))))
  r <- share_run(con, cats = CATS, denom = "global")
  pp <- r$primprod
  mean_usa <- (10 * 10 + 30 * 20) / A_USA
  expect_equal(pp$mean_usa[1], mean_usa)
  expect_equal(pp$v10[pp$zone_seq == 1], 100 * 10 / mean_usa)
  expect_equal(pp$v10[pp$zone_seq == 3], 100 * 30 / mean_usa)
  expect_equal(pp$coverage[pp$zone_seq == 1], 1)
  expect_equal(pp$coverage[pp$zone_seq == 2], 0)
  expect_true(is.na(pp$v10[pp$zone_seq == 2]))     # PA-B: no productivity cell -> not reportable
  # the composite takes primprod as one more equal-weight component where reportable
  comp <- r$composite
  expect_equal(comp$n_components[comp$zone_seq == 2], 2L)   # A (wide2, dup, low) + C; primprod skipped, never 0
  expect_true(comp$n_components[comp$zone_seq == 1] >= 6L)  # A, B, C, D, turtle, F, G, primprod
})

test_that("the national reference is the study area, not a zone row; scored cells outside it are refused", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r <- share_run(con, cats = CATS, denom = "global")
  expect_equal(r$nation$area_usa, rep(A_USA, nrow(r$nation)))
  expect_equal(r$nation$n_cells, rep(100L, nrow(r$nation)))
  # category C (uniform, er 20): the nation holds w * f_us / 100 = 0.2 of it
  expect_equal(r$nation$share_usa[r$nation$sp_cat == "C"], 0.2)
  # deleting the USA zone row changes nothing: the reference does not come from `zone`
  DBI::dbExecute(con, "DELETE FROM zone WHERE val = 'USA'")
  r2 <- share_run(con, cats = CATS, denom = "global")
  expect_equal(zrow(r2, 1, "B")$v10, zrow(r, 1, "B")$v10)
  # a nation that misses scored cells (the v8 subregion pseudo-zone case) is a hard stop ...
  expect_error(share_run(con, cats = CATS, nation_sql = "SELECT cell_id, 100.0 AS pct_covered FROM cell WHERE in_usa AND cell_id > 2"),
               "outside the national reference")
  # ... and so is an empty one
  expect_error(share_run(con, cats = CATS, nation_sql = "SELECT cell_id, 100.0 AS pct_covered FROM cell WHERE FALSE"), "no cell")
  expect_error(share_run(con, cats = CATS, denom = "world"), "arg")
  expect_error(share_run(con, cats = CATS, kappa = 1.5))
})

test_that("a bad denominator is refused under global: unresolved source, missing total, f_us > 1, duplicate rows", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "UPDATE taxon_denom SET source = 'national_only_unresolved' WHERE ms_merge_key = 'nat'")
  expect_error(share_run(con, cats = CATS, denom = "global"), "accepted source")
  expect_no_error(share_run(con, cats = CATS, denom = "usa"))          # the usa denominator never divides by it
  DBI::dbExecute(con, "UPDATE taxon_denom SET source = 'fallback:literature' WHERE ms_merge_key = 'nat'")
  DBI::dbExecute(con, "UPDATE taxon_denom SET total_global = NULL WHERE ms_merge_key = 'endemic'")
  expect_error(share_run(con, cats = CATS, denom = "global"), "positive global denominator")
  DBI::dbExecute(con, "UPDATE taxon_denom SET total_global = 5 WHERE ms_merge_key = 'endemic'")   # US total is 10
  expect_error(share_run(con, cats = CATS, denom = "global"), "f_us > 1")
  DBI::dbExecute(con, "UPDATE taxon_denom SET total_global = 10 WHERE ms_merge_key = 'endemic'")
  DBI::dbExecute(con, "INSERT INTO taxon_denom SELECT * FROM taxon_denom WHERE ms_merge_key = 'endemic'")
  expect_error(share_run(con, cats = CATS, denom = "global"), "more than once")
})

test_that("the heavy zone x species pass is built once and reused across denominators; temporaries are dropped", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  r1 <- share_run(con, cats = CATS, denom = "global")
  tabs <- DBI::dbListTables(con)
  expect_true(all(c("share_base", "share_base_sig") %in% tabs))
  expect_false(any(c("share_taxon", "share_cat_cell", "share_zone_area", "share_zone_taxon") %in% tabs))
  # a later denominator reuses it: poison the base and the reuse shows
  DBI::dbExecute(con, "UPDATE share_base SET num = num * 2 WHERE ms_merge_key = 'endemic'")
  r2 <- share_run(con, cats = CATS, denom = "usa")
  expect_equal(ztax(r2, 1, "endemic")$share_usa, 2)
  # a changed argument (floor_strip) rebuilds it; reuse_base = FALSE rebuilds it too
  r3 <- share_run(con, cats = CATS, denom = "usa", floor_strip = TRUE)
  expect_equal(ztax(r3, 1, "endemic")$share_usa, 1)
  DBI::dbExecute(con, "UPDATE share_base SET num = num * 2 WHERE ms_merge_key = 'endemic'")
  r4 <- share_run(con, cats = CATS, denom = "usa", floor_strip = TRUE, reuse_base = FALSE)
  expect_equal(ztax(r4, 1, "endemic")$share_usa, 1)
})

test_that("denom_sql: land included, merged-global > AquaMaps, outside + inside, inside-only, fallback, flags, strip", {
  con <- share_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- DBI::dbGetQuery(con, denom_sql(nc = NC))
  rownames(d) <- d$ms_merge_key
  expect_equal(d["endemic", "total_global"], 10);      expect_equal(d["endemic", "source"], "merged_global")  # not its AquaMaps 20
  expect_equal(d["bird",    "total_global"], 11);      expect_equal(d["bird", "n_cells_global"], 11L)         # 10 LAND cells counted
  expect_equal(d["bird",    "n_cells_no_area"], 0L)
  expect_equal(d["wide",    "total_global"], 99 + 1);  expect_equal(d["wide",  "source"], "aquamaps")
  expect_equal(d["wide2",   "total_global"], 199 + 1); expect_equal(d["wide2", "source"], "aquamaps")
  expect_equal(d["dup",     "total_global"], 98 + 2);  expect_equal(d["dup", "total_usa"], 2)                # two models, no dedup
  expect_equal(d["hi",      "total_global"], 2);       expect_equal(d["hi", "source"], "aquamaps")            # nothing abroad: f_us = 1
  expect_equal(d["nat",     "total_global"], 10 / 0.25); expect_equal(d["nat", "source"], "fallback:literature")
  expect_equal(d["turtle",  "total_global"], 1.31);    expect_equal(d["turtle", "total_global_strip"], 1.30)
  expect_equal(d["turtle",  "total_usa"], 0.81);       expect_equal(d["turtle", "total_usa_strip"], 0.80)
  expect_equal(d["dps", "source"], "national_only_unresolved")   # a cell-mode taxon resolves like any other (settled 2026-10-30): this one has only a national dataset
  expect_equal(d["low",     "total_global_strip"], 0.01)   # the strip is for premultiplied taxa only
  expect_equal(d$total_usa, vapply(d$ms_merge_key, function(k) DBI::dbGetQuery(con, sprintf(
    "SELECT SUM(val / 100.0 * area_km2) s FROM model_cell JOIN cell USING (cell_id) WHERE mdl_key = '%s'", k))$s, 0),
    ignore_attr = TRUE)
  # a national-only taxon without a fallback row is flagged, not scored as an endemic
  DBI::dbExecute(con, "DELETE FROM fallback WHERE ms_merge_key = 'nat'")
  d2 <- DBI::dbGetQuery(con, denom_sql(nc = NC)); rownames(d2) <- d2$ms_merge_key
  expect_equal(d2["nat", "source"], "national_only_unresolved")
  # the usa total never exceeds the global one on this fixture (the notebook asserts it on the real one)
  ok <- !is.na(d$total_global)
  expect_true(all(d$total_usa[ok] <= d$total_global[ok] + 1e-12))
  # and the share tests' hand-built denominators agree with denom_sql() wherever it is accepted
  hand <- DBI::dbGetQuery(con, "SELECT ms_merge_key, total_global FROM taxon_denom WHERE ms_merge_key NOT IN ('dps', 'dps20')")
  expect_equal(d[hand$ms_merge_key, "total_global"], hand$total_global, ignore_attr = TRUE)
  # a row of the grid with no ocean cell gives no area: empty the land cells' row of ocean cells
  DBI::dbExecute(con, "DELETE FROM cell WHERE cell_id BETWEEN 201 AND 300")
  d3 <- DBI::dbGetQuery(con, denom_sql(nc = NC)); rownames(d3) <- d3$ms_merge_key
  expect_equal(d3["bird", "n_cells_no_area"], 10L); expect_equal(d3["bird", "total_global"], 1)
})
