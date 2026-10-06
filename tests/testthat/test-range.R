# Guards the range-constraint rule (R/range.R): which cells of a model surface stay.
# The rule decides whether a species scores in a region at all (turtles in Alaska, the right whale
# in the Gulf), so every branch gets an exact assertion on a synthetic fixture.
#
# SQL fixture, threshold 50, unit = cell unless stated:
#
#   taxon "whale"
#   region ATL   cells 1-10    1-8  val 80 spread 5            -> present
#                              9    val 40                     -> drop_threshold (holds a record)
#                              10   val 52 spread 5            -> drop_uncertain (52 - 5 < 50)
#          searched 1-8, records in 1-6 (+ the record in cell 9, which the model does not call present)
#   region GULF  cells 11-20   11-16 val 70 spread 2           -> present, region DROPPED by the test
#                              17   val 70, extrapolated       -> drop_extrapolated (before region)
#                              18   val 30, critical habitat   -> kept_ch
#                              19   val 70, critical habitat   -> kept_ch (the region is dropped)
#                              20   val NULL                   -> drop_threshold
#          searched 11-16 + 19, ONE record (cell 11): the vagrant
#   no region    cells 21-22   val 90; 22 has NULL spread / extrap -> kept (tests 1 and 3 only)
#          searched 21; a record in 22, which the effort table does NOT list (a record is a search)
#
#   counts   ATL  n_cells 8  n_searched 8  n_occupied 6  n_records 7
#            GULF n_cells 7  n_searched 7  n_occupied 1  n_records 1
#            all  n_searched 17 (1-8, 11-16, 19, 21, 22) n_occupied 8
#   test     GULF p_core (8-1)/(17-7) = 0.7, P(X <= 1 | 7, 0.7) = 0.3^7 + 7(0.7)(0.3^6) = 0.0038 -> dropped,
#                 one record -> "documented occurrence only"
#            ATL  p_core (8-6)/(17-8) = 2/9 -> n_min 12 > 8 searched -> unsearched (kept, flagged)
#
#   taxon "ghost" has cells 1-2 (2 is critical habitat) and NO threshold row.

skip_if_not_installed("duckdb")

range_fixture_con <- function(block_units = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb())
  whale <- data.frame(
    taxon   = "whale",
    cell_id = 1:22,
    val     = c(rep(80, 8), 40, 52, rep(70, 6), 70, 30, 70, NA, 90, 90),
    spread  = c(rep(5, 8),   5,  5, rep(2, 6),   2,  2,  2,  2,  1, NA),
    extrap  = c(rep(FALSE, 16), TRUE, rep(FALSE, 4), NA),
    is_ch   = c(rep(FALSE, 17), TRUE, TRUE, rep(FALSE, 3)))
  ghost <- data.frame(
    taxon = "ghost", cell_id = 1:2, val = c(90, 90), spread = 0, extrap = FALSE, is_ch = c(FALSE, TRUE))
  cell <- rbind(whale, ghost)
  # block units: ATL's present cells fall in two blocks, everything else stays its own unit
  cell$unit_id <- if (block_units) ifelse(cell$cell_id <= 4, 100L, ifelse(cell$cell_id <= 8, 101L, cell$cell_id)) else cell$cell_id
  unit_of <- function(id) cell$unit_id[match(id, cell$cell_id)]

  DBI::dbWriteTable(con, "og_cell", cell)
  DBI::dbWriteTable(con, "og_thr", data.frame(taxon = "whale", thr = 50))
  DBI::dbWriteTable(con, "og_region", data.frame(
    region = c(rep("ATL", 10), rep("GULF", 10)), cell_id = 1:20))
  DBI::dbWriteTable(con, "og_effort", data.frame(taxon = "whale", unit_id = unit_of(c(1:8, 11:16, 19, 21))))
  DBI::dbWriteTable(con, "og_occ",    data.frame(taxon = "whale", unit_id = unit_of(c(1:6, 9, 11, 22))))
  con
}

range_verdicts <- function(con) {
  n <- DBI::dbGetQuery(con, range_region_sql())
  cbind(n, region_effort_test(
    n$n_searched, n$n_occupied, n$n_searched_all, n$n_occupied_all, n_records = n$n_records))
}

# 1. threshold ----
test_that("sens_threshold keeps the stated share of presences and returns an observed value", {
  x <- seq(10, 100, 10)
  expect_equal(sens_threshold(x), 20)             # nine of ten are >= 20
  expect_equal(mean(x >= sens_threshold(x)), 0.9)
  expect_equal(sens_threshold(x, sens = 1), 10)   # sens 1 = minimum presence
  expect_equal(sens_threshold(x, sens = 0.5), 60)
  expect_equal(sens_threshold(c(x, NA)), 20)      # missing predictions are dropped, not counted
  expect_equal(sens_threshold(rev(x)), 20)        # order blind
})

test_that("sens_threshold reproduces the OBIS pipeline's P10 exactly, ties and odd lengths included", {
  obis_p10 <- function(predv) rev(sort(predv))[ceiling(length(predv) * 0.9)]
  set.seed(1)
  for (n in c(1, 7, 10, 289, 1160)) {
    x <- round(runif(n) * 100)
    expect_equal(sens_threshold(x), obis_p10(x))
  }
})

test_that("sens_threshold refuses input it cannot answer", {
  expect_error(sens_threshold(c(NA_real_, NA_real_)), "no non-missing")
  expect_error(sens_threshold(1:10, sens = 0), "sens")
  expect_error(sens_threshold(1:10, sens = 1.2), "sens")
})

test_that("confusion_at counts each quadrant, with >= on the threshold", {
  pred     <- c(90, 60, 50, 40, 80, 50, 20, 10)
  presence <- c( 1,  1,  1,  1,  0,  0,  0,  0)
  cm <- confusion_at(pred, presence, thr = 50)
  expect_equal(unlist(cm[c("tp", "fp", "tn", "fn")]), c(tp = 3, fp = 2, tn = 2, fn = 1))
  expect_equal(cm$sens, 3 / 4)
  expect_equal(cm$spec, 2 / 4)
  expect_equal(cm$precision, 3 / 5)
})

test_that("heldout_threshold pools the held-out presences; per-fold thresholds come from TRAINING rows", {
  # two folds. held-out presences: 10 values 10..100 -> pooled threshold 20
  cv <- data.frame(
    fold     = rep(1:2, each = 7),
    presence = rep(c(1, 1, 1, 1, 1, 0, 0), 2),
    pred     = c(10, 30, 50, 70, 90, 20, 60,   20, 40, 60, 80, 100, 10, 50))
  h <- heldout_threshold(cv)
  expect_equal(h$thr, 20)
  expect_equal(h$pooled$sens, 0.9)
  expect_equal(h$pooled$fp, 3)                    # background 20, 60, 50 are >= 20
  expect_equal(h$folds$thr_from, c("pooled", "pooled"))

  # the fold models score their own training presences higher: training P10 = 64 and 73
  cv_train <- data.frame(
    fold     = rep(1:2, each = 10),
    presence = 1,
    pred     = c(seq(60, 96, 4), seq(70, 97, 3)))
  ht <- heldout_threshold(cv, cv_train)
  expect_equal(ht$thr, 20)                        # the pooled held-out threshold does not move
  expect_equal(ht$folds$thr, c(64, 73))           # sens 0.9 of 10 training presences = 2nd lowest
  expect_equal(ht$folds$thr_from, c("train", "train"))
  # ... and deliver far less than 0.90 on presences the model never saw
  expect_equal(ht$folds$sens, c(2 / 5, 2 / 5))
  expect_equal(ht$folds$fp, c(0, 0))
})

# 2. region ----
test_that("region_effort_test: searched and empty is dropped; a record makes it 'documented occurrence only'", {
  # the rest of the range: 5000 searched, 500 with a record -> p_core 0.1, n_min 29
  r <- region_effort_test(
    n_searched = c(400, 400, 400), n_occupied = c(0, 2, 40),
    n_searched_all = 5000 + 400, n_occupied_all = 500 + c(0, 2, 40))
  expect_equal(r$p_core, rep(0.1, 3))
  expect_equal(r$n_min, rep(29, 3))               # 0.9^29 = 0.047 < 0.05 <= 0.9^28
  expect_equal(r$verdict, c("dropped", "dropped", "kept"))
  expect_equal(r$label, c(NA, "documented occurrence only", NA))
  expect_equal(r$p_value[1], 0.9^400)
})

test_that("region_effort_test: too few searched units is 'unsearched' (kept), never 'dropped'", {
  r <- region_effort_test(
    n_searched = c(28, 29), n_occupied = c(0, 0),
    n_searched_all = 5000 + c(28, 29), n_occupied_all = c(500, 500))
  expect_equal(r$verdict, c("unsearched", "dropped"))   # the boundary is n_min itself
  expect_true(is.na(r$label[1]))
})

test_that("region_effort_test: no precision to compare with means no verdict against the region", {
  # the region is the only searched part of the range (n_core = 0), or the rest has no record
  r <- region_effort_test(
    n_searched = c(50, 50), n_occupied = c(0, 0),
    n_searched_all = c(50, 5050), n_occupied_all = c(0, 0))
  expect_equal(r$n_min, c(Inf, Inf))
  expect_equal(r$verdict, c("unsearched", "unsearched"))
})

test_that("region_effort_test leaves the tested region out of the core precision", {
  # a stronghold: 900 of its 1000 searched units hold a record; elsewhere 10 of 1000.
  # pooled precision would be 0.455 and would not drop the weak region as strongly; what must
  # hold is that the stronghold is compared with the REST (0.01) and the rest with the stronghold
  r <- region_effort_test(
    n_searched = c(1000, 1000), n_occupied = c(900, 10),
    n_searched_all = 2000, n_occupied_all = 910)
  expect_equal(r$p_core, c(0.01, 0.9))
  expect_equal(r$verdict, c("kept", "dropped"))
  expect_equal(r$label, c(NA, "documented occurrence only"))
})

test_that("region_effort_test: scarcer is not absent -- a drop needs a material shortfall, not only a significant one", {
  # REGRESSION (published OBIS models, 0.5 degree blocks, megafauna effort): the US Pacific
  # for the loggerhead, 139 occupied of 978 searched against 23 % elsewhere. Significance alone
  # (ratio = 1) drops it at p = 4e-12; 14 % is well above a quarter of 23 %, so it is range.
  args <- list(n_searched = 978, n_occupied = 139, n_searched_all = 978 + 3000, n_occupied_all = 139 + 690)
  kept    <- do.call(region_effort_test, args)
  as_was  <- do.call(region_effort_test, c(args, ratio = 1))
  expect_equal(kept$p_core, 0.23)
  expect_lt(kept$p_value, 1e-9)                 # significant either way
  expect_equal(kept$rate, 139 / 978)
  expect_equal(kept$verdict, "kept")
  expect_true(is.na(kept$label))
  expect_equal(as_was$verdict, "dropped")
  # the boundary: rate must be BELOW ratio * p_core. 0.23 * 0.25 = 0.0575 -> 57 of 1000 drops, 58 stays
  edge <- region_effort_test(
    n_searched = c(1000, 1000), n_occupied = c(57, 58),
    n_searched_all = 1000 + 3000, n_occupied_all = c(57, 58) + 690)
  expect_equal(edge$verdict, c("dropped", "kept"))
})

test_that("region_effort_test rejects impossible counts", {
  expect_error(region_effort_test(10, 1, 100, 50, ratio = 0), "ratio")
  expect_error(region_effort_test(10, 1, 100, 50, ratio = 1.5), "ratio")
  expect_error(region_effort_test(10, 11, 100, 50))
  expect_error(region_effort_test(10, 1, 5, 1))
})

test_that("range_region_sql counts present, searched and occupied units per region", {
  con <- range_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  n <- DBI::dbGetQuery(con, range_region_sql())
  w <- n[n$taxon == "whale", ]
  expect_equal(w$region, c("ATL", "GULF"))
  expect_equal(w$n_cells,    c(8, 7))
  expect_equal(w$n_searched, c(8, 7))
  expect_equal(w$n_occupied, c(6, 1))
  expect_equal(w$n_records,  c(7, 1))    # ATL: the record in cell 9 counts here, not in n_occupied
  expect_equal(w$n_searched_all, c(17, 17))   # 22 is searched because it holds a record
  expect_equal(w$n_occupied_all, c(8, 8))
  # a taxon without a threshold has no present cell: zeros, not a missing row
  g <- n[n$taxon == "ghost", ]
  expect_equal(g$region, "ATL")
  expect_equal(c(g$n_cells, g$n_searched, g$n_occupied, g$n_records), c(0, 0, 0, 0))
})

test_that("range_region_sql counts distinct UNITS, so a block is one trial", {
  con <- range_fixture_con(block_units = TRUE); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  n <- DBI::dbGetQuery(con, range_region_sql())
  atl <- n[n$taxon == "whale" & n$region == "ATL", ]
  expect_equal(atl$n_cells, 8)           # cells stay cells
  expect_equal(atl$n_searched, 2)        # 8 cells, 2 blocks
  expect_equal(atl$n_occupied, 2)
  expect_equal(atl$n_searched_all, 11)   # 2 blocks + 11-16, 19, 21, 22
})

# 3. the rule ----
test_that("range_constrain_sql gives every cell the first test it fails, and keeps the rest", {
  con <- range_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  v <- range_verdicts(con)
  expect_equal(v$verdict[v$taxon == "whale"], c("unsearched", "dropped"))
  expect_equal(v$p_value[v$taxon == "whale" & v$region == "GULF"], 0.3^7 + 7 * 0.7 * 0.3^6)
  DBI::dbWriteTable(con, "og_verdict", v[, c("taxon", "region", "verdict")])

  x <- DBI::dbGetQuery(con, paste(range_constrain_sql(), "ORDER BY taxon, cell_id"))
  w <- x[x$taxon == "whale", ]
  expect_equal(nrow(w), 22)                                  # one row per input cell, none duplicated
  expect_equal(w$status, c(
    rep("kept", 8), "drop_threshold", "drop_uncertain",      # ATL
    rep("drop_region", 6), "drop_extrapolated", "kept_ch", "kept_ch", "drop_threshold",  # GULF
    "kept", "kept"))                                         # no region; NULL spread / extrap
  expect_equal(w$keep, w$status %in% c("kept", "kept_ch"))
  expect_equal(sum(w$keep), 12)
})

test_that("critical habitat is never trimmed, whichever test the cell fails", {
  con <- range_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "og_verdict", range_verdicts(con)[, c("taxon", "region", "verdict")])
  # make EVERY whale cell critical habitat: nothing may be dropped
  DBI::dbExecute(con, "UPDATE og_cell SET is_ch = TRUE WHERE taxon = 'whale'")
  x <- DBI::dbGetQuery(con, range_constrain_sql())
  expect_true(all(x$keep[x$taxon == "whale"]))
  expect_equal(sum(x$status == "kept_ch" & x$taxon == "whale"), 12)  # the twelve that failed a test
})

test_that("a taxon without a threshold is dropped out loud, never silently kept or silently lost", {
  con <- range_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "og_verdict", range_verdicts(con)[, c("taxon", "region", "verdict")])
  x <- DBI::dbGetQuery(con, paste(range_constrain_sql(), "ORDER BY taxon, cell_id"))
  g <- x[x$taxon == "ghost", ]
  expect_equal(g$status, c("drop_no_threshold", "kept_ch"))
})

test_that("a cell in two regions is dropped if either is, and is returned once", {
  con <- range_fixture_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "og_verdict", range_verdicts(con)[, c("taxon", "region", "verdict")])
  # cell 1 (ATL, kept) is also listed under GULF (dropped); cell 11 sits in TWO dropped regions
  DBI::dbExecute(con, "INSERT INTO og_region VALUES ('GULF', 1), ('BOX', 11)")
  DBI::dbExecute(con, "INSERT INTO og_verdict VALUES ('whale', 'BOX', 'dropped')")
  x <- DBI::dbGetQuery(con, paste(range_constrain_sql(), "WHERE taxon = 'whale' ORDER BY cell_id"))
  expect_equal(nrow(x), 22)
  expect_equal(x$status[x$cell_id == 1], "drop_region")
  expect_equal(x$status[x$cell_id == 2], "kept")
  expect_equal(sum(x$cell_id == 11), 1)
})

# regression cases (permanent) ----
test_that("REGRESSION right whale in the Gulf: records in a searched region do not make it range", {
  # four vagrant records among 300 searched Gulf units; the Atlantic shows 1 record in 5 searched
  r <- region_effort_test(
    n_searched = c(300, 2000), n_occupied = c(4, 400),
    n_searched_all = 2300, n_occupied_all = 404, n_records = c(4, 420))
  expect_equal(r$verdict, c("dropped", "kept"))
  expect_equal(r$label, c("documented occurrence only", NA))
})

test_that("REGRESSION turtles in Alaska: predicted, barely searched -> kept and flagged, not dropped", {
  # the model calls 64,000 Alaska cells present; with turtle records as the effort layer only a
  # handful were ever 'searched'. Empty of observers is not empty of turtles: the verdict must be
  # 'unsearched', and must become 'dropped' once a real effort layer shows thousands searched
  few  <- region_effort_test(n_searched = 3,    n_occupied = 0, n_searched_all = 9003,  n_occupied_all = 900)
  many <- region_effort_test(n_searched = 3000, n_occupied = 0, n_searched_all = 12000, n_occupied_all = 900)
  expect_equal(few$verdict,  "unsearched")
  expect_equal(many$verdict, "dropped")
})

test_that("REGRESSION leatherback in Alaska: one record in a dropped region is a documented occurrence", {
  r <- region_effort_test(
    n_searched = 500, n_occupied = 1, n_searched_all = 20500, n_occupied_all = 4001, n_records = 1)
  expect_equal(r$verdict, "dropped")
  expect_equal(r$label, "documented occurrence only")
})

# hull ----
test_that("range_hull is the planar hull of the points", {
  skip_if_not_installed("sf")
  h <- range_hull(c(-90, -80, -80, -90, -85), c(20, 20, 30, 30, 25))
  expect_equal(as.numeric(sf::st_bbox(h)), c(-90, 20, -80, 30))
  expect_equal(sum(as.numeric(sf::st_area(sf::st_set_crs(h, NA)))), 100)
})

test_that("range_hull crosses the antimeridian the short way and comes back cut at 180", {
  skip_if_not_installed("sf")
  h <- range_hull(c(170, 175, -175, -170, 170, -170), c(-10, 10, 10, -10, 10, 10))
  # 20 x 20 degrees, not the 340-degree band the -180..180 frame would draw
  expect_equal(sum(as.numeric(sf::st_area(sf::st_set_crs(h, NA)))), 400)
  expect_equal(as.numeric(sf::st_bbox(h))[c(1, 3)], c(-180, 180))
  pt <- function(x, y) sf::st_sfc(sf::st_point(c(x, y)))
  inside <- function(x, y) lengths(sf::st_intersects(pt(x, y), sf::st_set_crs(h, NA))) > 0
  expect_true(inside(179, 0))
  expect_true(inside(-179, 0))
  expect_false(inside(0, 0))
})

test_that("range_hull of fewer than three points is empty, not an error", {
  skip_if_not_installed("sf")
  expect_true(all(sf::st_is_empty(range_hull(c(1, 2), c(1, 2)))))
})

test_that("the threshold is inclusive: a cell exactly at it, or exactly one spread above it, stays", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "og_cell", data.frame(
    taxon = "t", cell_id = 1:4, unit_id = 1:4, val = c(50, 55, 49.9, 54.9), spread = c(0, 5, 0, 5),
    extrap = FALSE, is_ch = FALSE))
  DBI::dbWriteTable(con, "og_thr", data.frame(taxon = "t", thr = 50))
  DBI::dbWriteTable(con, "og_region", data.frame(region = "R", cell_id = 1:4))
  DBI::dbWriteTable(con, "og_verdict", data.frame(taxon = "t", region = "R", verdict = "unsearched"))
  DBI::dbWriteTable(con, "og_effort", data.frame(taxon = character(), unit_id = integer()))
  DBI::dbWriteTable(con, "og_occ",    data.frame(taxon = character(), unit_id = integer()))
  x <- DBI::dbGetQuery(con, paste(range_constrain_sql(), "ORDER BY cell_id"))
  expect_equal(x$status, c("kept", "kept", "drop_threshold", "drop_uncertain"))
  # ... and the regional counts see the same two cells as present
  expect_equal(DBI::dbGetQuery(con, range_region_sql())$n_cells, 2)
})
