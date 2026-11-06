# Guards the mass-preserving calibration of a folded-in density survey (density_cal_factors_sql /
# density_calibrate_sql): the survey redistributes the mass the replaced surface gave the domain, never
# re-weights the domain against the rest of the range.

skip_if_not_installed("duckdb")

cal_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  # three density taxa, domain = cells 1-4:
  #   T_am:    AquaMaps covered cells 1-3 at 60, 60, 60 (cell 4 had no prior value); survey 30, 30, 30, 90
  #            -> f = mean(60,60,60) / mean(30,30,30) = 2; cell 4 keeps the pattern at the same scale (90 -> 100 cap)
  #   T_range: a range (er 20) covered cells 1-4; survey 40, 40, 40, 40 -> f = 0.5 -> 20 everywhere
  #   T_new:   nothing covered the domain before; survey 50, 50 -> f = 1, calibrated FALSE
  DBI::dbWriteTable(con, "dens_vals", data.frame(
    ms_merge_key = c(rep("T_am", 4), rep("T_range", 4), rep("T_new", 2)),
    cell_id = c(1:4, 1:4, 1:2),
    val = c(30, 30, 30, 90, 40, 40, 40, 40, 50, 50)))
  DBI::dbWriteTable(con, "dens_replaced", data.frame(
    ms_merge_key = c(rep("T_am", 3), rep("T_range", 4)),
    cell_id = c(1:3, 1:4),
    val = c(60, 60, 60, 20, 20, 20, 20)))
  DBI::dbExecute(con, paste("CREATE TABLE dens_cal AS", density_cal_factors_sql()))
  con
}

test_that("the factor is the ratio of means over the cells that had a prior value; no prior -> 1 and flagged", {
  con <- cal_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  f <- DBI::dbGetQuery(con, "SELECT * FROM dens_cal ORDER BY ms_merge_key")
  rownames(f) <- f$ms_merge_key
  expect_equal(f["T_am", "f"], 2); expect_equal(f["T_am", "n_prior"], 3); expect_true(f["T_am", "calibrated"])
  expect_equal(f["T_range", "f"], 0.5); expect_equal(f["T_range", "mean_prior"], 20)
  expect_equal(f["T_new", "f"], 1); expect_false(f["T_new", "calibrated"]); expect_equal(f["T_new", "n_prior"], 0)
})

test_that("applying the factors scales only the density rows, caps at 100, passes other datasets through", {
  con <- cal_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mc <- data.frame(
    mkey_id = 7L,
    ms_merge_key = c(rep("T_am", 5), rep("T_range", 4), rep("T_new", 2), "T_other"),
    ds_key  = c("nc", "nc", "nc", "nc", "rng_iucn",  "gm", "gm", "gm", "gm",  "cce", "cce",  "am"),
    cell_id = c(1:4, 9L,  1:4,  1:2, 1L),
    val     = c(30, 30, 30, 90, 20,  40, 40, 40, 40,  50, 50,  77))
  DBI::dbWriteTable(con, "mc", mc)
  out <- DBI::dbGetQuery(con, paste(density_calibrate_sql("SELECT * FROM mc", c("gm", "nc", "cce")), "ORDER BY ms_merge_key, ds_key, cell_id"))
  g <- function(k, ds) out$val[out$ms_merge_key == k & out$ds_key == ds]
  expect_equal(g("T_am", "nc"), c(60, 60, 60, 100))         # x2, cell 4 capped
  expect_equal(g("T_am", "rng_iucn"), 20)                    # a range row is untouched
  expect_equal(g("T_range", "gm"), c(20, 20, 20, 20))        # x0.5: the domain keeps the range's mass
  expect_equal(g("T_new", "cce"), c(50, 50))                 # f = 1
  expect_equal(g("T_other", "am"), 77)                       # not a density dataset
  expect_equal(names(out), c("mkey_id", "ms_merge_key", "ds_key", "cell_id", "val"))   # other columns pass through
  expect_true(all(out$mkey_id == 7L))
  # mass check: T_am's domain total over the prior cells equals the prior total (3 x 60)
  expect_equal(sum(g("T_am", "nc")[1:3]), 180)
})

test_that("bad arguments error", {
  expect_error(density_calibrate_sql("SELECT 1", character(0)))
  expect_error(density_cal_factors_sql(""))
})

test_that("density_prior_sql(): 'value' = the control's max(er, suit); 'suit' = suitability where modeled, er elsewhere", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # taxon T (er 100): cell 1 am 30 + range; cell 2 range only; cell 3 am 50 only; cell 4 is survey-only (not in src);
  # cell 9 has a prior but is outside the survey domain (ignored); a density row (nc) at cell 1 must be excluded
  DBI::dbWriteTable(con, "src", data.frame(
    mdl_key = c("am|1", "rng_iucn|1", "rng_iucn|1", "am|1", "am|1", "nc|9"),
    cell_id = c(1L, 1L, 2L, 3L, 9L, 1L), val = c(30, 100, 100, 50, 70, 99)))
  DBI::dbWriteTable(con, "taxon_model", data.frame(mdl_key = c("am|1", "rng_iucn|1", "nc|9"), ms_merge_key = "T"))
  DBI::dbWriteTable(con, "taxon", data.frame(ms_merge_key = "T", er_score = 100))
  DBI::dbWriteTable(con, "dens_dom", data.frame(ms_merge_key = "T", cell_id = 1:4))
  get <- function(mode) {
    r <- DBI::dbGetQuery(con, paste(density_prior_sql("src", c("am", "ax"), c("gm", "nc", "cce"), mode), "ORDER BY cell_id"))
    setNames(r$val, r$cell_id)
  }
  expect_equal(get("value"), c(`1` = 100, `2` = 100, `3` = 50))
  expect_equal(get("suit"),  c(`1` = 30,  `2` = 100, `3` = 50))
  expect_error(density_prior_sql("src", "am", "nc", mode = "mean"))
})
