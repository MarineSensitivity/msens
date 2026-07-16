# tests for the pure v7<->v8 equivalence helpers in R/validate.R

test_that("score_delta joins on key and computes b - a", {
  a <- data.frame(programarea_key = c("CGM", "WGM", "MAT"), score = c(0.50, 0.20, 0.80))
  b <- data.frame(programarea_key = c("MAT", "CGM", "WGM"), score = c(0.83, 0.52, 0.20))
  d <- score_delta(a, b, labels = c("v7", "v8"))

  expect_setequal(names(d), c("programarea_key", "score_v7", "score_v8", "delta"))
  expect_equal(nrow(d), 3)
  # delta = v8 - v7, and rows sorted by descending abs(delta) → MAT (0.03) first
  expect_equal(d$programarea_key[1], "MAT")
  cgm <- d[d$programarea_key == "CGM", ]
  expect_equal(cgm$delta, 0.02, tolerance = 1e-9)
  wgm <- d[d$programarea_key == "WGM", ]
  expect_equal(wgm$delta, 0.00, tolerance = 1e-9)
})

test_that("score_delta keeps only shared keys (inner join)", {
  a <- data.frame(programarea_key = c("CGM", "WGM"),  score = c(0.5, 0.2))
  b <- data.frame(programarea_key = c("CGM", "EXTRA"), score = c(0.5, 0.9))
  d <- score_delta(a, b)
  expect_equal(nrow(d), 1)
  expect_equal(d$programarea_key, "CGM")
})

test_that("score_delta_summary reports n, mean_abs, max_abs, rmse", {
  d <- data.frame(delta = c(0.02, -0.04, 0.00))
  s <- score_delta_summary(d)
  expect_equal(s$n, 3)
  expect_equal(s$mean_abs, mean(c(0.02, 0.04, 0.00)), tolerance = 1e-9)
  expect_equal(s$max_abs, 0.04, tolerance = 1e-9)
  expect_equal(s$rmse, sqrt(mean(c(0.02, -0.04, 0.00)^2)), tolerance = 1e-9)
})

test_that("assert_within_tolerance passes inside and errors outside tolerance", {
  ok  <- data.frame(delta = c(0.01, -0.015, 0.00))
  bad <- data.frame(delta = c(0.01, -0.09,  0.00))
  expect_invisible(assert_within_tolerance(ok, mean_tol = 0.02, max_tol = 0.05))
  s <- suppressMessages(assert_within_tolerance(ok, mean_tol = 0.02, max_tol = 0.05))
  expect_lte(s$max_abs, 0.05)
  expect_error(assert_within_tolerance(bad, mean_tol = 0.02, max_tol = 0.05),
               "equivalence FAILED")
})

test_that("rarity_class bins range sizes into an ordered factor", {
  rc <- rarity_class(c(500, 5e4, 5e5, 5e6))
  expect_s3_class(rc, "ordered")
  expect_equal(as.character(rc), c("very_rare", "rare", "common", "widespread"))
  expect_error(rarity_class(1, breaks = c(1, 2), labels = c("a", "b")))  # length mismatch
})

test_that("mass_conservation flags ratios inside/outside tolerance", {
  expect_true(mass_conservation(100, 105, tol = 0.1)$within)
  expect_false(mass_conservation(100, 130, tol = 0.1)$within)
  expect_equal(mass_conservation(100, 90)$ratio, 0.9, tolerance = 1e-9)
})

# regression: pra_score_delta must survive the v8 `value`->`val` reserved-word rename.
# v7 sdm.duckdb uses `zone.value`/`zone_metric.value`; v8 uses `val`. A hard-coded
# `z.value` errored on v8 with "Table z does not have a column named value" — so the
# score/key column is resolved per connection. Build one DB of each schema + compare.
test_that("pra_score_delta adapts to val vs value schema (v8<->v7 rename)", {
  skip_if_not_installed("duckdb")
  metric <- "m1"

  # write a 2-Program-Area sdm schema using either `val` or `value` as the scalar column
  make_db <- function(col, scores) {
    path <- tempfile(fileext = ".duckdb")
    con  <- DBI::dbConnect(duckdb::duckdb(), path)
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE zone (zone_seq INT, tbl VARCHAR, fld VARCHAR, %s VARCHAR)", col))
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone VALUES (1,'z','programarea_key','ALA'),(2,'z','programarea_key','GEO')"))
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE zone_metric (zone_seq INT, metric_seq INT, %s DOUBLE)", col))
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone_metric VALUES (1,1,%s),(2,1,%s)", scores[1], scores[2]))
    DBI::dbExecute(con, "CREATE TABLE metric (metric_seq INT, metric_key VARCHAR)")
    DBI::dbExecute(con, sprintf("INSERT INTO metric VALUES (1,'%s')", metric))
    con
  }

  con_v7 <- make_db("value", c(0.50, 0.80))   # v7-style
  con_v8 <- make_db("val",   c(0.55, 0.80))   # v8-style
  on.exit({ DBI::dbDisconnect(con_v7, shutdown = TRUE)
            DBI::dbDisconnect(con_v8, shutdown = TRUE) }, add = TRUE)

  d <- pra_score_delta(con_v7, con_v8, metric_key = metric, labels = c("v7", "v8"))
  expect_setequal(names(d), c("programarea_key", "score_v7", "score_v8", "delta"))
  expect_setequal(d$programarea_key, c("ALA", "GEO"))
  ala <- d[d$programarea_key == "ALA", ]
  expect_equal(ala$delta, 0.05, tolerance = 1e-9)   # 0.55 - 0.50
  geo <- d[d$programarea_key == "GEO", ]
  expect_equal(geo$delta, 0.00, tolerance = 1e-9)   # 0.80 - 0.80

  # and a v8<->v8 comparison (both `val`) must also work, not just v7<->v8
  con_v8b <- make_db("val", c(0.60, 0.80))
  on.exit(DBI::dbDisconnect(con_v8b, shutdown = TRUE), add = TRUE)
  d2 <- pra_score_delta(con_v8, con_v8b, metric_key = metric, labels = c("a", "b"))
  expect_equal(d2[d2$programarea_key == "ALA", ]$delta, 0.05, tolerance = 1e-9)
})
