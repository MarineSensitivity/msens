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

# --- pinning the comparison to one spatial unit --------------------------------

mk_scored <- function(env = parent.frame(), with_zs = TRUE, zsk = "programarea_2026-01",
                      scores = c(A = 50, B = 60)) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  zs <- if (with_zs) ", zone_set_key VARCHAR" else ""
  DBI::dbExecute(con, sprintf("CREATE TABLE zone (zone_seq INTEGER, fld VARCHAR, val VARCHAR%s)", zs))
  for (i in seq_along(scores)) {
    v <- if (with_zs) sprintf("(%d,'programarea_key','%s','%s')", i, names(scores)[i], zsk)
         else         sprintf("(%d,'programarea_key','%s')",      i, names(scores)[i])
    DBI::dbExecute(con, paste("INSERT INTO zone VALUES", v))
  }
  DBI::dbExecute(con, "CREATE TABLE metric (metric_seq INTEGER, metric_key VARCHAR)")
  DBI::dbExecute(con, sprintf("INSERT INTO metric VALUES (1,'%s')", METRIC_SCORE_DEFAULT))
  DBI::dbExecute(con, "CREATE TABLE zone_metric (zone_seq INTEGER, metric_seq INTEGER, val DOUBLE)")
  for (i in seq_along(scores))
    DBI::dbExecute(con, sprintf("INSERT INTO zone_metric VALUES (%d,1,%f)", i, scores[[i]]))
  con
}

test_that("pra_score_delta pins to a zone set when asked", {
  a <- mk_scored(); b <- mk_scored(scores = c(A = 50, B = 60))
  d <- pra_score_delta(a, b, zone_set_key = "programarea_2026-01")
  expect_equal(nrow(d), 2L)
  expect_true(all(abs(d[[grep("^d", names(d), value = TRUE)[1]]]) < 1e-9))
})

test_that("a database predating zone_set_key still compares, via the fld filter", {
  # v1-v7 have no such column; pinning must degrade, not return nothing
  old <- mk_scored(with_zs = FALSE)
  new <- mk_scored()
  expect_equal(nrow(pra_score_delta(old, new, zone_set_key = "programarea_2026-01")), 2L)
})

test_that("naming a zone set the release does not have is an error, not an empty join", {
  a <- mk_scored(); b <- mk_scored()
  expect_error(pra_score_delta(a, b, zone_set_key = "programarea_1999-01"),
               "no zones for zone_set_key")
})

test_that("without zone_set_key the fld filter is used, as before", {
  expect_equal(nrow(pra_score_delta(mk_scored(), mk_scored())), 2L)
})

test_that("identical labels are refused with a message about labels", {
  # they collide into one column and otherwise surface as an opaque rlang
  # data-pronoun error several frames from the cause
  d <- data.frame(programarea_key = "A", score = 1)
  expect_error(score_delta(d, d, labels = c("v8", "v8")), "`labels` must differ")
  expect_no_error(score_delta(d, d, labels = c("v8a", "v8b")))
})

# zone_score_delta() -- the per-release "what moved" comparison. The rule that
# matters is that it never compares things that only LOOK alike (see the v7 `FULL`
# vs v8 `AT` subregion trap in the roxygen).
test_that("zone_score_delta() compares only shared zones and metrics, and says what it dropped", {
  a <- data.frame(zone_key = c("GAA","GAB","FULL"), metric_key = "extrisk_fish",
                  score = c(10, 20, 30))
  b <- data.frame(zone_key = c("GAA","GAB","AT"),   metric_key = "extrisk_fish",
                  score = c(12, 20, 44))
  r <- zone_score_delta(a, b, labels = c("v7","v8"))
  expect_equal(r$n_zones_shared, 2)
  expect_equal(r$zones_only_a, "FULL")
  expect_equal(r$zones_only_b, "AT")
  expect_equal(r$by_metric$n_zones, 2)          # FULL/AT excluded, not compared
  expect_equal(r$by_metric$mean_delta, 1)       # (+2, 0) / 2
  expect_equal(r$by_metric$max_abs_delta, 2)
})

test_that("zone_score_delta() reports metrics unique to one release rather than comparing them", {
  a <- data.frame(zone_key = c("A","B"), metric_key = rep(c("extrisk_other","extrisk_fish"), each = 2),
                  score = c(1, 2, 3, 4))
  b <- data.frame(zone_key = c("A","B"), metric_key = rep(c("extrisk_primary_producer","extrisk_fish"), each = 2),
                  score = c(9, 9, 3, 4))
  r <- zone_score_delta(a, b)
  expect_equal(r$metrics_only_a, "extrisk_other")
  expect_equal(r$metrics_only_b, "extrisk_primary_producer")
  expect_equal(r$by_metric$metric_key, "extrisk_fish")
  expect_equal(r$by_metric$mean_abs_delta, 0)   # unchanged where comparable
})

test_that("zone_score_delta() rejects a frame missing the columns it needs", {
  expect_error(zone_score_delta(data.frame(x = 1), data.frame(x = 1)), "zone_key")
})


# zone_scores() -- the long-form reader behind zone_score_delta(), so the
# per-component comparison in compare_versions.qmd reads each release ONCE.
test_that("zone_scores() returns every (zone, metric) of the unit, on either schema", {
  skip_if_not_installed("duckdb")
  new <- mk_scored(scores = c(A = 50, B = 60))
  DBI::dbExecute(new, "INSERT INTO metric VALUES (2,'extrisk_fish_ecoregion_rescaled')")
  DBI::dbExecute(new, "INSERT INTO zone_metric VALUES (1,2,10),(2,2,20)")
  s <- zone_scores(new, zone_set_key = "programarea_2026-01", label = "new")
  expect_named(s, c("zone_key", "metric_key", "score"))
  expect_equal(nrow(s), 4L)
  expect_equal(s$score[s$zone_key == "B" & s$metric_key == "extrisk_fish_ecoregion_rescaled"], 20)

  # v1-v7 schema: no zone_set_key column, so the fld filter carries it
  old <- mk_scored(with_zs = FALSE, scores = c(A = 40, B = 60))
  s_old <- zone_scores(old, zone_set_key = "programarea_2026-01", label = "old")
  expect_equal(nrow(s_old), 2L)

  # and the two feed zone_score_delta() directly
  d <- zone_score_delta(s_old, s, labels = c("old", "new"))
  expect_equal(d$n_zones_shared, 2L)
  expect_equal(d$metrics_only_b, "extrisk_fish_ecoregion_rescaled")
  expect_equal(d$by_metric$mean_delta, 5)   # (50-40 + 60-60) / 2

  # a zone set the release lacks is an error, never an empty frame
  expect_error(zone_scores(new, zone_set_key = "programarea_1999-01"), "no zones for zone_set_key")
})


test_that("zone_scored_flds() names the units that carry the metric, with zone counts", {
  skip_if_not_installed("duckdb")
  con <- mk_scored(scores = c(A = 50, B = 60))           # 2 program areas with the composite
  DBI::dbExecute(con, "INSERT INTO zone VALUES (3,'planarea_key','P1','planarea_2025-06')")
  DBI::dbExecute(con, "INSERT INTO metric VALUES (2,'extrisk_fish')")
  DBI::dbExecute(con, "INSERT INTO zone_metric VALUES (3,2,1)")   # planarea has fish, NOT the composite
  f <- zone_scored_flds(con)
  expect_equal(unname(f), "programarea_key")
  expect_equal(names(f), "2")
  expect_equal(unname(zone_scored_flds(con, "extrisk_fish")), "planarea_key")
  expect_length(zone_scored_flds(con, "nope"), 0)
})

# zone_crosswalk() -- the v1 Planning Area <-> Program Area bridge. The rule:
# only the SAME polygon is compared; a subset (the Gulf Program Areas inside
# their Planning Areas) is reported with its IoU and NOT marked identical.
test_that("zone_crosswalk() pairs identical polygons and refuses subsets", {
  skip_if_not_installed("sf")
  sq <- function(x0, y0, w = 1, h = 1) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0),
                                                                 c(x0 + w, y0 + h), c(x0, y0 + h), c(x0, y0))))
  a <- sf::st_sf(planarea_key = c("COK", "WGA", "CGA", "HAW"),
                 geometry = sf::st_sfc(sq(0, 0), sq(10, 0, 2), sq(12, 0, 2), sq(50, 50)), crs = 4326)
  b <- sf::st_sf(programarea_key = c("COK", "GAA"), planarea_key = c("COK", "WGA,CGA"),
                 geometry = sf::st_sfc(sq(0, 0), sq(11, 0, 2)), crs = 4326)   # GAA = half of WGA + half of CGA
  x <- zone_crosswalk(a, b, "planarea_key", "programarea_key", hint = "planarea_key")
  expect_equal(x$key_b, c("COK", "GAA"))
  expect_equal(x$identical, c(TRUE, FALSE))
  expect_equal(x$iou[x$key_b == "COK"], 1)
  expect_equal(x$iou[x$key_b == "GAA"], 0.5)          # 2 of the 4 km^2 union
  # without a hint the candidates are every intersecting pair, same verdicts
  y <- zone_crosswalk(a, b, "planarea_key", "programarea_key")
  expect_true(y$identical[y$key_a == "COK" & y$key_b == "COK"])
  expect_false(any(y$identical[y$key_b == "GAA"]))
  expect_false("HAW" %in% y$key_a)                     # nothing to compare it with
})
