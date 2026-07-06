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
