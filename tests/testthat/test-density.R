# Guards the density → [0,100] transforms (R/density.R) that the gm/nc fold-in evaluation
# (workflows/compare_density_methods.qmd) and, once adopted, the density ingests call. One
# fixture per rule with the exact expected output, so a re-ordered cumsum, a dropped tie
# group or an off-by-one quantile cannot pass silently.

test_that("cap: linear to the p_cap quantile, clamped, absent = 0, NA passes through", {
  d <- c(0, 0.001, 0.01, 0.1, 1, NA)
  v <- density_to_suit(d, "cap", p_cap = 1)             # 100 at the max
  expect_equal(v, c(0, 0.1, 1, 10, 100, NA))
  # p_cap below the max: the top cell is clamped at 100, the rest stay proportional to the cap
  d2 <- c(1, 2, 3, 4, 100)
  v2 <- density_to_suit(d2, "cap", p_cap = 0.5, digits = NULL)
  cap <- stats::quantile(d2, 0.5, names = FALSE)          # 3
  expect_equal(v2, c(100 * 1 / cap, 100 * 2 / cap, 100, 100, 100))
  # proportionality below the cap: Σ val ∝ abundance
  expect_equal(v2[2] / v2[1], 2)
})

test_that("log: 0 at the floor quantile, 100 at the cap quantile, monotone between", {
  d <- 10^seq(-3, 2, by = 0.5)                            # 11 values, 3 decades
  v <- density_to_suit(d, "log", p_floor = 0, p_cap = 1, digits = NULL)
  expect_equal(v[1], 0); expect_equal(v[length(v)], 100)
  expect_equal(v, seq(0, 100, length.out = length(d)))    # linear in log10
  expect_true(all(diff(v) > 0))
  expect_equal(density_to_suit(c(0, 5), "log", p_floor = 0, p_cap = 1)[1], 0)
})

test_that("ud: population percentile — densest cell 100, ties equal, area-weighted", {
  # equal areas: W = 10; strictly-denser shares 0, .4, .7, .9 -> 100, 60, 30, 10
  expect_equal(density_to_suit(c(4, 3, 2, 1), "ud"), c(100, 60, 30, 10))
  # order of input does not matter
  expect_equal(density_to_suit(c(1, 3, 4, 2), "ud"), c(10, 60, 100, 30))
  # ties share a value: two cells at 2 hold 80 % of 5 -> both 100; the 1 sees 80 % denser -> 20
  expect_equal(density_to_suit(c(2, 2, 1), "ud"), c(100, 100, 20))
  # the p % core is exactly {val >= 100 - p}: cells (4,3) hold 70 % of the population, so the
  # 70 % core is the cells with val >= 30
  v <- density_to_suit(c(4, 3, 2, 1), "ud")
  expect_equal(which(v >= 30), 1:3)                       # (4,3,2) — 2 completes the 70 % at 90 %
  expect_equal(which(v >= 40), 1:2)                       # 60 % core: (4,3)
  # area weighting: the same densities on a large sparse cell vs a small dense one
  # d = (1, 4), area = (10, 1): animals = (10, 4), W = 14; 4 is densest -> 100; 1 sees 4/14 denser
  expect_equal(density_to_suit(c(1, 4), "ud", area = c(10, 1), digits = NULL),
               c(100 * (1 - 4 / 14), 100))
  # absent cells are 0, not the bottom percentile
  expect_equal(density_to_suit(c(0, 2, 1), "ud"), c(0, 100, 100 * (1 - 2 / 3)), tolerance = 1e-2)
  # densities that differ only beyond 15 significant digits are DISTINCT cells, not one tie group
  # (a factor() on doubles collapsed them into duplicate levels and errored on the real gm surface)
  d <- c(0.1, 0.1 + 1e-17 * 0.1, 0.1 + 3e-16, 0.05)
  expect_silent(v <- density_to_suit(d, "ud", digits = NULL))
  expect_equal(length(v), 4)
  expect_true(all(v[1:3] > v[4]))
  # a large realistic surface with many exact ties and many near-ties runs and stays in [0,100]
  set.seed(7); dd <- round(rexp(20000, 50), 4) + runif(20000, 0, 1e-12)
  vv <- density_to_suit(dd, "ud")
  expect_true(all(vv >= 0 & vv <= 100)); expect_equal(max(vv), 100)
})

test_that("qmap: takes the reference marginal, keeps the density ordering", {
  d   <- c(0.1, 0.5, 0.2, 0.9, 0.3)
  ref <- c(5, 20, 40, 60, 90)
  v   <- density_to_suit(d, "qmap", ref = ref)
  # the same ranks: densest -> max(ref), sparsest -> min(ref)
  expect_equal(v[order(d)], sort(ref))
  expect_equal(rank(v), rank(d))
  # a flat reference (a range-only incumbent valued at its ER) maps everything to that value
  expect_equal(density_to_suit(d, "qmap", ref = rep(10, 7)), rep(10, 5))
  # absent stays 0 even though the reference has no 0
  expect_equal(density_to_suit(c(0, 1), "qmap", ref = ref)[1], 0)
  expect_error(density_to_suit(d, "qmap"), "ref")
})

test_that("every method returns [0,100] of the input length and 0s for an all-zero surface", {
  d <- c(0, 0, NA)
  for (m in c("cap", "log", "ud")) expect_equal(density_to_suit(d, m), c(0, 0, NA))
  expect_equal(density_to_suit(d, "qmap", ref = 1:100), c(0, 0, NA))
  set.seed(1); dd <- rexp(500)
  for (m in c("cap", "log", "ud"))
    expect_true(all(density_to_suit(dd, m) >= 0 & density_to_suit(dd, m) <= 100))
})

test_that("density_annual counts an absent interval as zero (the v8 present-only mean was the bug)", {
  x <- data.frame(cell_id = c(1L, 1L, 2L, 3L, 3L, 3L),
                  interval = c("01", "02", "01", "01", "02", "03"),
                  dens = c(1, 1, 3, 2, 2, 2))
  a <- density_annual(x, n_intervals = 12)
  expect_equal(a$cell_id, 1:3)
  expect_equal(a$dens, c(2 / 12, 3 / 12, 6 / 12))
  # the present-only mean (what the ingests wrote) is 1, 3, 2 — never equal unless all 12 present
  expect_false(any(a$dens == c(1, 3, 2)))
  # seasonal: 4 intervals, a cell in every season averages plainly
  y <- data.frame(cell_id = 9L, ssn = c("a", "b", "c", "d"), dens = c(1, 2, 3, 4))
  expect_equal(density_annual(y, n_intervals = 4)$dens, 2.5)
})
