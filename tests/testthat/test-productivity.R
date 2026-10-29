# Guards vgpm_annual_integrate() (v11 Q4, plan 2026-10-29 §4.1; spec v10-2): a month without a
# retrieval counts as ZERO production, the year is integrated by days, land stays NA, leap years use
# 366, and n_valid counts the observed months. The regression case is (b): the old recipe's
# mean(na.rm = TRUE) returned 100 for six months of 100 and six of NA.

skip_if_not_installed("terra")

# a 2 x 2 raster per month; cell 1 = all months observed, cell 2 = six observed (Apr-Sep), cell 3 =
# never observed (land), cell 4 = one month (July) only
month_stack <- function(year, vals_by_cell) {
  layers <- lapply(1:12, function(m) {
    v <- vapply(vals_by_cell, function(cell) cell[m], 0)
    terra::rast(matrix(v, nrow = 2, ncol = 2, byrow = TRUE))
  })
  r <- terra::rast(layers); names(r) <- month.abb; r
}
obs6 <- c(NA, NA, NA, 100, 100, 100, 100, 100, 100, NA, NA, NA)
jul1 <- c(NA, NA, NA, NA, NA, NA, 50, NA, NA, NA, NA, NA)
cells <- list(rep(100, 12), obs6, rep(NA_real_, 12), jul1)

test_that("twelve observed months give the day-weighted mean (equal months: the plain mean)", {
  r <- vgpm_annual_integrate(month_stack(2019, cells), 2019)
  expect_equal(names(r), c("npp", "n_valid"))
  expect_equal(terra::values(r$npp)[1], 100)
  expect_equal(terra::values(r$n_valid)[1], 12)
  # unequal months: January 31 days of 31, February 28 days of 28, ... -> day-weighted
  ramp <- lapply(1:4, function(i) vgpm_days_in_months(2019))
  r2 <- vgpm_annual_integrate(month_stack(2019, ramp), 2019)
  expect_equal(terra::values(r2$npp)[1], sum(vgpm_days_in_months(2019)^2) / 365)
})

test_that("six months of 100 and six without a retrieval integrate to 100 x days(Apr-Sep)/365, NOT 100 (the regression)", {
  r <- vgpm_annual_integrate(month_stack(2019, cells), 2019)
  d <- vgpm_days_in_months(2019)
  expect_equal(terra::values(r$npp)[2], 100 * sum(d[4:9]) / 365)
  expect_equal(terra::values(r$n_valid)[2], 6)
  expect_false(isTRUE(all.equal(terra::values(r$npp)[2], 100)))
})

test_that("a cell never observed stays NA (land is not zero); one observed month counts its days alone", {
  r <- vgpm_annual_integrate(month_stack(2019, cells), 2019)
  expect_true(is.na(terra::values(r$npp)[3]))
  expect_equal(terra::values(r$n_valid)[3], 0)
  expect_equal(terra::values(r$npp)[4], 50 * 31 / 365)
  expect_equal(terra::values(r$n_valid)[4], 1)
})

test_that("a leap year integrates over 366 days", {
  expect_equal(vgpm_days_in_months(2020)[2], 29); expect_equal(sum(vgpm_days_in_months(2020)), 366)
  expect_equal(vgpm_days_in_months(1900)[2], 28); expect_equal(vgpm_days_in_months(2000)[2], 29)
  r <- vgpm_annual_integrate(month_stack(2020, cells), 2020)
  d <- vgpm_days_in_months(2020)
  expect_equal(terra::values(r$npp)[2], 100 * sum(d[4:9]) / 366)
  expect_equal(terra::values(r$npp)[1], 100)
})

test_that("the inputs are checked: 12 layers, a SpatRaster, one integer year", {
  m <- month_stack(2019, cells)
  expect_error(vgpm_annual_integrate(m[[1:11]], 2019), "12 layers")
  expect_error(vgpm_annual_integrate(as.matrix(1:12), 2019), "SpatRaster")
  expect_error(vgpm_annual_integrate(m, 2019.5), "year")
})
