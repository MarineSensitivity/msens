# Primary productivity: integrate the year, do not average the observed months.
#
# Monthly VGPM composites have no retrieval under polar night or sea ice, so a high-latitude "year"
# averaged over its observed months is the mean of its 4-6 bloom months while a Gulf year is the mean
# of all twelve; annualised alike, the Arctic came out ~2x too productive (Hope Basin 875 vs ~470 g C
# m-2 yr-1 in situ; Mike Rasser's v1 review, comment C76). The rule, decided 2026-09-04 and placed in
# v11 (plan 2026-10-29 §4.1): a month without a retrieval contributes ZERO production, and the annual
# rate is the day-weighted integral of the months over the whole year. `ingest_productivity.qmd`
# CALLS this; `test-productivity.R` ASSERTS it.

#' Integrate monthly productivity over a year (no-retrieval months count as zero)
#'
#' Turns twelve monthly mean daily rates into one annual mean daily rate in the same units:
#' `npp = sum_m rate_m * days_m / days_in_year`, where a month without a retrieval (`NA`: polar night,
#' ice, no data) contributes 0 and `days_in_year` is 365 or 366 for a leap year. A cell with no
#' retrieval in ANY month stays `NA` (land, permanent no-data) rather than becoming a zero, so land is
#' never painted as zero production. `n_valid` counts the months with a retrieval, so the share of a
#' year a cell was observed travels with its rate.
#'
#' Because the result is still a mean daily rate, nothing downstream changes units: with twelve
#' observed months it equals the day-weighted mean of the months (and, for equal-length months, the
#' plain mean the old recipe used); with six months of 100 and six of `NA` it is `100 * days(those
#' six) / 365`, not 100.
#'
#' @param monthly a `SpatRaster` of 12 layers, the monthly mean daily rates in month order
#'   (January first), `NA` = no retrieval.
#' @param year integer; the calendar year of the twelve months (sets `days_m` and leap years).
#' @return a 2-layer `SpatRaster`: `npp` (the annual mean daily rate) and `n_valid` (0-12).
#' @concept ingest
#' @export
#' @examples
#' \dontrun{
#' m <- terra::rast(replicate(12, matrix(100, 2, 2), simplify = FALSE) |> lapply(terra::rast))
#' vgpm_annual_integrate(m, 2019)
#' }
vgpm_annual_integrate <- function(monthly, year) {
  stopifnot(
    "monthly must be a terra SpatRaster" = inherits(monthly, "SpatRaster"),
    "monthly must have exactly 12 layers, January first" = terra::nlyr(monthly) == 12L,
    "year must be one integer" = is.numeric(year), length(year) == 1L, !is.na(year), year == round(year))
  year <- as.integer(year)
  days <- vgpm_days_in_months(year)
  # NA -> 0 for the integral, but remember where any retrieval exists
  n_valid <- terra::app(monthly, function(x) sum(!is.na(x)))
  filled  <- terra::ifel(is.na(monthly), 0, monthly)
  npp     <- terra::app(filled * days, sum) / sum(days)
  npp     <- terra::ifel(n_valid == 0, NA, npp)
  out <- c(npp, n_valid)
  names(out) <- c("npp", "n_valid")
  out
}

#' Days in each month of a year
#'
#' @param year integer calendar year.
#' @return an integer vector of 12.
#' @keywords internal
#' @export
vgpm_days_in_months <- function(year) {
  year <- as.integer(year)
  leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  c(31L, if (leap) 29L else 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
}
