# density.R — density-response models (individuals km^-2) onto the merge's [0,100] scale ----
#
# The Marine Atlas merges one surface per taxon on AquaMaps' relative-suitability scale
# ([0,100], `msens::merge_sql()`). Two ingested datasets respond in DENSITY instead — NOAA
# SEFSC Gulf of America cetaceans + sea turtles (`gm`, monthly, 40 km² hexagons) and NOAA
# NCCOS seabirds (`nc`, seasonal, 2 km) — and were registered but never scored, because
# density has no agreed mapping onto that scale and a density surface covers a REGION,
# not the taxon's range. These functions hold the candidate transforms so the evaluation
# notebook (`workflows/compare_density_methods.qmd`) CALLS them and `test-density.R`
# ASSERTS them; whichever transform the release adopts, the ingest calls the same function.

#' Density → suitability-scale [0,100] transforms
#'
#' Maps a density-response surface (individuals km^-2 per cell over a model's domain) onto
#' the [0,100] scale the per-taxon merge operates on. Four candidates, all monotone in
#' density; they differ in how they DISTRIBUTE values, which is what an extinction-risk-
#' weighted sum (`Σ er × val / 100`) and an ecoregional min–max rescale respond to:
#' \describe{
#'   \item{`"cap"`}{Linear in density, 100 at the `p_cap` quantile of the positive densities,
#'     clamped above. Proportional — `Σ val` is proportional to modelled abundance below the
#'     cap — so a surface keeps its meaning as a relative-abundance layer. Heavy-tailed
#'     densities leave most cells near 0. What the v8 `gm`/`nc` ingests do.}
#'   \item{`"log"`}{Linear in log density between the `p_floor` and `p_cap` quantiles (0 below,
#'     100 above). Compresses the tail; loses proportionality; the floor is arbitrary.}
#'   \item{`"ud"`}{The population percentile of the cell: 100 minus the percent of the modelled
#'     population living in cells STRICTLY denser than this one (`area`-weighted). The densest
#'     cell is 100; the cells with `val >= 100 - p` are exactly the p % core — the
#'     utilization-distribution isopleth convention (50 % core, 95 % home range).}
#'   \item{`"qmap"`}{Quantile mapping (histogram matching) of the density distribution onto a
#'     reference suitability distribution `ref`: `val = Q_ref(F_d(d))`. Keeps the density's
#'     spatial ordering and takes the incumbent's marginal, so folding the surface in
#'     REDISTRIBUTES the taxon's values inside the domain rather than re-levelling them.}
#' }
#' A non-positive density is 0 under every method (absent is absent), and `NA` passes through.
#'
#' @param d numeric; density per cell (individuals km^-2) over the model domain.
#' @param method one of `"cap"`, `"log"`, `"ud"`, `"qmap"`.
#' @param p_cap quantile of the POSITIVE densities that maps to 100 (`cap`, `log`); default 0.995.
#' @param p_floor quantile of the positive densities that maps to 0 under `log`; default 0.01.
#' @param area numeric; cell areas (km^2) weighting `ud` (default `NULL` = equal areas).
#' @param ref numeric in [0,100]; the reference distribution for `qmap` (e.g. the taxon's own
#'   AquaMaps/AquaX suitability over the same cells). Required for `qmap`.
#' @param digits rounding of the result (default 2, the atlas' `val` precision); `NULL` = none.
#' @return numeric in [0,100] of `length(d)`; `NA` where `d` is `NA`.
#' @examples
#' d <- c(0, 0.001, 0.01, 0.1, 1)
#' density_to_suit(d, "cap", p_cap = 1)     # 0, 0.1, 1, 10, 100
#' density_to_suit(d, "ud")                 # population percentile: densest cell = 100
#' density_to_suit(d, "qmap", ref = c(5, 20, 40, 60, 90))
#' @concept ingest
#' @importFrom stats quantile ecdf
#' @export
density_to_suit <- function(d, method = c("cap", "log", "ud", "qmap"),
                            p_cap = 0.995, p_floor = 0.01, area = NULL, ref = NULL,
                            digits = 2) {
  method <- match.arg(method)
  stopifnot(is.numeric(d),
            is.numeric(p_cap), length(p_cap) == 1, p_cap > 0, p_cap <= 1,
            is.numeric(p_floor), length(p_floor) == 1, p_floor >= 0, p_floor < p_cap,
            is.null(area) || (is.numeric(area) && length(area) == length(d)),
            is.null(digits) || (is.numeric(digits) && length(digits) == 1))
  ok  <- !is.na(d)
  pos <- ok & d > 0
  out <- rep(NA_real_, length(d))
  out[ok] <- 0                                          # absent (d <= 0) is 0 under every method
  if (!any(pos)) return(out)

  if (method == "cap") {
    hi <- stats::quantile(d[pos], p_cap, names = FALSE)
    out[pos] <- 100 * pmin(d[pos] / hi, 1)

  } else if (method == "log") {
    lo <- stats::quantile(d[pos], p_floor, names = FALSE)
    hi <- stats::quantile(d[pos], p_cap,   names = FALSE)
    out[pos] <- if (hi > lo) {
      100 * pmin(pmax((log(d[pos]) - log(lo)) / (log(hi) - log(lo)), 0), 1)
    } else 100 * as.numeric(d[pos] >= hi)              # degenerate: a single positive value

  } else if (method == "ud") {
    a <- if (is.null(area)) rep(1, length(d)) else area
    w <- d[pos] * a[pos]                                 # animals per cell
    W <- sum(w)
    # share of the population in cells STRICTLY denser than each cell: sort densest first, take
    # the exclusive cumulative sum, and give every member of a tie group the value at the group's
    # first row (never through factor(): as.character() on doubles collapses near-equal densities
    # into duplicate levels)
    dd    <- d[pos]
    o     <- order(dd, decreasing = TRUE)
    d_o   <- dd[o]; w_o <- w[o]
    above <- c(0, cumsum(w_o))[seq_along(w_o)]                # animals in rows before this one
    above <- above[match(d_o, d_o)]                           # first row of each tie group
    v     <- numeric(length(dd)); v[o] <- 100 * (1 - above / W)
    out[pos] <- v

  } else if (method == "qmap") {
    stopifnot("qmap needs a non-empty numeric `ref` in [0,100]" =
                is.numeric(ref) && length(ref[!is.na(ref)]) > 0)
    ref <- ref[!is.na(ref)]
    stopifnot(all(ref >= 0 & ref <= 100))
    # plotting-position probabilities (rank - 1)/(n - 1): the sparsest positive cell takes
    # min(ref), the densest max(ref), ties share a value; with equal n this is an exact
    # rank-for-rank histogram match
    n  <- sum(pos)
    Fd <- if (n > 1) (rank(d[pos], ties.method = "average") - 1) / (n - 1) else 1
    out[pos] <- stats::quantile(ref, Fd, names = FALSE, type = 7)
  }
  out <- pmin(pmax(out, 0), 100)
  if (!is.null(digits)) out <- round(out, digits)
  out
}

#' Annual mean density from interval (monthly / seasonal) surfaces
#'
#' The annual surface a static score consumes is the mean over ALL intervals of the year, with
#' an interval in which a cell carries no value counted as ZERO — not the mean over the
#' intervals in which the cell happens to be present. The v8 `gm`/`nc` ingests averaged only the
#' present intervals, which biases every cell upward and most for the sparsest species (a cell
#' modelled in 2 of 12 months at density 1 is 0.17 animals km^-2 year-round, not 1).
#'
#' Whether an UNMODELLED interval (a season NCCOS did not fit for a species, a `-9999` month) is
#' truly zero is the caller's decision: pass only the intervals that count in `n_intervals`.
#'
#' @param x a data frame with columns `cell_id`, an interval column and a density column; one
#'   row per (cell, interval) where the cell has a value.
#' @param n_intervals integer; the number of intervals in the year (12 months, 4 seasons).
#' @param value name of the density column (default `"dens"`).
#' @return a data frame `(cell_id, dens)` — `dens` = Σ interval density / `n_intervals`.
#' @examples
#' x <- data.frame(cell_id = c(1, 1, 2), interval = c("01", "02", "01"), dens = c(1, 1, 3))
#' density_annual(x, n_intervals = 12)   # cell 1: 2/12; cell 2: 3/12
#' @concept ingest
#' @importFrom stats aggregate
#' @export
density_annual <- function(x, n_intervals, value = "dens") {
  stopifnot(is.data.frame(x), "cell_id" %in% names(x), value %in% names(x),
            is.numeric(n_intervals), length(n_intervals) == 1, n_intervals >= 1)
  s <- stats::aggregate(list(dens = x[[value]]), by = list(cell_id = x$cell_id), FUN = sum)
  s$dens <- s$dens / n_intervals
  s[order(s$cell_id), , drop = FALSE]
}
