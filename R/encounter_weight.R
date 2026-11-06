# The encounter-rate weight (v11 Q3b, plan 2026-10-29 §4.2 (iv), D11 "gate + weight"): where the
# occurrence rule KEEPS a species in a zone but its record rate is well below its rate elsewhere, the
# merged value is DOWNWEIGHTED rather than dropped -- Tim White's "downweight, not drop" (2026-10-21/22).
# A uniform per-zone weight cancelled under v7's ecoregional min-max rescale (hence v7c dropped instead);
# under the share (v10+) there is no rescale, so a weight survives and acts on both the numerator and the
# species' total. `merge_models.qmd` applies it as a per-species x per-zone multiplier behind
# REGION_WEIGHT=1; `test-encounter_weight.R` asserts it.

#' Encounter-rate weight per species x zone, from the occurrence rule's counts
#'
#' `w = min(1, rate_zone / rate_reference)`, where `rate_zone` = the species' record rate in the zone
#' (occupied / searched 0.5-degree blocks, [occurrence_rule()]'s `rate`) and `rate_reference` = its rate
#' over the baseline ([occurrence_rule()]'s `p_core`: the other zones of the set, or the whole range).
#' The weight is a MULTIPLIER on the merged value inside the zone. The tiers Tim White asked for apply
#' in this order:
#' \itemize{
#'   \item `dropped` (the gate): `w = 0` -- zero detections under adequate effort are a gate, not a
#'     weight; the rows are also in the exclusion table, so the weight is redundant there by design.
#'   \item an override (`protected`, `narrow range`) or `unsearched`: `w = 1` -- unsampled is unknown,
#'     not rare; critical habitat is never downweighted.
#'   \item no reference (`p_core` is 0 or `NA`: the species has no record anywhere else): `w = 1`
#'     (the pooled fallback -- nothing to compare against, so nothing is taken away).
#'   \item otherwise `w = max(floor, min(1, rate / p_core))`; a zone at or above its reference rate
#'     is never UPweighted (cap at 1).
#' }
#'
#' @param x a data frame as [occurrence_rule()] returns: `rate`, `p_core`, `verdict`, `override`
#'   (and `mdl_key`, `zone_key`, carried through).
#' @param floor numeric in [0, 1]; the smallest weight a kept zone can take (default 0: the rate ratio
#'   as measured).
#' @return `x` with a `w` column (numeric in [floor, 1], 0 where dropped) and `w_reason`
#'   (`dropped | override | unsearched | no reference | rate ratio | at reference`).
#' @concept range
#' @export
#' @examples
#' x <- data.frame(mdl_key = "a", zone_key = c("A", "B", "C", "D"),
#'                 rate = c(0.1, 0.5, 0, 0.02), p_core = c(0.4, 0.4, 0.4, 0.4),
#'                 verdict = c("kept", "kept", "unsearched", "dropped"), override = NA)
#' encounter_weight(x)$w   # 0.25, 1, 1, 0
encounter_weight <- function(x, floor = 0) {
  stopifnot(
    is.data.frame(x),
    "x needs the occurrence rule's columns rate, p_core, verdict, override" =
      all(c("rate", "p_core", "verdict", "override") %in% names(x)),
    is.numeric(floor), length(floor) == 1L, is.finite(floor), floor >= 0, floor <= 1,
    all(x$verdict %in% c("kept", "dropped", "unsearched")))
  ratio <- ifelse(is.finite(x$p_core) & x$p_core > 0, x$rate / x$p_core, NA_real_)
  w <- pmax(floor, pmin(1, ratio))
  reason <- ifelse(is.na(ratio), "no reference", ifelse(ratio >= 1, "at reference", "rate ratio"))
  w[is.na(ratio)] <- 1
  unsearched <- x$verdict == "unsearched"
  w[unsearched] <- 1; reason[unsearched] <- "unsearched"
  overridden <- !is.na(x$override)
  w[overridden] <- 1; reason[overridden] <- "override"
  dropped <- x$verdict == "dropped"
  w[dropped] <- 0; reason[dropped] <- "dropped"
  x$w <- w; x$w_reason <- reason
  x
}
