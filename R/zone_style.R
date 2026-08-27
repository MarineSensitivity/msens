# zone_style.R — how each spatial unit is DRAWN, in one place for every app
#
# The scores app used to paint every scored unit (Program Areas, Ecoregions,
# Subregions) with the same white 1px line and the same white label, so at a
# national zoom "CEC" sat on "CAC", "AK" on "ALA", and nothing said which outline
# was which; the species app hand-coded its own pair (white Program Areas, black
# 3px Ecoregions, no ecoregion labels). Both apps now read THIS table, so the two
# maps agree and a new unit type is styled once. Subregions are drawn but never
# labelled: their boundaries ARE the ecoregion rollup, so labelling both put a
# second code on every ecoregion's line.

#' Line + label style for a spatial unit type
#'
#' @param zone_type character(1): `programarea`, `planarea` (v1's analogue of the
#'   Program Areas), `ecoregion`, `subregion`, or anything else (which gets the muted
#'   context style)
#' @return list with `line` (`color`, `width`, `opacity`, `dasharray` or `NULL`) and
#'   `label` (`color`, `size`, `halo_color`, `halo_width`), where `label` is `NULL`
#'   for a unit drawn without labels
#' @export
#' @concept viz
zone_style <- function(zone_type) {
  stopifnot(is.character(zone_type), length(zone_type) == 1, !is.na(zone_type))
  switch(zone_type,
    programarea = ,
    planarea = list(
      line  = list(color = "white", width = 1, opacity = 1, dasharray = NULL),
      label = list(color = "white", size = 12,
                   halo_color = "rgba(0,0,0,0.75)", halo_width = 1)),
    ecoregion = list(
      line  = list(color = "black", width = 3, opacity = 1, dasharray = NULL),
      label = list(color = "black", size = 16,
                   halo_color = "rgba(255,255,255,0.85)", halo_width = 1.5)),
    subregion = list(
      line  = list(color = "#d9d9d9", width = 2, opacity = 0.7, dasharray = c(3, 3)),
      label = NULL),
    list(
      line  = list(color = "white", width = 0.5, opacity = 0.45, dasharray = NULL),
      label = list(color = "white", size = 11,
                   halo_color = "rgba(0,0,0,0.75)", halo_width = 1)))
}

#' Outline arguments for [add_pmline()], from [zone_style()]
#'
#' @inheritParams zone_style
#' @return named list (`line_color`, `line_width`, `line_opacity`, and
#'   `line_dasharray` when the style dashes) to `c()` into an outline spec
#' @export
#' @concept viz
zone_line_args <- function(zone_type) {
  ln  <- zone_style(zone_type)$line
  out <- list(line_color = ln$color, line_width = ln$width, line_opacity = ln$opacity)
  if (!is.null(ln$dasharray)) out$line_dasharray <- ln$dasharray
  out
}

#' Label arguments for [add_pmlabel()], from [zone_style()]
#'
#' @inheritParams zone_style
#' @return named list (`text_color`, `text_size`, `text_halo_color`,
#'   `text_halo_width`) to `c()` into a label spec, or `NULL` when the unit is
#'   drawn without labels -- callers skip the label layer entirely then
#' @export
#' @concept viz
zone_label_args <- function(zone_type) {
  lb <- zone_style(zone_type)$label
  if (is.null(lb)) return(NULL)
  list(text_color = lb$color, text_size = lb$size,
       text_halo_color = lb$halo_color, text_halo_width = lb$halo_width)
}
