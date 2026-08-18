# study_area.R — the study-area picker's camera presets.
#
# A study area MOVES THE CAMERA and nothing else (apps#13). Once that is true,
# the presets stop being a property of a release and become a property of
# GEOGRAPHY, so there is one set and every release gets all of it — including a
# release that never scored there. v7 has Atlantic scores and, until this, no
# Atlantic preset, because the old subregions were dissolved from the 2026
# Program Areas and that program has no Atlantic areas (apps#14).
#
# The source of truth is the ecoregion `region_key` rollup, which is shared
# across v1-v8 (`zone_sets.csv`: ecoregion_2025-06):
#
#   AK Alaska          CBS EBS GOA HAR
#   AT Atlantic        NECS PUR SECS
#   GA Gulf of America EGOA WCGOA
#   PA Pacific         CAC PIS WAOR
#
# WHY SPHERICAL, NOT A BOUNDING BOX
#
# Two of these cross the antimeridian -- East Bering Sea and the Pacific Island
# Territories -- so min/max longitude describes them as spanning the globe. PIS
# is the extreme case: -180..180 naively, 141.2..208.7 the short way round, a
# 67.5 deg span reported as 360. That is the same defect as apps#9, and a bbox
# centre inherits it. Everything here is computed on the UNIT SPHERE instead,
# where the antimeridian is not a special case at all.
#
# WHY UNWEIGHTED VERTICES
#
# The centre is the spherical mean of the ecoregion polygons' vertices, each
# counting once. Weighting by AREA was tried and is wrong: it puts "All US
# waters" at (-158.6, 39.6), out in the north Pacific with the Gulf and the
# east coast behind the horizon, because PIS alone is 5.8M km2 of open ocean
# (more than a quarter of the total) and Alaska is another 4.2M. Vertices track
# the shelf and coastline -- where the models and the cells actually are --
# and give (-101.3, 46.9), which frames Alaska, the west coast, the Gulf and
# the east coast together.
#
# ZOOM comes from the 99th-percentile angular distance from the centre, not the
# maximum: one far islet should not zoom the whole view out. It is deliberately
# the same shape as the formula the app used before (z = log2(360/span) + 0.55,
# clamped), so the framing stays familiar.

#' Spherical mean of longitude/latitude points
#'
#' Averages the unit vectors, so a set straddling the antimeridian averages to
#' the place it actually surrounds rather than to its numeric midpoint. There is
#' no wrapping special case because there is no wrapping.
#'
#' @param lon,lat numeric degrees
#' @param w optional weights
#' @return `c(lon, lat)` in `[-180, 180]`
#' @export
#' @concept study_area
#' @examples
#' # either side of the dateline: the mean is ON the dateline, not at lon 0
#' round(sphere_centroid(c(170, -170), c(0, 0)))
sphere_centroid <- function(lon, lat, w = NULL) {
  ok <- is.finite(lon) & is.finite(lat)
  lon <- lon[ok]; lat <- lat[ok]
  if (!length(lon)) return(c(NA_real_, NA_real_))
  if (is.null(w)) w <- rep(1, length(lon)) else w <- w[ok]
  r <- pi / 180
  x <- sum(cos(lat * r) * cos(lon * r) * w)
  y <- sum(cos(lat * r) * sin(lon * r) * w)
  z <- sum(sin(lat * r) * w)
  n <- sqrt(x^2 + y^2 + z^2)
  # antipodally balanced points cancel to the origin and have no mean direction
  if (!is.finite(n) || n < .Machine$double.eps) return(c(NA_real_, NA_real_))
  c(atan2(y, x) / r, atan2(z, sqrt(x^2 + y^2)) / r)
}

#' Angular distance from a centre to each point, in degrees
#'
#' Great-circle, so it is the quantity a globe camera actually cares about.
#'
#' @param center `c(lon, lat)`
#' @param lon,lat numeric degrees
#' @return numeric degrees, `0` to `180`
#' @export
#' @concept study_area
angular_distance <- function(center, lon, lat) {
  r <- pi / 180
  c0 <- c(cos(center[2] * r) * cos(center[1] * r),
          cos(center[2] * r) * sin(center[1] * r),
          sin(center[2] * r))
  X <- cbind(cos(lat * r) * cos(lon * r), cos(lat * r) * sin(lon * r), sin(lat * r))
  acos(pmin(1, pmax(-1, as.numeric(X %*% c0)))) / r
}

#' Zoom that frames a given angular radius
#'
#' @param radius_deg angular radius from the centre, degrees
#' @return a MapLibre zoom, clamped to `[1.2, 5]`
#' @export
#' @concept study_area
view_zoom <- function(radius_deg) {
  if (!is.finite(radius_deg) || radius_deg <= 0) return(5)
  max(1.2, min(5, log2(360 / (2 * radius_deg)) + 0.55))
}

#' Canonical study areas — one set, every release
#'
#' Camera presets derived from the ecoregion `region_key` rollup by
#' [study_area_views()]; see the note at the top of `study_area.R` for how the
#' centres and zooms are chosen and what was rejected.
#'
#' Baked rather than computed at run time because the ecoregion geometry is
#' shared across every release and does not change: an app should not open a
#' GeoPackage on startup to find out where Alaska is. Regenerate with
#' [study_area_views()] if the ecoregion layer is ever revised.
#'
#' @return a data frame: `key`, `label`, `lon`, `lat`, `zoom`, `ecoregions`
#' @export
#' @concept study_area
study_areas <- function() {
  data.frame(
    key   = c("FULL", "AK", "AT", "GA", "PA"),
    label = c("All US waters", "Alaska", "Atlantic", "Gulf of America", "Pacific"),
    lon   = c(-101.304, -153.991, -75.921, -87.552, -126.454),
    lat   = c(  46.900,   60.225,  35.721,  28.406,   37.322),
    zoom  = c(   2.16,     4.02,    4.40,    4.92,     1.82),
    ecoregions = c("CAC CBS EBS EGOA GOA HAR NECS PIS PUR SECS WAOR WCGOA",
                   "CBS EBS GOA HAR", "NECS PUR SECS", "EGOA WCGOA", "CAC PIS WAOR"),
    stringsAsFactors = FALSE)
}

#' Recompute the study-area presets from an ecoregion layer
#'
#' The derivation behind [study_areas()], kept runnable so the baked constants
#' are reproducible rather than magic.
#'
#' @param x an `sf` of ecoregion polygons with `region_key` and `ecoregion_key`
#' @param quantile_r quantile of angular distance used to set the zoom; the max
#'   would let a single far islet zoom the whole view out
#' @param labels named character vector of region labels
#' @return the same shape as [study_areas()]
#' @export
#' @concept study_area
study_area_views <- function(x, quantile_r = 0.99,
                             labels = c(FULL = "All US waters", AK = "Alaska",
                                        AT = "Atlantic", GA = "Gulf of America",
                                        PA = "Pacific")) {
  stopifnot("x needs region_key and ecoregion_key" =
              all(c("region_key", "ecoregion_key") %in% names(x)))
  g <- sf::st_geometry(x)
  # Per feature, dropping EMPTY parts. A whole-layer st_coordinates() ERRORS on
  # this layer, and st_cast(., "MULTIPOINT") silently returns nothing for a
  # MULTIPOLYGON whose FIRST part is EMPTY -- which is exactly how PIS is
  # stored, so the largest ecoregion vanished without a warning.
  V <- do.call(rbind, lapply(seq_along(g), function(i) {
    gi <- tryCatch(sf::st_make_valid(g[i]), error = function(e) g[i])
    p  <- tryCatch(suppressWarnings(sf::st_cast(gi, "POLYGON")), error = function(e) NULL)
    if (is.null(p) || !length(p)) return(NULL)
    p <- p[!sf::st_is_empty(p)]
    if (!length(p)) return(NULL)
    xy <- sf::st_coordinates(p)
    data.frame(lon = xy[, "X"], lat = xy[, "Y"],
               region = x$region_key[i], eco = x$ecoregion_key[i],
               stringsAsFactors = FALSE)
  }))
  stopifnot("no vertices extracted" = !is.null(V) && nrow(V) > 0)

  one <- function(key, sel) {
    s  <- V[sel, , drop = FALSE]
    ct <- sphere_centroid(s$lon, s$lat)
    a  <- angular_distance(ct, s$lon, s$lat)
    data.frame(key = key,
               label = unname(if (key %in% names(labels)) labels[[key]] else key),
               lon = round(ct[1], 3), lat = round(ct[2], 3),
               zoom = round(view_zoom(stats::quantile(a, quantile_r)), 2),
               ecoregions = paste(sort(unique(s$eco)), collapse = " "),
               stringsAsFactors = FALSE)
  }
  regions <- sort(unique(V$region))
  rbind(one("FULL", rep(TRUE, nrow(V))),
        do.call(rbind, lapply(regions, function(r) one(r, V$region == r))))
}
