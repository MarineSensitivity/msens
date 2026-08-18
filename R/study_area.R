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

#' Centre of the smallest circle containing every point
#'
#' The framing rule for a REGION: by construction it minimises the distance to
#' the farthest point, so a zoom derived from its radius shows all of the region
#' and no more. A centroid cannot promise that -- it is pulled toward wherever
#' the points are dense, which for polygon vertices means wherever the coastline
#' is crinkliest, not where the region is.
#'
#' That bias was not academic: weighting Alaska by vertex count put its centre
#' at 60.2 N, south of the Arctic ecoregions, and the view cut off Beaufort and
#' the High Arctic entirely. The Pacific was worse in the other direction --
#' California and Washington/Oregon hold 154,878 vertices against the Pacific
#' Island Territories' 14,305, for a twenty-fifth of the area, so the centre
#' landed on the US west coast and the zoom had to pull back to a whole-globe
#' view to reach Guam.
#'
#' Badoiu-Clarkson: step toward the current farthest point by 1/(i+1). Converges
#' quickly and needs no convex hull on the sphere.
#'
#' @param lon,lat numeric degrees
#' @param iter iterations
#' @return list with `center` (`c(lon, lat)`) and `radius` in degrees
#' @export
#' @concept study_area
mec_center <- function(lon, lat, iter = 300L) {
  ok <- is.finite(lon) & is.finite(lat)
  lon <- lon[ok]; lat <- lat[ok]
  if (!length(lon)) return(list(center = c(NA_real_, NA_real_), radius = NA_real_))
  r <- pi / 180
  xyz <- function(a, b) c(cos(b * r) * cos(a * r), cos(b * r) * sin(a * r), sin(b * r))
  ct <- sphere_centroid(lon, lat)
  if (anyNA(ct)) ct <- c(lon[1], lat[1])
  for (i in seq_len(iter)) {
    j  <- which.max(angular_distance(ct, lon, lat))
    m  <- xyz(ct[1], ct[2]) + (xyz(lon[j], lat[j]) - xyz(ct[1], ct[2])) / (i + 1)
    n  <- sqrt(sum(m^2))
    if (!is.finite(n) || n < .Machine$double.eps) break
    m  <- m / n
    ct <- c(atan2(m[2], m[1]) / r, atan2(m[3], sqrt(m[1]^2 + m[2]^2)) / r)
  }
  list(center = ct, radius = max(angular_distance(ct, lon, lat)))
}

#' Zoom that frames a given angular radius
#'
#' @param radius_deg angular radius from the centre, degrees
#' @param margin fraction to inflate the radius by before computing the zoom, so
#'   the area sits inside the frame rather than flush against its edge
#' @return a MapLibre zoom, clamped to `[1.2, 5]`
#' @export
#' @concept study_area
view_zoom <- function(radius_deg, margin = 0.10) {
  if (!is.finite(radius_deg) || radius_deg <= 0) return(5)
  max(1.2, min(5, log2(360 / (2 * radius_deg * (1 + margin))) + 0.55))
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
    lon   = c(-101.304, -164.654, -67.627, -89.089, -171.570),
    lat   = c(  46.900,   63.327,  29.862,  26.251,   28.541),
    zoom  = c(   2.16,     3.64,    4.00,    5.00,     2.36),
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

  row <- function(key, sel, ct, zoom) {
    s <- V[sel, , drop = FALSE]
    data.frame(key = key,
               label = unname(if (key %in% names(labels)) labels[[key]] else key),
               lon = round(ct[1], 3), lat = round(ct[2], 3), zoom = round(zoom, 2),
               ecoregions = paste(sort(unique(s$eco)), collapse = " "),
               stringsAsFactors = FALSE)
  }

  # A REGION is framed by its minimum enclosing circle, so all of it fits.
  one_region <- function(r) {
    sel <- V$region == r
    m   <- mec_center(V$lon[sel], V$lat[sel])
    row(r, sel, m$center, view_zoom(m$radius))
  }

  # FULL is the one deliberate compromise. The EEZ spans MORE THAN A HEMISPHERE
  # -- Guam sits about 180 degrees from Puerto Rico -- so no globe view can hold
  # it, and asking for the smallest enclosing circle gives a 70 degree radius
  # centred at (-143.5, 47.3): the Pacific, with the Gulf and the whole east
  # coast behind the horizon. It is the correct answer to the wrong question.
  #
  # So FULL frames the NORTH AMERICAN block instead, at the centroid of the
  # geometry, and `Pacific` is where the islands live. Stated here rather than
  # left to look like an oversight: the picker cannot show everything at once,
  # and this is which half it chooses.
  ct_full <- sphere_centroid(V$lon, V$lat)
  a_full  <- angular_distance(ct_full, V$lon, V$lat)
  rbind(row("FULL", rep(TRUE, nrow(V)), ct_full,
            view_zoom(stats::quantile(a_full, quantile_r), margin = 0)),
        do.call(rbind, lapply(sort(unique(V$region)), one_region)))
}
