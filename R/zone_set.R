# zone_set.R — spatial units as VINTAGES, independent of MST version
#
# BOEM's planning units keep morphing, and every release so far baked its own
# copy into a version-suffixed file (ply_programareas_2026_v3.gpkg ... _v8.gpkg)
# and its own `zone_cell`. That makes the interesting question — how did THIS
# Program Area's score change from v3 to v8? — unanswerable, because nothing
# guarantees the two releases meant the same polygon.
#
# A zone set is therefore identified by its GEOMETRY, not by the release that
# happened to use it:
#
#   zone_set_key = {zone_type}_{YYYY-MM}    e.g. "programarea_2026-03"
#
# Measured across every published gpkg (2026-08-10), the per-version files are
# mostly the same layer copied forward:
#
#   programarea   8 files -> 2 distinct geometries (v2 alone; v3-v8 IDENTICAL)
#   ecoregion    10 files -> 2 distinct (v1-v8 identical, + one standalone)
#   planarea      3 files -> 3 distinct
#
# So `zone_cell` — which depends only on (geometry x grid) — is computed ONCE per
# (zone_set_key, grid_id) and reused by every release on that grid, instead of
# being re-extracted per version as score_zones.qmd does today.
#
# CAVEAT on the hash: it is exact-WKB, so a re-export at different coordinate
# precision reads as a NEW vintage even if the polygons are cartographically the
# same. In practice that has not bitten (the six v3-v8 program-area files were
# written on six different dates and hash identically), but when two hashes
# differ and a human believes they are the same unit, that is a CURATION call —
# record it in the registry, do not loosen the hash.

.ZONE_TYPES <- c("planarea", "programarea", "ecoregion", "subregion")

#' The key column for a zone type
#'
#' A zone gpkg typically carries SEVERAL `*_key` columns — the program-area
#' layer has `region_key`, `planarea_key` and `programarea_key` — so "the first
#' column ending in key" picks the wrong one. That is not a cosmetic error:
#' taking `region_key` (3 values) for the program-area layer yields 3 zones
#' instead of 20, and, because the ordering it induces is not total, leaves ties
#' whose order depends on the file's feature order — so two byte-identical
#' layers saved in different order could fingerprint differently.
#'
#' @param zone_type one of [zone_set_key()]'s types
#' @param nms column names of the layer
#' @return the matching column name
#' @export
#' @concept zone_set
zone_key_col <- function(zone_type, nms) {
  want <- paste0(zone_type, "_key")
  if (want %in% nms) return(want)
  alt <- grep("key$", nms, value = TRUE)
  if (!length(alt))
    stop(sprintf("no `%s` and no *_key column among: %s", want,
                 paste(nms, collapse = ", ")), call. = FALSE)
  alt[1]
}

#' Order-invariant geometry fingerprint of a zone layer
#'
#' Hashes each feature's WKB, ordered by the zone key (not by row order), so a
#' layer re-saved with features shuffled fingerprints identically while any real
#' change to a boundary does not.
#'
#' @param x an `sf` object, or a path to a vector file (e.g. `.gpkg`)
#' @param key_col column holding the zone key; resolved from `zone_type` when
#'   given, else the first column whose name ends in `key`, else row order
#' @param zone_type when given, resolves `key_col` via [zone_key_col()] — the
#'   correct choice when a layer carries several `*_key` columns
#' @param layer layer name when `x` is a multi-layer file (default: the first)
#' @return a list with `n`, `key_col`, `keys` and a 16-char `geom_hash`
#' @importFrom sf st_read st_layers st_geometry st_as_binary
#' @importFrom digest digest
#' @export
#' @concept zone_set
zone_geom_hash <- function(x, key_col = NULL, zone_type = NULL, layer = NULL) {
  if (is.character(x)) {
    if (!file.exists(x)) stop(sprintf("no such file: %s", x), call. = FALSE)
    if (is.null(layer)) layer <- sf::st_layers(x)$name[1]
    x <- sf::st_read(x, layer = layer, quiet = TRUE)
  }
  if (is.null(key_col)) {
    key_col <- if (!is.null(zone_type)) zone_key_col(zone_type, names(x)) else {
      kc <- grep("key$", names(x), value = TRUE)
      if (length(kc)) kc[1] else NA_character_
    }
  }
  k <- if (!is.na(key_col) && key_col %in% names(x))
    as.character(x[[key_col]]) else as.character(seq_len(nrow(x)))
  o <- order(k)
  list(n = nrow(x), key_col = key_col, keys = sort(k),
       geom_hash = substr(digest::digest(
         lapply(sf::st_as_binary(sf::st_geometry(x)[o]), as.vector),
         algo = "xxhash64"), 1, 16))
}

#' Resolve a release's zone columns to zone-set keys via the registry
#'
#' Only v8 stamps `zone_set_key` into its own `zone` table; v1–v7 identify a
#' spatial unit by `fld` (`programarea_key`) and a per-version table name
#' (`ply_programareas_2026_v7`). This maps the former to the vintage that release
#' actually used, by matching `zone_type` and finding `ver` in the registry's
#' space-separated `versions` column.
#'
#' Returns `NA` rather than guessing when a release's zone type is absent from
#' the registry, or when the registry lists more than one vintage of that type
#' for the same version. A wrong outline is worse than a missing one: it would
#' draw the 2026 Program Areas over scores computed on a different geometry and
#' look entirely plausible.
#'
#' @param ver MST version, e.g. `"v7"`
#' @param fld zone key column(s) from the release's `zone` table
#' @param zone_sets the registry (`data/zone_sets.csv`)
#' @return character vector of `zone_set_key`, `NA` where unresolvable
#' @export
#' @concept zone_set
zone_set_resolve <- function(ver, fld, zone_sets) {
  stopifnot(all(c("zone_set_key", "zone_type", "versions") %in% names(zone_sets)))
  zt  <- sub("_key$", "", fld)
  vapply(zt, function(t) {
    hit <- zone_sets[zone_sets$zone_type == t &
                     vapply(strsplit(as.character(zone_sets$versions), "\\s+"),
                            function(vs) ver %in% vs, logical(1)), , drop = FALSE]
    if (nrow(hit) == 1L) as.character(hit$zone_set_key) else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Compose (and validate) a zone-set key
#'
#' @param zone_type one of `planarea`, `programarea`, `ecoregion`, `subregion`
#' @param vintage `YYYY-MM`
#' @return `"{zone_type}_{YYYY-MM}"`
#' @export
#' @concept zone_set
zone_set_key <- function(zone_type, vintage) {
  if (any(bad <- !zone_type %in% .ZONE_TYPES))
    stop(sprintf("unknown zone_type %s; known: %s",
                 paste(sQuote(unique(zone_type[bad])), collapse = ", "),
                 paste(.ZONE_TYPES, collapse = ", ")), call. = FALSE)
  if (any(!grepl("^[0-9]{4}-[0-9]{2}$", vintage)))
    stop("vintage must be YYYY-MM", call. = FALSE)
  sprintf("%s_%s", zone_type, vintage)
}

#' Validate a zone-set registry table
#'
#' Enforces the two invariants that make cross-version comparison meaningful:
#' one key names exactly one geometry, and one geometry carries exactly one key.
#' Without the second, the same polygons published under two keys would be scored
#' twice and compared as though they were different places.
#'
#' @param d data frame with `zone_set_key`, `zone_type`, `vintage`, `geom_hash`,
#'   `n_zones`
#' @return `d`, invisibly
#' @export
#' @concept zone_set
validate_zone_sets <- function(d) {
  req <- c("zone_set_key", "zone_type", "vintage", "geom_hash", "n_zones")
  if (length(miss <- setdiff(req, names(d))))
    stop(sprintf("zone-set registry missing column(s): %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  if (any(dup <- duplicated(d$zone_set_key)))
    stop(sprintf("duplicate zone_set_key: %s",
                 paste(unique(d$zone_set_key[dup]), collapse = ", ")), call. = FALSE)

  # one key -> one geometry
  by_key <- tapply(d$geom_hash, d$zone_set_key, function(z) length(unique(z)))
  if (any(by_key > 1))
    stop(sprintf("zone_set_key(s) map to >1 geometry: %s",
                 paste(names(by_key)[by_key > 1], collapse = ", ")), call. = FALSE)

  # one geometry -> one key (else the same polygons get scored twice)
  by_geom <- tapply(d$zone_set_key, d$geom_hash, function(z) length(unique(z)))
  if (any(by_geom > 1)) {
    h <- names(by_geom)[by_geom > 1][1]
    stop(sprintf("geometry %s is published under >1 key (%s); collapse them to one vintage",
                 h, paste(unique(d$zone_set_key[d$geom_hash == h]), collapse = ", ")),
         call. = FALSE)
  }
  invisible(zone_set_key(d$zone_type, d$vintage) -> k) # re-validates type + vintage
  if (!identical(as.character(k), as.character(d$zone_set_key)))
    stop("zone_set_key does not match its zone_type/vintage columns", call. = FALSE)
  invisible(d)
}

#' Cells covered by each zone, on a given grid
#'
#' The (zone × grid) intersection — which is a function of **geometry and grid
#' alone**, not of any release. `score_zones.qmd` recomputes it per version, so
#' the six releases sharing `programarea_2026-03` each paid for the same
#' `exactextractr` pass; keyed on `(zone_set_key, grid_id)` it is computed once.
#'
#' Reads the grid's **cell-id COG**, whose pixel VALUES are cell ids (a lookup
#' image — see `grid.R`), so the returned ids are in that grid's id-space
#' whatever frame the raster itself uses.
#'
#' Semantics deliberately match the per-version implementation this replaces, so
#' relocating it cannot move any score: coverage is rounded to whole percent and
#' anything rounding to zero is dropped.
#'
#' @param ply an `sf` of zone polygons
#' @param cellid_tif path to that grid's cell-id COG
#' @param key_col column of `ply` holding the zone key
#' @return a data frame with `zone_key`, `cell_id`, `pct_covered` (1-100)
#' @importFrom sf st_make_valid st_transform st_geometry
#' @export
#' @concept zone_set
zone_cells <- function(ply, cellid_tif, key_col) {
  if (!requireNamespace("exactextractr", quietly = TRUE))
    stop("zone_cells() needs the exactextractr package", call. = FALSE)
  if (!key_col %in% names(ply))
    stop(sprintf("`%s` not a column of ply", key_col), call. = FALSE)

  r   <- terra::rast(cellid_tif)
  ply <- sf::st_transform(sf::st_make_valid(ply), 4326)

  out <- lapply(seq_len(nrow(ply)), function(i) {
    d <- exactextractr::exact_extract(r, ply[i, ], progress = FALSE)[[1]]
    d <- d[!is.na(d$value) & d$coverage_fraction > 0, , drop = FALSE]
    if (!nrow(d)) return(NULL)
    pct <- as.integer(round(d$coverage_fraction * 100))
    keep <- pct > 0                      # a sliver rounding to 0% is not coverage
    if (!any(keep)) return(NULL)
    data.frame(zone_key    = as.character(ply[[key_col]][i]),
               cell_id     = as.integer(d$value[keep]),
               pct_covered = pct[keep],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Group zone layers into distinct vintages by geometry
#'
#' Takes the fingerprints of many candidate layers and reports how many genuinely
#' distinct geometries they contain — the measurement that decides how many zone
#' sets must be scored, rather than assuming one per release.
#'
#' @param x data frame with `source`, `zone_type` and `geom_hash`
#' @param vintage_of named character mapping `geom_hash` -> `YYYY-MM`; hashes not
#'   named here are left `NA` for a human to label
#' @return `x` with `zone_set_key` and `vintage` columns
#' @export
#' @concept zone_set
zone_set_group <- function(x, vintage_of = character()) {
  if (!all(c("zone_type", "geom_hash") %in% names(x)))
    stop("need `zone_type` and `geom_hash` columns", call. = FALSE)
  x$vintage <- unname(vintage_of[x$geom_hash])
  ok <- !is.na(x$vintage)
  x$zone_set_key <- NA_character_
  if (any(ok)) x$zone_set_key[ok] <- zone_set_key(x$zone_type[ok], x$vintage[ok])
  x
}
