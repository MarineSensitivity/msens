# native_asset.R — backfill a v8-shaped `native_asset` for a v1-v7 release ----
#
# v1-v7 publish `model_asset` (mdl_seq, cog_url) and nothing else: one gridded 0.05 deg COG
# per model. `.app_assets()` used to relabel that COG `representation = 'native'`, so the
# atlas could never offer an Original | Interpolated toggle on the public release. v8/v9
# publish `native_asset` with the ORIGINAL surface (`native`: a PMTiles range or the 0.5 deg
# AquaMaps COG) next to the gridded COG (`model`). This builds the same table for v7 by
# reusing the originals a later release already publishes, matched on the stable model key.

# columns of a v8 `native_asset`, in v8's order, then `content_hash` (0.47.0): the 16-hex store hash
# naming the object the row points at (`cog/{grid}/{hash}.tif` or `native/{ds}/{hash}.ext`), NA
# for a row that points at a legacy versioned path; then `source_key` (0.50.0): for a PMTiles row,
# the `mdl_key` the tile's FEATURES carry (the Atlas draws a range by filtering the tile on it); NA for a COG
.NATIVE_ASSET_COLS <- c(
  "ms_merge_key", "mdl_key", "ds_key", "asset_type", "representation", "asset_url",
  "rescale_min", "rescale_max", "colormap", "xmin", "xmax", "ymin", "ymax", "source_layer",
  "content_hash", "source_key")

#' Spell a v1-v7 stable model key the way a later release's `native_asset` does
#'
#' The v1-v7 crosswalk mints `{ds_key}|{taxa}` where `taxa` carries the dataset prefix
#' (`bl|bl:22698216`, `rng_iucn|rng_iucn:Oncorhynchus keta`); v8 and later key the SAME source
#' object by its bare id, with underscores for spaces (`bl|22698216`,
#' `ca_nmfs|Balaenoptera_ricei`). AquaMaps keys (`am|Fis-29291`) are already identical.
#' This is a rule about key spelling, so it lives here where a test can assert it.
#'
#' @param mdl_key character stable model key(s) from the v1-v7 crosswalk
#' @return character key(s) in the v8 spelling
#' @examples
#' native_key_v8("bl|bl:22698216")
#' native_key_v8("ch_nmfs|ch_nmfs:Acropora globiceps")
#' @export
#' @concept app
native_key_v8 <- function(mdl_key) {
  mdl_key <- as.character(mdl_key)
  ds <- sub("\\|.*$", "", mdl_key)
  id <- sub("^[^|]*\\|", "", mdl_key)
  has_id <- grepl("|", mdl_key, fixed = TRUE)
  id <- ifelse(startsWith(id, paste0(ds, ":")), substring(id, nchar(ds) + 2L), id)
  id <- ifelse(ds == "ms_merge", id, gsub(" ", "_", id, fixed = TRUE))
  ifelse(has_id, paste(ds, id, sep = "|"), mdl_key)
}

# the unversioned, content-addressed store of ORIGINAL surfaces ----
#
#   S3:  {atlas_base}/native/{ds_key}/{hash}.tif | .pmtiles       (PMTiles are served from S3 too)
#
# Twin of cog_store.R's `cog/{grid_id}/{content_hash}.tif`: no version in the path, so a release
# that re-publishes identical content writes nothing, and every release's `native_asset`
# references the same objects. `hash` is ALWAYS the 16-hex SOURCE-content hash folded with the
# file family's encoding tag (content_hash_encoded(); tags in asset_enc()) - computed before the
# file is built. The MD5 of an object's bytes is an integrity column of the catalog
# (`assets.parquet`), never a key: a rebuilt GeoTIFF is not byte-identical, so a bytes key would
# make every re-publish look new.

#' Object key of a stored original surface
#'
#' @param ds_key dataset key (`"am"`, `"bl"`, `"rng_iucn"`, ...)
#' @param hash 16-hex source-content hash folded with the encoding tag ([content_hash_encoded()])
#' @param type `"cog"` (`.tif`) or `"pmtiles"`
#' @return relative key(s), e.g. `"native/bl/3f2a....pmtiles"`
#' @export
#' @concept cog_store
native_key <- function(ds_key, hash, type = c("cog", "pmtiles")) {
  type <- match.arg(type)
  stopifnot(length(ds_key) == 1 || length(ds_key) == length(hash),
            !anyNA(hash), all(grepl("^[0-9a-f]{16}$", hash)),
            all(grepl("^[A-Za-z0-9_]+$", ds_key)))
  sprintf("native/%s/%s.%s", ds_key, hash, if (type == "cog") "tif" else "pmtiles")
}

#' Public URL of a stored original surface
#'
#' Twin of [content_url()]. COGs AND PMTiles are served from S3 (S3 answers range requests with
#' CORS, as the zone PMTiles already prove); `pmtiles_base` exists only to point a test or a
#' legacy mirror elsewhere, and defaults to `base`.
#'
#' @param ds_key,hash,type see [native_key()]
#' @param base S3 atlas base URL, from [atlas_base_url()]
#' @param pmtiles_base PMTiles root (no trailing slash); default `base`
#' @return absolute `https://` URL(s)
#' @export
#' @concept cog_store
native_url <- function(ds_key, hash, type = c("cog", "pmtiles"), base = atlas_base_url(),
                       pmtiles_base = base) {
  type <- match.arg(type)
  k <- native_key(ds_key, hash, type)
  if (type == "cog") sprintf("%s/%s", base, k)
  else sprintf("%s/%s", pmtiles_base, k)
}

#' Index the store of original surfaces
#'
#' Twin of [cog_store_index()]: ONE recursive listing into a set so "already published?" is a
#' lookup. Both `.tif` and `.pmtiles` are indexed.
#'
#' @param ds_key restrict to one dataset's prefix, or `NULL` for the whole store
#' @param bucket S3 URI of the atlas root
#' @param aws path to the `aws` CLI
#' @return character vector of `hash` values already present
#' @export
#' @concept cog_store
native_store_index <- function(ds_key = NULL,
                               bucket = "s3://oceanmetrics.io-public/marine-atlas",
                               aws = "aws") {
  pre <- if (is.null(ds_key)) sprintf("%s/native/", bucket) else sprintf("%s/native/%s/", bucket, ds_key)
  out <- suppressWarnings(system2(aws, c("s3", "ls", "--recursive", shQuote(pre)),
                                  stdout = TRUE, stderr = TRUE))
  txt <- out[nzchar(trimws(out))]
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    if (length(txt))
      stop(sprintf("listing '%s' failed: %s", pre, paste(utils::tail(txt, 3), collapse = " ")),
           call. = FALSE)
    return(character())
  }
  keys <- sub("^.*\\s+", "", out[nzchar(out)])
  # the release-scoped v8/v9 layout also lives under native/; only {ds_key}/{hash}.ext counts
  keys <- keys[grepl("/[0-9a-f]{16}\\.(tif|pmtiles)$", keys)]
  unique(sub("\\.[^.]*$", "", basename(keys)))
}

#' Rewrite original-surface rows to the content-addressed store
#'
#' Replaces `asset_url` of every `native` row by [native_url()] of its `hash`, looked up by the
#' row's current URL in `map`. A row with no mapping is an ERROR, never passed through: a
#' release must not silently keep pointing at a versioned path.
#'
#' @param native_ref a `native_asset` (v8 shape)
#' @param map data frame `asset_url` (current URL), `hash`
#' @param ... passed to [native_url()] (`base`, `pmtiles_base`)
#' @return `native_ref` with store URLs on its `native` rows, and `content_hash` set to the store
#'   hash on those rows
#' @export
#' @concept cog_store
native_store_rewrite <- function(native_ref, map, ...) {
  stopifnot(all(c("asset_url", "hash") %in% names(map)), !anyDuplicated(map$asset_url))
  isn <- native_ref$representation == "native"
  h <- map$hash[match(native_ref$asset_url[isn], map$asset_url)]
  if (anyNA(h))
    stop(sprintf("native_store_rewrite(): %d native row(s) have no store mapping (e.g. %s)",
                 sum(is.na(h)), paste(utils::head(native_ref$asset_url[isn][is.na(h)], 2), collapse = ", ")),
         call. = FALSE)
  ty <- ifelse(native_ref$asset_type[isn] == "pmtiles", "pmtiles", "cog")
  native_ref$asset_url[isn] <- vapply(seq_along(h), function(i)
    native_url(native_ref$ds_key[isn][i], h[i], ty[i], ...), "")
  if (!"content_hash" %in% names(native_ref)) native_ref$content_hash <- rep(NA_character_, nrow(native_ref))
  native_ref$content_hash[isn] <- h
  native_ref
}

#' MD5 of an object's bytes (the catalog's integrity column, never a store key)
#'
#' For a single-part S3 upload this equals the object's ETag, so a copy can be checked without
#' downloading it. A rebuilt GeoTIFF is not byte-identical, which is why it is not the key.
#'
#' @param path local file(s)
#' @return 32-hex MD5(s)
#' @importFrom digest digest
#' @export
#' @concept cog_store
native_hash_file <- function(path)
  vapply(path, function(p) digest::digest(p, algo = "md5", file = TRUE), "", USE.NAMES = FALSE)

#' Backfill a v8-shaped `native_asset` for a `model_asset`-only release
#'
#' Produces one `representation = 'model'` row per model in `model_asset` (the gridded COG,
#' rescale 1-100, `spectral_r`, no bbox: exactly the constants `.app_assets()` substituted
#' before) and, for every INPUT model whose original is published by a reference release,
#' one `representation = 'native'` row copied from it (URL, type, `source_layer`, rescale,
#' colormap, bbox). Merged (`ms_merge`) models get only the `model` row, as in v8. `native`
#' rows carry no `ms_merge_key` (they must never be mistaken for a taxon's own surface). Rows are
#' keyed by `mdl_key = as.character(mdl_seq)`, the way v1-v7's `taxon_model` keys models,
#' so `app_taxon_shards()`'s `mdl_key`-alone asset join still finds them. Inputs with no
#' matching original keep their single `model` row: the app then offers no toggle, and
#' nothing is invented.
#'
#' Only a reference original from the SAME source vintage is honest to reuse: the caller
#' checks that (the `dataset` tables), this function only matches keys.
#'
#' @param model_asset data frame with `mdl_seq`, `ds_key`, `cog_url` (the release's own)
#' @param crosswalk data frame with `mdl_seq` and the stable `mdl_key` (v1-v7 spelling)
#' @param native_ref a later release's `native_asset` (v8 shape); only rows with
#'   `representation == 'native'` are used
#' @param store data frame `asset_url` (the reference row's current URL), `hash`: where each
#'   original lives in the content-addressed store ([native_store_rewrite()]). REQUIRED: a matched
#'   original with no mapping is an error, so a release never points at a versioned path.
#' @param taxon_key the release's `taxon` key values (v1-v7: `taxon.mdl_seq`). A `model` row
#'   whose `mdl_seq` is one of them carries it as `ms_merge_key`, which is what
#'   `.app_merged()` reads to pick a taxon's own surface: v1-v7 do that for single-dataset
#'   taxa too, not only for `ms_merge` models, and dropping it loses `merged` on those cards
#'   (found in the R4-F dry run: 6,729 v7 taxa). `NULL` falls back to `ds_key == 'ms_merge'`.
#' @param key_map optional data frame `mdl_seq`, `ref_key`: v1-v7 models whose original is NOT reachable by
#'   [native_key_v8()] spelling, matched some other way (IUCN ranges key by scientific NAME in v1-v7 and by
#'   `id_no` in v8+, so the caller matches them by an exact, unambiguous name and passes the v8 `mdl_key`
#'   here). Overrides the derived key for those models only; a `ref_key` absent from `native_ref` is an error.
#' @param ... passed to [native_url()] (`base`, `pmtiles_base`)
#' @return a data frame with v8's `native_asset` columns
#' @export
#' @concept app
native_asset_backfill <- function(model_asset, crosswalk, native_ref, store, taxon_key = NULL, key_map = NULL, ...) {
  stopifnot(is.data.frame(model_asset), is.data.frame(crosswalk), is.data.frame(native_ref),
            all(c("mdl_seq", "ds_key", "cog_url") %in% names(model_asset)),
            all(c("mdl_seq", "mdl_key") %in% names(crosswalk)),
            all(c("mdl_key", "representation", "asset_type", "asset_url") %in% names(native_ref)))
  seq_chr <- function(x) format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  ma <- data.frame(mdl_seq = seq_chr(model_asset$mdl_seq),
                   ds_key = as.character(model_asset$ds_key),
                   cog_url = as.character(model_asset$cog_url), stringsAsFactors = FALSE)
  if (anyDuplicated(ma$mdl_seq))
    stop("native_asset_backfill(): duplicated mdl_seq in model_asset (e.g. ",
         paste(utils::head(ma$mdl_seq[duplicated(ma$mdl_seq)], 3), collapse = ", "), ")",
         call. = FALSE)
  xw <- data.frame(mdl_seq = seq_chr(crosswalk$mdl_seq), key = as.character(crosswalk$mdl_key),
                   stringsAsFactors = FALSE)
  if (anyDuplicated(xw$mdl_seq) || anyDuplicated(xw$key))
    stop("native_asset_backfill(): crosswalk maps one mdl_seq or one mdl_key twice", call. = FALSE)

  model <- data.frame(
    ms_merge_key = if (is.null(taxon_key)) ifelse(ma$ds_key == "ms_merge", ma$mdl_seq, NA_character_)
                   else ifelse(ma$mdl_seq %in% seq_chr(taxon_key[!is.na(taxon_key)]), ma$mdl_seq,
                               NA_character_),
    mdl_key = ma$mdl_seq, ds_key = ma$ds_key, asset_type = "cog", representation = "model",
    asset_url = ma$cog_url, rescale_min = 1L, rescale_max = 100L, colormap = "spectral_r",
    xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_,
    source_layer = NA_character_,
    content_hash = ifelse(grepl("/cog/[A-Za-z0-9]+/[0-9a-f]{16}\\.tif$", ma$cog_url),
                          sub("^.*/([0-9a-f]{16})\\.tif$", "\\1", ma$cog_url), NA_character_),
    source_key = NA_character_,
    stringsAsFactors = FALSE)

  nat <- native_ref[native_ref$representation == "native", , drop = FALSE]
  # the key the reference tile's features carry: its own `source_key`, else (a v8/v9 table that predates the column) the
  # row's `mdl_key`, which v8+ stamps into every feature. v1-v7 key the SAME model by `mdl_seq`, so the original's row
  # must tell the app which feature key to filter on.
  if (!"source_key" %in% names(nat)) nat$source_key <- rep(NA_character_, nrow(nat))
  nat$source_key <- ifelse(nat$asset_type == "pmtiles" & is.na(nat$source_key), as.character(nat$mdl_key), nat$source_key)
  nat$source_key[nat$asset_type != "pmtiles"] <- NA_character_
  dup <- paste(nat$mdl_key, nat$representation, sep = "\r")
  if (anyDuplicated(dup))
    stop("native_asset_backfill(): duplicate (mdl_key, representation) in native_ref (e.g. ",
         paste(utils::head(nat$mdl_key[duplicated(dup)], 3), collapse = ", "), ")", call. = FALSE)

  inp <- ma[ma$ds_key != "ms_merge", , drop = FALSE]
  inp$ref_key <- native_key_v8(xw$key[match(inp$mdl_seq, xw$mdl_seq)])
  if (!is.null(key_map)) {
    stopifnot(is.data.frame(key_map), all(c("mdl_seq", "ref_key") %in% names(key_map)))
    km <- data.frame(mdl_seq = seq_chr(key_map$mdl_seq), ref_key = as.character(key_map$ref_key), stringsAsFactors = FALSE)
    if (anyDuplicated(km$mdl_seq)) stop("native_asset_backfill(): key_map names a model twice", call. = FALSE)
    if (anyDuplicated(km$ref_key)) stop("native_asset_backfill(): key_map points two models at one original", call. = FALSE)
    if (!all(km$ref_key %in% nat$mdl_key))
      stop("native_asset_backfill(): key_map names an original that native_ref does not hold (e.g. ",
           paste(utils::head(setdiff(km$ref_key, nat$mdl_key), 2), collapse = ", "), ")", call. = FALSE)
    if (!all(km$mdl_seq %in% inp$mdl_seq)) stop("native_asset_backfill(): key_map names a model that is not an input of this release", call. = FALSE)
    inp$ref_key[match(km$mdl_seq, inp$mdl_seq)] <- km$ref_key
  }
  j <- match(inp$ref_key, nat$mdl_key)
  hit <- which(!is.na(j))
  keep <- c("asset_type", "asset_url", "rescale_min", "rescale_max", "colormap",
            "xmin", "xmax", "ymin", "ymax", "source_layer", "content_hash", "source_key")
  n <- length(hit)
  sel <- native_store_rewrite(nat[j[hit], , drop = FALSE], store, ...)   # store URLs, never the reference's
  native <- data.frame(ms_merge_key = rep(NA_character_, n), mdl_key = inp$mdl_seq[hit],
                       ds_key = inp$ds_key[hit], representation = rep("native", n),
                       sel[, keep, drop = FALSE], stringsAsFactors = FALSE)
  native <- native[, .NATIVE_ASSET_COLS, drop = FALSE]
  out <- rbind(model[, .NATIVE_ASSET_COLS], native)
  rownames(out) <- NULL
  out[order(out$mdl_key, out$representation), , drop = FALSE]
}

#' Is a dataset the same source vintage in two releases?
#'
#' Reusing a reference release's original surface for another release is only honest when
#' both were gridded from the same source. Compares the `dataset` tables' vintage columns
#' (`year_pub`, `date_created`, `date_obs_*`, `date_env_*`, `citation`, `source_detail`) per
#' `ds_key` (legacy `am_0.05` is normalised to `am`).
#'
#' @param ds a release's `dataset` table
#' @param ds_ref the reference release's `dataset` table
#' @return data frame `ds_key`, `same` (`TRUE`/`FALSE`, `NA` when the reference lacks the
#'   dataset) and `differs` (the columns that disagree, `""` when none)
#' @export
#' @concept app
native_vintage_check <- function(ds, ds_ref) {
  cols <- c("year_pub", "date_created", "date_obs_beg", "date_obs_end", "date_env_beg",
            "date_env_end", "citation", "source_detail")
  cols <- intersect(cols, intersect(names(ds), names(ds_ref)))
  norm <- function(x) ifelse(is.na(x), "", as.character(x))
  ds$ds_key     <- normalize_ds_key(as.character(ds$ds_key))
  ds_ref$ds_key <- normalize_ds_key(as.character(ds_ref$ds_key))
  i <- match(ds$ds_key, ds_ref$ds_key)
  differs <- vapply(seq_len(nrow(ds)), function(r) {
    if (is.na(i[r])) return(NA_character_)
    paste(cols[vapply(cols, function(cl) !identical(norm(ds[[cl]][r]), norm(ds_ref[[cl]][i[r]])),
                      logical(1))], collapse = ",")
  }, "")
  data.frame(ds_key = ds$ds_key, same = ifelse(is.na(differs), NA, differs == ""),
             differs = differs, stringsAsFactors = FALSE)
}

#' Restore the gridded (`model`) representation of vector ranges
#'
#' A vector range is published twice: the source polygons (`native`, PMTiles) and the same range gridded onto
#' the 0.05 degree scoring grid (`model`, COG). v8 registers both; v9's `native_asset` lost every vector
#' `model` row, so v9 shows no Interpolated view of any range although the gridded COGs were published (and are
#' byte-identical to v8's). This adds, for every `native` PMTiles row that has no `model` row for the same
#' `mdl_key`, one `model` COG row: same `ds_key`, `ms_merge_key`, bbox and `source_layer`, `rescale_min/max` 1-100,
#' `colormap` `spectral_r`, and the URL (and `content_hash`) from `model_urls`. A PMTiles row with no URL in
#' `model_urls` is an ERROR, never skipped: a release must not silently lose a representation twice.
#'
#' @param native_asset a v8-shaped `native_asset` (with `content_hash` once re-pointed to the store)
#' @param model_urls data frame `mdl_key`, `asset_url` (+ optional `content_hash`): the gridded COG of each range
#' @return `native_asset` with the missing `model` rows appended
#' @export
#' @concept app
native_asset_restore_model <- function(native_asset, model_urls) {
  stopifnot(is.data.frame(native_asset), is.data.frame(model_urls),
            all(c("mdl_key", "asset_url") %in% names(model_urls)), !anyDuplicated(model_urls$mdl_key),
            all(c("mdl_key", "ds_key", "asset_type", "representation") %in% names(native_asset)))
  vec <- native_asset[native_asset$representation == "native" & native_asset$asset_type == "pmtiles", , drop = FALSE]
  have <- native_asset$mdl_key[native_asset$representation == "model"]
  need <- vec[!vec$mdl_key %in% have, , drop = FALSE]
  if (!nrow(need)) return(native_asset)
  u <- model_urls$asset_url[match(need$mdl_key, model_urls$mdl_key)]
  if (anyNA(u))
    stop(sprintf("native_asset_restore_model(): %d range(s) have no gridded COG in model_urls (e.g. %s)",
                 sum(is.na(u)), paste(utils::head(need$mdl_key[is.na(u)], 2), collapse = ", ")), call. = FALSE)
  add <- need
  add$asset_type <- "cog"; add$representation <- "model"; add$asset_url <- u
  if ("source_key" %in% names(add)) add$source_key <- NA_character_
  add$rescale_min <- 1L; add$rescale_max <- 100L; add$colormap <- "spectral_r"; add$source_layer <- NA_character_
  if ("content_hash" %in% names(native_asset))
    add$content_hash <- if ("content_hash" %in% names(model_urls)) model_urls$content_hash[match(need$mdl_key, model_urls$mdl_key)] else NA_character_
  out <- rbind(native_asset, add[, names(native_asset), drop = FALSE])
  rownames(out) <- NULL
  out
}
