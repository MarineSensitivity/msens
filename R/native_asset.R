# native_asset.R — backfill a v8-shaped `native_asset` for a v1-v7 release ----
#
# v1-v7 publish `model_asset` (mdl_seq, cog_url) and nothing else: one gridded 0.05 deg COG
# per model. `.app_assets()` used to relabel that COG `representation = 'native'`, so the
# atlas could never offer an Original | Interpolated toggle on the public release. v8/v9
# publish `native_asset` with the ORIGINAL surface (`native`: a PMTiles range or the 0.5 deg
# AquaMaps COG) next to the gridded COG (`model`). This builds the same table for v7 by
# reusing the originals a later release already publishes, matched on the stable model key.

# columns of a v8 `native_asset`, in v8's order
.NATIVE_ASSET_COLS <- c(
  "ms_merge_key", "mdl_key", "ds_key", "asset_type", "representation", "asset_url",
  "rescale_min", "rescale_max", "colormap", "xmin", "xmax", "ymin", "ymax", "source_layer")

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
#' @param taxon_key the release's `taxon` key values (v1-v7: `taxon.mdl_seq`). A `model` row
#'   whose `mdl_seq` is one of them carries it as `ms_merge_key`, which is what
#'   `.app_merged()` reads to pick a taxon's own surface: v1-v7 do that for single-dataset
#'   taxa too, not only for `ms_merge` models, and dropping it loses `merged` on those cards
#'   (found in the R4-F dry run: 6,729 v7 taxa). `NULL` falls back to `ds_key == 'ms_merge'`.
#' @return a data frame with v8's `native_asset` columns
#' @export
#' @concept app
native_asset_backfill <- function(model_asset, crosswalk, native_ref, taxon_key = NULL) {
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
    source_layer = NA_character_, stringsAsFactors = FALSE)

  nat <- native_ref[native_ref$representation == "native", , drop = FALSE]
  dup <- paste(nat$mdl_key, nat$representation, sep = "\r")
  if (anyDuplicated(dup))
    stop("native_asset_backfill(): duplicate (mdl_key, representation) in native_ref (e.g. ",
         paste(utils::head(nat$mdl_key[duplicated(dup)], 3), collapse = ", "), ")", call. = FALSE)

  inp <- ma[ma$ds_key != "ms_merge", , drop = FALSE]
  inp$ref_key <- native_key_v8(xw$key[match(inp$mdl_seq, xw$mdl_seq)])
  j <- match(inp$ref_key, nat$mdl_key)
  hit <- which(!is.na(j))
  keep <- c("asset_type", "asset_url", "rescale_min", "rescale_max", "colormap",
            "xmin", "xmax", "ymin", "ymax", "source_layer")
  n <- length(hit)
  native <- data.frame(ms_merge_key = rep(NA_character_, n), mdl_key = inp$mdl_seq[hit],
                       ds_key = inp$ds_key[hit], representation = rep("native", n),
                       nat[j[hit], keep, drop = FALSE], stringsAsFactors = FALSE)
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
