# mdl_key.R — stable model identifiers (mdl_key) ----
#
# A model's stable, URL-safe public id, replacing the old auto-increment `mdl_seq`
# (which re-numbered on every rebuild and dropped the prior id, breaking
# model-revisit URLs across versions — apps/titiler key on it). Format:
#
#   mdl_key = {dataset_key}|{sp_id}[|{interval}]
#
# Fields are pipe-`|`-separated (dash `-` occurs inside AquaMaps sp_keys). RAW
# per-dataset models use the dataset-NATIVE species/guild id (some NCCOS models are
# guilds, not resolvable to a single worms_id): `am|Fis-29291`, `gm|1234|01`
# (monthly), `nc|kelp-guild|summer` (seasonal). MERGED `ms_merge` models use the
# taxon id with a taxadb AUTHORITY prefix: `ms_merge|WORMS:137209`,
# `ms_merge|BOTW:22694927` (also `ITIS:`/`GBIF:`/`SLB:` for crosswalking).

# field separator + taxadb authorities usable as ms_merge prefixes (internal)
.MDL_KEY_SEP <- "|"
.TAXON_AUTHORITIES <- c("WORMS", "BOTW", "ITIS", "GBIF", "SLB")

#' Normalise a legacy dataset key to the `mdl_key` grammar
#'
#' v1–v7 spell AquaMaps `am_0.05` (the 0.5° source resolution baked into the
#' name); the `mdl_key` grammar uses `am`. Minting a key from the raw legacy
#' string yields `am_0.05|Fis-29291`, which matches nothing in v8 — so a
#' crosswalk built without this silently fails to join the two generations.
#'
#' Lives here rather than inline in the backfill notebook because it is a rule
#' about the key grammar, and rules belong in the package where a test can
#' assert them.
#'
#' @param dataset_key character vector of dataset keys, legacy or current
#' @return the same vector with legacy spellings normalised
#' @export
#' @concept mdl_key
normalize_ds_key <- function(dataset_key)
  sub("^am_0\\.05$", "am", dataset_key)

#' Compose a raw (per-dataset) model key
#'
#' The stable id for a native, per-dataset model: `{dataset_key}|{sp_id}` with an
#' optional trailing `|{interval}` for time-resolved models (monthly `gm`, seasonal
#' `nc`). Vectorised over `sp_id` (and `interval`).
#'
#' @param dataset_key scalar dataset key, e.g. `"am"`, `"gm"`, `"nc"`, `"botw"`
#' @param sp_id dataset-native species/guild id(s) (character or coercible)
#' @param interval optional interval label(s), e.g. month `"01"` or season `"summer"`
#' @return character `mdl_key`(s)
#' @examples
#' mdl_key_raw("am", "Fis-29291")   # "am|Fis-29291"
#' mdl_key_raw("gm", 1234, "01")    # "gm|1234|01"
#' @export
#' @concept mdl_key
mdl_key_raw <- function(dataset_key, sp_id, interval = NULL) {
  # vectorised, like mdl_key_merged(): a backfill mints tens of thousands of keys
  # at once, and the scalar-only form failed inside mutate() with an opaque
  # stopifnot rather than saying it wanted one value.
  stopifnot(
    length(dataset_key) == 1L || length(dataset_key) == length(sp_id),
    all(nzchar(dataset_key)),
    !any(grepl("|", dataset_key, fixed = TRUE)))   # must not contain the separator
  key <- paste(dataset_key, sp_id, sep = .MDL_KEY_SEP)
  if (!is.null(interval)) key <- paste(key, interval, sep = .MDL_KEY_SEP)
  key
}

#' Compose a merged (`ms_merge`) model key
#'
#' The stable id for a cross-dataset merged model: the taxon id with a **taxadb
#' authority prefix**, `ms_merge|{AUTHORITY}:{taxon_id}`. Vectorised over
#' `taxon_id` (and `taxon_authority`).
#'
#' @param taxon_authority taxadb authority, one of `"WORMS"`, `"BOTW"`, `"ITIS"`,
#'   `"GBIF"`, `"SLB"` (case-insensitive)
#' @param taxon_id taxon id(s) in that authority
#' @return character `mdl_key`(s), e.g. `"ms_merge|WORMS:137209"`
#' @examples
#' mdl_key_merged("WORMS", 137209)  # "ms_merge|WORMS:137209"
#' mdl_key_merged("botw", 22694927) # "ms_merge|BOTW:22694927"
#' @export
#' @concept mdl_key
mdl_key_merged <- function(taxon_authority, taxon_id) {
  taxon_authority <- toupper(taxon_authority)
  stopifnot(all(taxon_authority %in% .TAXON_AUTHORITIES))
  paste0("ms_merge", .MDL_KEY_SEP, taxon_authority, ":", taxon_id)
}

#' Parse `mdl_key`(s) into components
#'
#' Inverse of [mdl_key_raw()] / [mdl_key_merged()]: splits on `|` into
#' `dataset_key`, `sp_id`, `interval`; for `ms_merge` keys the `sp_id`
#' (`AUTHORITY:id`) is further split into `taxon_authority` + `taxon_id`.
#'
#' @param mdl_key character `mdl_key`(s)
#' @return a tibble with columns `mdl_key`, `dataset_key`, `sp_id`, `interval`,
#'   `taxon_authority`, `taxon_id` (`NA` where not applicable)
#' @examples
#' mdl_key_parse(c("am|Fis-29291", "gm|1234|01", "ms_merge|WORMS:137209"))
#' @export
#' @concept mdl_key
#' @importFrom tibble tibble
mdl_key_parse <- function(mdl_key) {
  parts <- strsplit(as.character(mdl_key), .MDL_KEY_SEP, fixed = TRUE)
  first    <- function(p, i) if (length(p) >= i) p[[i]] else NA_character_
  dataset_key <- vapply(parts, first, "", 1L)
  sp_id       <- vapply(parts, first, "", 2L)
  interval    <- vapply(parts, first, "", 3L)

  is_merge <- !is.na(dataset_key) & dataset_key == "ms_merge"
  taxon_authority <- taxon_id <- rep(NA_character_, length(parts))
  if (any(is_merge)) {
    sub <- strsplit(sp_id[is_merge], ":", fixed = TRUE)
    taxon_authority[is_merge] <- vapply(sub, first, "", 1L)
    taxon_id[is_merge]        <- vapply(sub, function(s) paste(s[-1L], collapse = ":"), "")
  }
  tibble::tibble(
    mdl_key         = as.character(mdl_key),
    dataset_key     = dataset_key,
    sp_id           = sp_id,
    interval        = interval,
    taxon_authority = taxon_authority,
    taxon_id        = taxon_id)
}

#' Assign the dense integer `mdl_id` for a set of models
#'
#' `mdl_id` is the compact partition key of the serving surface
#' (`serve/model_cell/mdl_id={id}/`), so a titiler tile is a partition-pruned point
#' read. The **stable public identifier stays `mdl_key`**: titiler resolves
#' `mdl_key -> mdl_id` from the published `model` registry, so `mdl_id` never
#' appears in a URL.
#'
#' That indirection has a sharp edge. A plain `dense_rank(mdl_key)` is a
#' deterministic function of the model *set*, which means **adding any model
#' renumbers every model sorted after it**. Ingesting `gm` + `nc` into a released
#' version's `dist/` — registered but deliberately not merged — moved 45,499 of
#' v8's 80,261 `mdl_id`s, and nothing would have failed: the registry and the
#' `serve/` partitions would simply disagree, and titiler would serve the wrong
#' species' distribution for every model past `ch_nmfs`.
#'
#' So ids are assigned **against the published registry when one exists**: a
#' `mdl_key` already published keeps its id, and new keys are appended above the
#' current maximum in sorted order. Deterministic given
#' (published registry, model set), and it can never invalidate a published
#' partition. With `published = NULL` (a version that has never shipped) it falls
#' back to `dense_rank` over the sorted keys, which is what produced the ids of
#' every release to date.
#'
#' @param mdl_key character vector of model keys (need not be unique or sorted)
#' @param published optional data frame of the already-published registry with
#'   columns `mdl_key` and `mdl_id`; ids for keys present here are preserved
#' @return integer vector of `mdl_id`, parallel to `mdl_key`
#' @examples
#' assign_mdl_id(c("b", "a", "c"))                                     # 2 1 3
#' assign_mdl_id(c("b", "a", "c"),
#'               data.frame(mdl_key = c("a", "c"), mdl_id = c(1L, 2L))) # 3 1 2
#' @export
#' @concept mdl_key
assign_mdl_id <- function(mdl_key, published = NULL) {
  mdl_key <- as.character(mdl_key)
  keys    <- sort(unique(mdl_key))
  if (is.null(published) || !nrow(published)) {
    ids <- stats::setNames(seq_along(keys), keys)
    return(unname(as.integer(ids[mdl_key])))
  }
  if (!all(c("mdl_key", "mdl_id") %in% names(published)))
    stop("`published` must have columns `mdl_key` and `mdl_id`", call. = FALSE)
  pub <- published[!duplicated(published$mdl_key), c("mdl_key", "mdl_id")]
  if (anyDuplicated(pub$mdl_id))
    stop("`published` has duplicate `mdl_id` — it is not a valid partition key", call. = FALSE)
  ids <- stats::setNames(as.integer(pub$mdl_id), as.character(pub$mdl_key))

  # new keys go above the published maximum, in sorted order, so no published
  # partition is ever renumbered
  new <- setdiff(keys, names(ids))
  if (length(new))
    ids <- c(ids, stats::setNames(max(ids, 0L) + seq_along(new), sort(new)))
  unname(ids[mdl_key])
}

#' Which datasets actually fed the scored surface?
#'
#' A release's `dataset` table registers every dataset that was *ingested*, which is
#' not the same as every dataset the scores were computed from. v8 ingests NOAA SEFSC
#' (`gm`) and NCCOS (`nc`) density models into `dist/`, but their density units
#' (#/km²) are not yet mapped onto the [0,100] suitability scale, so they are
#' deliberately excluded from the merge — and therefore contribute nothing to any
#' score. Listing them beside AquaMaps as though they did overstates the inputs.
#'
#' The test is introspected, never asserted: a dataset is scored iff it contributed at
#' least one model to a merged taxon, i.e. its `ds_key` appears in `taxon_model`. The
#' merged dataset itself (`ms_merge`) is scored by construction — it *is* the surface
#' scoring reads.
#'
#' @param ds_key character vector of dataset keys to classify
#' @param taxon_model_ds_key `ds_key` column of that release's `taxon_model` (the
#'   taxon ↔ contributing-model edges); `NULL` when the release does not publish the
#'   relation, in which case every registered dataset is assumed scored — the honest
#'   answer when the release gives no way to tell
#' @param merged_key the merged dataset's key
#' @return logical vector parallel to `ds_key`
#' @examples
#' dataset_is_scored(c("ms_merge", "am", "gm"), c("am", "am", "bl"))  # TRUE TRUE FALSE
#' @export
#' @concept mdl_key
dataset_is_scored <- function(ds_key, taxon_model_ds_key, merged_key = "ms_merge") {
  ds_key <- as.character(ds_key)
  if (is.null(taxon_model_ds_key)) return(rep(TRUE, length(ds_key)))
  ds_key %in% c(merged_key, unique(as.character(taxon_model_ds_key)))
}
