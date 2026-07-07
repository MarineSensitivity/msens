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
  stopifnot(
    length(dataset_key) == 1L, nzchar(dataset_key),
    !grepl("|", dataset_key, fixed = TRUE))   # dataset_key must not contain the separator
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
