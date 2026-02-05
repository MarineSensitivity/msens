#' Match taxa to spp.duckdb via cascade
#'
#' Match species records to the canonical taxonomy in spp.duckdb using a
#' three-step cascade:
#'
#' 1. ITIS TSN crosswalk -> worms_id
#' 2. Exact scientific_name match in worms table
#' 3. WoRMS REST API for unmatched (via `msens::wm_rest()`)
#'
#' @param d data.frame with `scientific_name` and optionally `itis_id` columns
#' @param con_spp DBI connection to spp.duckdb (read-only)
#' @return d with added `worms_id` and `botw_id` columns
#' @importFrom dplyr left_join filter select mutate coalesce anti_join bind_rows pull
#' @importFrom DBI dbGetQuery
#' @export
#' @concept taxa
match_taxa <- function(d, con_spp) {

  stopifnot("scientific_name" %in% names(d))

  # step 1: match via ITIS TSN -> worms crosswalk ----
  if ("itis_id" %in% names(d)) {
    d_itis <- dplyr::tbl(con_spp, "itis") |>
      dplyr::filter(
        taxonID %in% !!unique(stats::na.omit(d$itis_id))) |>
      dplyr::select(
        itis_id  = taxonID,
        worms_id = acceptedNameUsageID) |>
      dplyr::collect()

    d <- d |>
      dplyr::left_join(d_itis, by = "itis_id")
  } else {
    d <- d |>
      dplyr::mutate(worms_id = NA_real_)
  }

  # step 2: exact scientific name match for unmatched ----
  d_unmatched <- d |>
    dplyr::filter(is.na(worms_id))

  if (nrow(d_unmatched) > 0) {
    sci_names <- unique(d_unmatched$scientific_name)

    d_worms <- dplyr::tbl(con_spp, "worms") |>
      dplyr::filter(
        scientificName %in% sci_names |
          acceptedNameUsage %in% sci_names) |>
      dplyr::select(
        scientific_name = scientificName,
        worms_id_name   = acceptedNameUsageID) |>
      dplyr::distinct() |>
      dplyr::collect()

    d <- dplyr::bind_rows(
      d |>
        dplyr::filter(!is.na(worms_id)),
      d_unmatched |>
        dplyr::left_join(d_worms, by = "scientific_name") |>
        dplyr::mutate(
          worms_id = dplyr::coalesce(worms_id, worms_id_name)) |>
        dplyr::select(-worms_id_name))
  }

  # step 3: WoRMS REST API for remaining unmatched ----
  d_still_unmatched <- d |>
    dplyr::filter(is.na(worms_id))

  if (nrow(d_still_unmatched) > 0) {
    message(
      glue::glue(
        "match_taxa: {nrow(d_still_unmatched)} taxa unmatched, ",
        "querying WoRMS REST API..."))

    d_api <- msens::wm_rest(
      d_still_unmatched,
      scientific_name,
      "AphiaRecordsByMatchNames")

    if (nrow(d_api) > 0 && "valid_aphia_id" %in% names(d_api)) {
      d_api_match <- d_api |>
        dplyr::select(
          scientific_name,
          worms_id_api = valid_aphia_id) |>
        dplyr::distinct()

      d <- dplyr::bind_rows(
        d |>
          dplyr::filter(!is.na(worms_id)),
        d_still_unmatched |>
          dplyr::left_join(d_api_match, by = "scientific_name") |>
          dplyr::mutate(
            worms_id = dplyr::coalesce(worms_id, worms_id_api)) |>
          dplyr::select(-worms_id_api))
    }
  }

  # add botw_id column (NA placeholder; filled by caller for birds)
  if (!"botw_id" %in% names(d)) {
    d <- d |>
      dplyr::mutate(botw_id = NA_real_)
  }

  d
}
