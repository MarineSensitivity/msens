#' Clean a scientific name for taxonomic matching
#'
#' Normalizes messy source names (esp. USFWS) so they match WoRMS/BOTW: drops
#' parenthetical synonym notation like `(=oxyrhynchus)`, an `spp.`/`ssp.` marker, and
#' collapses whitespace. Use `binomial = TRUE` to also reduce a trinomial (subspecies)
#' to its `Genus species` binomial — a useful fallback when WoRMS has the species but
#' not the subspecies.
#'
#' @param x character vector of scientific names
#' @param binomial logical; if `TRUE`, keep only the first two words (default `FALSE`)
#' @return cleaned character vector
#' @examples
#' clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi")  # "Acipenser oxyrinchus desotoi"
#' clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi", binomial = TRUE)  # "Acipenser oxyrinchus"
#' @export
#' @concept taxa
#' @importFrom stringr str_replace_all str_squish str_split_fixed
clean_sci_name <- function(x, binomial = FALSE) {
  x <- stringr::str_replace_all(x, "\\s*\\(=[^)]*\\)", "")          # drop (=synonym)
  x <- stringr::str_replace_all(x, "\\s+(ssp|spp|subsp|var)\\.?\\s+", " ")
  x <- stringr::str_squish(x)
  if (binomial) {
    w <- stringr::str_split_fixed(x, " ", 3)
    x <- ifelse(w[, 2] == "", x, paste(w[, 1], w[, 2]))
  }
  x
}

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

#' Species category (`sp_cat`) from taxonomy
#'
#' The one rule that turns a taxon's WoRMS class / kingdom (plus "is a BirdLife bird" and "is a
#' SWOT turtle") into the MST scoring component. It lived inline in `merge_taxon.qmd`; from v9
#' the AquaX ingest also needs it — to report species by component \emph{before} a merge exists —
#' so it is a function both call rather than two copies that drift.
#'
#' Categories: `turtle` (the SWOT set), `bird` (BirdLife or class Aves), `mammal`, `reptile` and
#' `amphibian` (categorized but EXCLUDED from scoring), `fish`, `coral` (Hexa/Octocorallia),
#' `primary_producer` (Plantae / Chromista / Bacteria), else `invertebrate`. No `other`.
#'
#' @param class WoRMS class (character; `NA` allowed)
#' @param kingdom WoRMS kingdom (character; `NA` allowed)
#' @param is_botw logical; the taxon is keyed by BirdLife (BOTW)
#' @param is_turtle logical; the taxon is one of the SWOT sea turtles
#' @return character vector of categories, parallel to `class`
#' @examples
#' sp_cat_from_taxonomy(c("Mammalia", "Teleostei", "Hexacorallia", NA),
#'                      c("Animalia", "Animalia", "Animalia", "Plantae"))
#' @export
#' @concept taxa
sp_cat_from_taxonomy <- function(class, kingdom = NA_character_, is_botw = FALSE, is_turtle = FALSE) {
  n <- length(class)
  kingdom   <- rep_len(as.character(kingdom), n)
  is_botw   <- rep_len(as.logical(is_botw), n)   %in% TRUE
  is_turtle <- rep_len(as.logical(is_turtle), n) %in% TRUE
  cls <- as.character(class)
  fish_cls    <- c("Teleostei","Elasmobranchii","Holocephali","Myxini","Chondrostei","Petromyzonti",
                   "Actinopteri","Actinopterygii","Chondrichthyes","Coelacanthi","Dipneusti",
                   "Cladistia","Sarcopterygii")
  reptile_cls <- c("Reptilia","Crocodylia","Squamata","Testudines","Lepidosauria","Sauropsida",
                   "Archosauria")
  dplyr::case_when(
    is_turtle                                        ~ "turtle",
    is_botw | cls %in% "Aves"                        ~ "bird",
    cls %in% "Mammalia"                              ~ "mammal",
    cls %in% reptile_cls                             ~ "reptile",        # excluded from scoring
    cls %in% "Amphibia"                              ~ "amphibian",      # excluded from scoring
    cls %in% fish_cls                                ~ "fish",
    cls %in% c("Hexacorallia", "Octocorallia")       ~ "coral",
    kingdom %in% c("Plantae", "Chromista", "Bacteria") ~ "primary_producer",
    TRUE                                             ~ "invertebrate")
}
