#' Parse NOAA protected_status field
#'
#' Extract ESA status and MMPA flag from the semicolon-delimited
#' `protected_status` field in the NOAA species directory.
#'
#' @param status_str character vector of semicolon-delimited statuses
#' @return tibble with columns: esa_status (character), is_mmpa (logical)
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#' @export
#' @concept listings
parse_noaa_status <- function(status_str) {
  tibble::tibble(
    esa_status = dplyr::case_when(
      stringr::str_detect(status_str, "ESA Endangered(?! - Foreign)") ~ "EN",
      stringr::str_detect(status_str, "ESA Threatened(?! - Foreign)")  ~ "TN",
      TRUE ~ "LC"),
    is_mmpa = stringr::str_detect(status_str, "MMPA") |>
      tidyr::replace_na(FALSE))
}

#' Compute extinction risk score from extrisk_code and flags
#'
#' US-listed species get a base score from ESA status plus additive bonuses
#' for MMPA (+20) and MBTA (+10), capped at 100. Non-US species fall back to
#' IUCN Red List scale (CR=50, EN=25, VU=5, NT=2, other=1).
#'
#' @param extrisk_code character, e.g. "NMFS:EN", "FWS:TN", "IUCN:CR"
#' @param is_mmpa logical; species protected under MMPA (default: FALSE)
#' @param is_mbta logical; species protected under MBTA (default: FALSE)
#' @return integer score 0-100
#' @importFrom stringr str_split_i
#' @importFrom dplyr case_match case_when
#' @export
#' @concept listings
compute_er_score <- function(extrisk_code, is_mmpa = FALSE, is_mbta = FALSE) {
  authority <- stringr::str_split_i(extrisk_code, ":", 1)
  code      <- stringr::str_split_i(extrisk_code, ":", 2)
  is_us     <- authority %in% c("NMFS", "FWS")

  base_us <- dplyr::case_match(
    code,
    "EN" ~ 100L,
    "TN" ~ 50L,
    "LC" ~ 1L,
    .default = 0L)
  bonus <- ifelse(is_mmpa, 20L, 0L) + ifelse(is_mbta, 10L, 0L)

  score_iucn <- dplyr::case_match(
    code,
    "CR" ~ 50L,
    "EN" ~ 25L,
    "VU" ~ 5L,
    "NT" ~ 2L,
    .default = 1L)

  dplyr::case_when(
    is.na(extrisk_code) ~ 1L,
    is_us               ~ pmin(base_us + bonus, 100L),
    TRUE                ~ score_iucn)
}
