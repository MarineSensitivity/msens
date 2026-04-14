#' Path to SDM DuckDB
#'
#' Get the file path to the species distribution model DuckDB database.
#' v3 lives under `<data>/derived/sdm_v3.duckdb`; v4+ lives under
#' `<big>/<version>/sdm.duckdb`.
#'
#' @param version version suffix (default: "v6")
#' @return character path to the DuckDB file
#' @importFrom glue glue
#' @export
#' @concept db
sdm_db_path <- function(version = "v6") {
  sysname <- Sys.info()[["sysname"]]
  if (version == "v3") {
    dir_data <- switch(
      sysname,
      "Darwin" = "~/My Drive/projects/msens/data",
      "Linux"  = "/share/data")
    glue::glue("{dir_data}/derived/sdm_v3.duckdb")
  } else {
    dir_big <- switch(
      sysname,
      "Darwin" = glue::glue("~/_big/msens/derived/{version}"),
      "Linux"  = glue::glue("/share/data/big/{version}"))
    glue::glue("{dir_big}/sdm.duckdb")
  }
}

#' Connect to SDM DuckDB
#'
#' Open a DBI connection to the species distribution model DuckDB database.
#'
#' @param version version suffix (default: "v6")
#' @param read_only logical; open in read-only mode (default: TRUE)
#' @return DBI connection object
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @export
#' @concept db
sdm_db_con <- function(version = "v6", read_only = TRUE) {
  DBI::dbConnect(duckdb::duckdb(
    dbdir     = sdm_db_path(version),
    read_only = read_only))
}

#' Connect to species taxonomy DuckDB
#'
#' Open a DBI connection to the species taxonomy reference DuckDB database.
#'
#' @param read_only logical; open in read-only mode (default: TRUE)
#' @return DBI connection object
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom glue glue
#' @export
#' @concept db
spp_db_con <- function(read_only = TRUE) {
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  DBI::dbConnect(duckdb::duckdb(
    dbdir     = glue::glue("{dir_data}/derived/spp.duckdb"),
    read_only = read_only))
}
