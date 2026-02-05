#' Path to SDM DuckDB
#'
#' Get the file path to the species distribution model DuckDB database.
#'
#' @param version version date suffix (default: "2026")
#' @return character path to the DuckDB file
#' @importFrom glue glue
#' @export
#' @concept db
sdm_db_path <- function(version = "2026") {
  dir_data <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "~/My Drive/projects/msens/data",
    "Linux"  = "/share/data")
  glue::glue("{dir_data}/derived/sdm_{version}.duckdb")
}

#' Connect to SDM DuckDB
#'
#' Open a DBI connection to the species distribution model DuckDB database.
#'
#' @param version version date suffix (default: "2026")
#' @param read_only logical; open in read-only mode (default: FALSE)
#' @return DBI connection object
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @export
#' @concept db
sdm_db_con <- function(version = "2026", read_only = FALSE) {
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
