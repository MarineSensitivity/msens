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

#' Require a modern DuckDB (and optionally the spatial GEOMETRY extension)
#'
#' Guards the v8 Parquet-V2 / byte-sized-row-group writers ([copy_atlas_parquet()],
#' [write_atlas_parquet()]) and leaves room for a future GeoParquet cell-geometry
#' column: checks the installed `duckdb` R package is `>= min` and, when
#' `spatial = TRUE`, that `LOAD spatial` succeeds (native `GEOMETRY` type, DuckDB
#' 1.5+). Geometry is not yet persisted — `spatial` defaults `FALSE`.
#'
#' @param min minimum `duckdb` package version (default `"1.5.0"`)
#' @param con optional open connection to test `spatial` on (a temp in-memory one
#'   is used if `NULL`)
#' @param spatial also require the spatial extension (default `FALSE`)
#' @return `TRUE` invisibly, or stops
#' @importFrom utils packageVersion
#' @importFrom DBI dbConnect dbExecute dbDisconnect
#' @importFrom duckdb duckdb
#' @export
#' @concept db
require_duckdb <- function(min = "1.5.0", con = NULL, spatial = FALSE) {
  if (utils::packageVersion("duckdb") < min)
    stop("duckdb >= ", min, " required; installed ",
         as.character(utils::packageVersion("duckdb")))
  if (spatial) {
    if (is.null(con)) {
      con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    }
    DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  }
  invisible(TRUE)
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
