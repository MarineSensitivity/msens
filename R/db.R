#' Path to SDM DuckDB
#'
#' Get the file path to the species distribution model DuckDB database.
#' v3 lives under `<data>/derived/sdm_v3.duckdb`; v4+ lives under
#' `<big>/<version>/sdm.duckdb`.
#'
#' **Falls back to `serve.duckdb`** when the full `sdm.duckdb` is absent. The
#' server deliberately does not carry the multi-GB v8 database — it holds only
#' the KB-sized view DB over the released Parquet — so without this fallback
#' anything calling `sdm_db_con()` there (notably the `/report` endpoint) fails
#' outright on v8. The Shiny apps already did this inline; centralising it here
#' stops the two from drifting.
#'
#' @param version version suffix (default: "v6")
#' @return character path to the DuckDB file
#' @importFrom glue glue
#' @export
#' @concept db
sdm_db_path <- function(version = "v6") {
  sysname <- Sys.info()[["sysname"]]
  # v3 once lived at derived/sdm_v3.duckdb, before releases were foldered by
  # version. It has since moved to the standard big/{version}/ layout, so the
  # special case is a FALLBACK, not the answer: returning the legacy path
  # unconditionally made every v3 caller fail on a file that is not there
  # (caught when the v1-v7 backfill skipped v3 alone).
  legacy_v3 <- if (version == "v3") {
    dir_data <- switch(sysname,
      "Darwin" = "~/My Drive/projects/msens/data",
      "Linux"  = "/share/data")
    glue::glue("{dir_data}/derived/sdm_v3.duckdb")
  } else NULL
  if (!is.null(legacy_v3) && file.exists(path.expand(legacy_v3))) {
    legacy_v3
  } else {
    dir_big <- switch(
      sysname,
      "Darwin" = glue::glue("~/_big/msens/derived/{version}"),
      "Linux"  = glue::glue("/share/data/big/{version}"))
    full  <- glue::glue("{dir_big}/sdm.duckdb")
    serve <- glue::glue("{dir_big}/serve.duckdb")
    if (!file.exists(path.expand(full)) && file.exists(path.expand(serve)))
      serve else full
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
