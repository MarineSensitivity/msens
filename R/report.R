# report.R — standardized "## Outputs" reporting for pipeline notebooks
#
# Each pipeline notebook renders to HTML (via quarto_render inside its target) and
# carries a `## Design` mermaid diagram plus a `## Outputs` summary table. These
# helpers standardize the summary so every notebook reports its Parquet outputs the
# same way (row/model counts, value range, file count + size).

#' Summarize a set of atlas Parquet files for a notebook's `## Outputs` section
#'
#' One-row summary of a `read_parquet` glob: `n_rows`, `n_models` (distinct
#' `mdl_key`, when present), `val_min`/`val_max` (when a `val` column is present),
#' `n_files` and `size_mb`. Pair with [report_table()] and the `## Outputs`
#' section convention.
#'
#' @param glob a `read_parquet` glob/path
#' @param con optional DuckDB connection; a temp in-memory one is used if `NULL`
#' @return a one-row tibble
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @export
#' @concept report
report_parquet_summary <- function(glob, con = NULL) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  }
  from <- glue::glue("read_parquet('{glob}')")
  cols <- DBI::dbGetQuery(con, glue::glue("SELECT column_name FROM (DESCRIBE SELECT * FROM {from})"))[[1]]
  sel  <- c("count(*) AS n_rows",
            if ("mdl_key" %in% cols) "count(DISTINCT mdl_key) AS n_models",
            if ("val" %in% cols) "min(val) AS val_min",
            if ("val" %in% cols) "max(val) AS val_max")
  r  <- DBI::dbGetQuery(con, glue::glue("SELECT {paste(sel, collapse = ', ')} FROM {from}"))
  fm <- DBI::dbGetQuery(con, glue::glue(
    "SELECT count(*) AS n_files, sum(file_size_bytes) AS bytes FROM parquet_file_metadata('{glob}')"))
  r$n_files <- fm$n_files
  r$size_mb <- round(fm$bytes / 1e6, 1)
  tibble::as_tibble(r)
}

#' House-style table for pipeline reports
#'
#' Thin wrapper over `knitr::kable` so notebook `## Outputs` tables share one style
#' and a single call site to restyle later. Returns `x` unchanged if `knitr` is
#' unavailable.
#'
#' @param x a data.frame / tibble
#' @param caption optional table caption
#' @return a `knitr_kable` object (or `x`)
#' @export
#' @concept report
report_table <- function(x, caption = NULL) {
  if (!requireNamespace("knitr", quietly = TRUE)) return(x)
  knitr::kable(x, caption = caption)
}
