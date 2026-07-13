# manifest.R — content-addressed change detection for the targets pipeline
#
# Each pipeline notebook's tracked `output:` is a small manifest JSON, hashed by
# `targets` (format = "file") to decide whether downstream targets must re-run.
# The trick: make that file's bytes a pure function of the OUTPUT CONTENT — not of
# wall-clock time, row order, row-group boundaries or Parquet file metadata — so a
# notebook that re-executes over identical data leaves an identical manifest and
# nothing downstream re-runs. A previous design embedded `built = Sys.time()`, so
# every run changed the bytes and the whole DAG rebuilt every time.
#
# The fingerprint is an ORDER-INDEPENDENT reduction (count + bit_xor(hash) +
# sum(hash)): bit_xor and sum are commutative so row/file order and row-group
# layout don't matter; count + sum guard the even-multiplicity cancellation that
# bit_xor alone has on exact-duplicate rows.

.fold_hash <- function(r)
  substr(digest::digest(paste(r$n, r$x, r$s, sep = "|"), algo = "xxhash64"), 1, 16)

.hash_sql <- function(from, cols) {
  hcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  glue::glue(
    "SELECT count(*) n, bit_xor(hash({hcols}))::VARCHAR x, sum(hash({hcols}))::VARCHAR s FROM {from}")
}

#' Order-independent content fingerprint of Parquet file(s)
#'
#' Scans the ON-DISK Parquet (cheap — never re-runs the ingest) and reduces it to
#' a short fingerprint invariant to row order, row-group boundaries, compression
#' and Parquet metadata timestamps. Works for surface files (`mdl_key,cell_id,val`)
#' and registry tables alike; columns are introspected unless given.
#'
#' @param glob a `read_parquet` glob/path (e.g. `"dir/*.parquet"`, `"dir/**/*.parquet"`)
#' @param con optional DuckDB connection; a temp in-memory one is used if `NULL`
#' @param cols optional character vector of columns to hash (default: all)
#' @return a 16-char hex fingerprint string
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom glue glue
#' @export
#' @concept manifest
hash_parquet <- function(glob, con = NULL, cols = NULL) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  }
  from <- glue::glue("read_parquet('{glob}')")
  if (is.null(cols))
    cols <- DBI::dbGetQuery(con, glue::glue("SELECT column_name FROM (DESCRIBE SELECT * FROM {from})"))[[1]]
  .fold_hash(DBI::dbGetQuery(con, .hash_sql(from, cols)))
}

#' Order-independent content fingerprint of a DuckDB table/query
#'
#' Same reduction as [hash_parquet()] but over a DuckDB table or query — for
#' targets whose real output is a DB table (e.g. `merge_taxon`, `score_*`) rather
#' than a file.
#'
#' @param con open DuckDB connection
#' @param sql a table name OR a SELECT (no trailing `;`)
#' @param cols optional character vector of columns to hash (default: all)
#' @return a 16-char hex fingerprint string
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue
#' @export
#' @concept manifest
hash_query <- function(con, sql, cols = NULL) {
  from <- if (grepl("[[:space:]]", trimws(sql))) glue::glue("({sql})") else sql
  if (is.null(cols))
    cols <- DBI::dbGetQuery(con, glue::glue("SELECT column_name FROM (DESCRIBE SELECT * FROM {from})"))[[1]]
  .fold_hash(DBI::dbGetQuery(con, .hash_sql(from, cols)))
}

#' Write a deterministic, content-addressed target manifest
#'
#' The tracked `output:` for a pipeline notebook. Bytes are a pure function of
#' `target`, `content_hash` and the (content-derived) `stats`, keys in fixed order,
#' NO wall-clock — so `targets` `format = "file"` re-hashes to the SAME value when
#' the data is unchanged and downstream targets do not re-run. Idempotent: if the
#' file already holds identical bytes it is left untouched (mtime preserved) unless
#' `force`. Keep `stats` deterministic (counts, ranges, versions) and free of
#' machine-specific paths so the manifest is host-independent.
#'
#' @param path manifest path (the notebook's `output:`)
#' @param target target name
#' @param content_hash from [hash_parquet()] / [hash_query()]
#' @param stats named list of deterministic summary stats
#' @param force rewrite even if unchanged (default `FALSE`; see [force_target()])
#' @return `content_hash`, invisibly
#' @importFrom jsonlite toJSON
#' @export
#' @concept manifest
write_manifest <- function(path, target, content_hash, stats = list(), force = FALSE) {
  obj  <- c(list(target = target, content_hash = content_hash),
            if (length(stats)) stats[order(names(stats))] else NULL)
  json <- as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  lines <- strsplit(json, "\n", fixed = TRUE)[[1]]
  if (!force && file.exists(path) && identical(readLines(path, warn = FALSE), lines))
    return(invisible(content_hash))
  writeLines(lines, path)
  invisible(content_hash)
}

#' Should this target be forced to rebuild?
#'
#' Reads two env vars: `MSENS_FORCE_ALL` (global truthy → force everything) and
#' `MSENS_FORCE` (comma-separated target names). Notebooks pass the result to
#' [write_manifest()] `force=` and/or use it to gate their own expensive rebuild.
#'
#' @param target target name
#' @return logical
#' @export
#' @concept manifest
force_target <- function(target) {
  truthy <- function(x) tolower(trimws(x)) %in% c("1", "true", "yes", "t", "on")
  if (truthy(Sys.getenv("MSENS_FORCE_ALL"))) return(TRUE)
  target %in% trimws(strsplit(Sys.getenv("MSENS_FORCE", ""), ",", fixed = TRUE)[[1]])
}
