# parquet.R — one place for the v8 marine-atlas Parquet write conventions
#
# Every atlas Parquet write goes through one of two helpers so the format options
# live in exactly one spot: Parquet V2 data pages, zstd compression, ~80 MB row
# groups. Two engines, one option set:
#   - write_atlas_parquet() : arrow path, for in-memory tibbles written in the
#     per-species/per-model ingest loops (often inside mclapply/furrr forks, where
#     a DuckDB connection is not fork-safe).
#   - copy_atlas_parquet()  : DuckDB `COPY` path, for engine-resident bulk writes
#     (release tables, merge surfaces, the partitioned serving surface). Only this
#     path gets true byte-sized row groups (ROW_GROUP_SIZE_BYTES).

# the ONLY place Parquet write options live
.atlas_pq <- list(
  arrow_version   = "2.6",         # arrow -> Parquet V2 data pages / logical types
  duckdb_version  = "V2",          # DuckDB PARQUET_VERSION
  compression     = "zstd",        # zstd >> snappy for these narrow columnar surfaces
  row_group_bytes = "80MB",        # DuckDB ROW_GROUP_SIZE_BYTES (unordered writes only)
  # a row-count cap so large it never binds before the 80 MB byte cap. Needed
  # because DuckDB flushes a row group when EITHER the row OR byte limit is hit,
  # and the narrow (mdl_key,cell_id,val) schema compresses ~122k rows to ~1-2 MB,
  # so without this the default row cap binds and you silently keep ~1 MB groups.
  rg_rows_unbound = 100000000L,
  # ordered writes must preserve insertion order, which is incompatible with
  # ROW_GROUP_SIZE_BYTES; approximate ~80 MB with a row count instead.
  rg_rows_sorted  = 4000000L,
  # arrow has no byte-based row-group control; ~80 MB for the narrow schema.
  arrow_chunk     = 2500000L)

#' Write an atlas surface to Parquet (arrow path) with the standard v8 options
#'
#' Parquet V2 (`version = "2.6"`), zstd compression, row groups of `chunk_size`
#' rows (arrow has no byte-based row-group control). Use in per-model / per-species
#' ingest loops where the data is an in-memory tibble (and often inside `mclapply` /
#' `furrr` forks). For DuckDB-resident data use [copy_atlas_parquet()] instead —
#' both share the one option set in `.atlas_pq`.
#'
#' @param x a data.frame / tibble (typically `mdl_key, cell_id, val`)
#' @param path output `.parquet` path
#' @param chunk_size rows per row group (default from `.atlas_pq`)
#' @return `path`, invisibly
#' @importFrom utils packageVersion
#' @export
#' @concept parquet
write_atlas_parquet <- function(x, path, chunk_size = .atlas_pq$arrow_chunk) {
  require_duckdb()                                   # enforce the engine floor everywhere
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("package 'arrow' is required for write_atlas_parquet()")
  arrow::write_parquet(
    x, path,
    version     = .atlas_pq$arrow_version,
    compression = .atlas_pq$compression,
    chunk_size  = chunk_size)
  invisible(path)
}

#' COPY a DuckDB relation/query to Parquet with the standard v8 options
#'
#' Emits `COPY (<sql>) TO '<path>' (FORMAT parquet, PARQUET_VERSION V2,
#' COMPRESSION zstd, ...)`. Two regimes:
#' \itemize{
#'   \item **unordered / partitioned** (default, or `partition_by`/`per_thread`):
#'     drops insertion-order preservation so `ROW_GROUP_SIZE_BYTES '80MB'` binds
#'     (true ~80 MB row groups). Restores the prior setting afterward.
#'   \item **ordered** (`order_by` given): keeps insertion order for the serving
#'     row-group zone-map pruning, so byte-sized groups can't be used — approximates
#'     ~80 MB with a row count instead.
#' }
#'
#' @param con open DuckDB connection
#' @param sql a SELECT (no trailing `;`) OR a bare table name
#' @param path output file (or a directory when `partition_by`/`per_thread`)
#' @param order_by optional ORDER BY column expression, e.g. `"mdl_key"`
#' @param per_thread write one file per thread into `path` (a dir); default FALSE
#' @param partition_by optional Hive-partition column(s), e.g. `"mdl_id"`
#' @return `path`, invisibly
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom glue glue
#' @export
#' @concept parquet
copy_atlas_parquet <- function(con, sql, path, order_by = NULL,
                               per_thread = FALSE, partition_by = NULL) {
  require_duckdb(con = con)
  q       <- if (grepl("[[:space:]]", trimws(sql))) sql else glue::glue("SELECT * FROM {sql}")
  ordered <- !is.null(order_by)
  if (ordered) q <- glue::glue("SELECT * FROM ({q}) ORDER BY {order_by}")

  opts <- c(
    "FORMAT parquet",
    glue::glue("PARQUET_VERSION {.atlas_pq$duckdb_version}"),
    glue::glue("COMPRESSION {.atlas_pq$compression}"),
    glue::glue("ROW_GROUP_SIZE {if (ordered) .atlas_pq$rg_rows_sorted else .atlas_pq$rg_rows_unbound}"))
  if (!ordered)
    opts <- c(opts, glue::glue("ROW_GROUP_SIZE_BYTES '{.atlas_pq$row_group_bytes}'"))
  if (per_thread)
    opts <- c(opts, "PER_THREAD_OUTPUT true")
  if (!is.null(partition_by))
    opts <- c(opts, glue::glue("PARTITION_BY ({paste(partition_by, collapse = ', ')})"),
              "OVERWRITE_OR_IGNORE")

  # byte-sized row groups need preserve_insertion_order=false; an ORDER BY needs
  # it true. toggle only for the unordered case and restore the prior value.
  if (!ordered) {
    prev <- DBI::dbGetQuery(con, "SELECT current_setting('preserve_insertion_order') AS v")$v
    DBI::dbExecute(con, "SET preserve_insertion_order=false")
    on.exit(DBI::dbExecute(con, glue::glue("SET preserve_insertion_order={tolower(prev)}")), add = TRUE)
  }
  DBI::dbExecute(con, glue::glue("COPY ({q}) TO '{path}' ({paste(opts, collapse = ', ')})"))
  invisible(path)
}
