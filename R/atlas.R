# atlas.R — read the marine-atlas Parquet release from S3 ----
#
# The release bucket `oceanmetrics.io-public` contains dots, which breaks
# virtual-hosted-style TLS (`*.s3.amazonaws.com` cert), so httpfs MUST use
# path-style addressing. Globbing partitioned Parquet (e.g. dist_merged) needs S3
# LIST, so a credential-chain secret is used by default; single-file reads work
# anonymously against the public bucket.

#' Configure a DuckDB connection to read the marine-atlas release from S3
#'
#' Loads `httpfs` (+ `aws` for credentialed listing), sets path-style addressing +
#' region, and (unless `anon`) creates a credential-chain S3 secret. Returns the
#' connection with the atlas base URL stored in `attr(con, "atlas_base")`.
#'
#' @param con   an open DuckDB connection, or `NULL` to open a fresh in-memory one
#' @param version atlas version (e.g. `"v8"`)
#' @param anon  if `TRUE`, skip credentials (single-file public reads only; globs
#'   need LIST and will 403 unless the bucket policy allows anonymous ListBucket)
#' @param bucket S3 base of the atlas
#' @param region S3 region
#' @return the (configured) DuckDB connection
#' @importFrom DBI dbConnect dbExecute
#' @export
attach_atlas <- function(con = NULL, version = "v8", anon = FALSE,
                         bucket = "s3://oceanmetrics.io-public/marine-atlas",
                         region = "us-east-1") {
  if (is.null(con)) con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con, sprintf("SET s3_url_style='path'; SET s3_region='%s';", region))
  if (!anon) {
    DBI::dbExecute(con, "INSTALL aws; LOAD aws;")
    try(DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE SECRET atlas_s3 (TYPE s3, PROVIDER credential_chain, REGION '%s')", region)),
      silent = TRUE)
  }
  attr(con, "atlas_base") <- sprintf("%s/%s", bucket, version)
  con
}

#' Path to a release component under the atlas base
#'
#' @param con connection from [attach_atlas()]
#' @param ... path parts under the version root, e.g. `"tables"`, `"taxon.parquet"`
#' @return an `s3://…` string
#' @export
atlas_path <- function(con, ...) {
  base <- attr(con, "atlas_base")
  if (is.null(base)) stop("con not configured; call attach_atlas() first")
  paste(c(base, ...), collapse = "/")
}

#' Read a released derived table (`tables/<name>.parquet`) as a lazy relation
#'
#' @param con  connection from [attach_atlas()]
#' @param name table name (`cell`, `taxon`, `dataset`, `model`, `cell_metric`,
#'   `zone`, `zone_cell`, `zone_metric`, `metric`)
#' @return a `dplyr` tbl over the remote Parquet
#' @importFrom dplyr tbl
#' @export
atlas_tbl <- function(con, name) {
  dplyr::tbl(con, glue::glue("read_parquet('{atlas_path(con, \"tables\", paste0(name, \".parquet\"))}')"))
}
