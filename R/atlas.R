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
#' @param views if `TRUE` (default), also create named views over the released
#'   tables (via [atlas_views()]) so the calc/score helpers compose directly —
#'   e.g. `scores_for_pra(attach_atlas(), pra_key)` just works
#' @param bucket S3 base of the atlas
#' @param region S3 region
#' @return the (configured) DuckDB connection, with the atlas base URL in
#'   `attr(con, "atlas_base")` and (unless `views = FALSE`) the atlas table views
#' @importFrom DBI dbConnect dbExecute
#' @export
attach_atlas <- function(con = NULL, version = "v8", anon = FALSE, views = TRUE,
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
  base <- sprintf("%s/%s", bucket, version)
  attr(con, "atlas_base") <- base
  if (views) atlas_views(con, base = base, region = region, anon = anon)
  con
}

#' Create named views over the released atlas tables
#'
#' Mirrors the view set the serving `serve.duckdb` exposes, so the calc/score
#' helpers that reference bare table names (`tbl(con, "zone")`, `scores_for_pra()`,
#' `species_for_cells()`, …) compose directly with an [attach_atlas()] connection.
#' Single-file tables are read via **path-style HTTPS** (anonymous GET + HTTP range;
#' the dotted bucket breaks virtual-hosted TLS). `model_cell` is Hive-partitioned
#' under `serve/` and its glob needs S3 LIST, so it is created only when
#' `anon = FALSE` (credentialed); it joins `model` back so ad-hoc queries select by
#' the stable `mdl_key`, not the volatile `mdl_id`. The scoring tables store the
#' metric in `val`; a `val AS value` alias is exposed for back-compat.
#'
#' @param con  connection from [attach_atlas()]
#' @param base atlas base URL (defaults to `attr(con, "atlas_base")`)
#' @param region S3 region
#' @param anon  if `TRUE`, skip the credentialed `model_cell` glob view
#' @return `con`, invisibly
#' @importFrom DBI dbExecute
#' @export
atlas_views <- function(con, base = attr(con, "atlas_base"),
                        region = "us-east-1", anon = FALSE) {
  if (is.null(base)) stop("con not configured; call attach_atlas() first")
  http     <- sub("^s3://", sprintf("https://s3.%s.amazonaws.com/", region), base)
  val_tbls <- c("cell_metric", "zone_metric", "zone")     # apps/calc reference `value`
  tbls     <- c("cell", "taxon", "dataset", "model", "metric", "cell_metric",
                "zone", "zone_cell", "zone_metric", "native_asset")
  for (t in tbls) {
    extra <- if (t %in% val_tbls) ", val AS value" else ""
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT *%s FROM read_parquet('%s/tables/%s.parquet')",
      t, extra, http, t))
  }
  if (!anon) {
    mc <- sprintf("%s/serve/model_cell/*/*.parquet", base)
    try(DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW model_cell AS
         SELECT m.mdl_key, mc.mdl_id, mc.cell_id, mc.val, mc.val AS value
         FROM read_parquet('%s', hive_partitioning = true) mc JOIN model m USING (mdl_id)", mc)),
      silent = TRUE)
  }
  invisible(con)
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
