# sync.R — content-addressed Parquet: digest, manifest, sync plan.
#
# WHY THIS EXISTS
#
# A release is published in three places — the machine that built it, the
# server that serves it, and S3 — and until now the only way to know whether
# they agreed was to compare file bytes. That does not work for Parquet: the
# same rows written twice differ byte-for-byte (timestamps in the footer, row
# group boundaries, dictionary state, compression nondeterminism), and a
# re-partitioned dataset differs again while meaning exactly the same thing.
# So `rsync` re-transfers gigabytes that did not change, and equally cannot
# tell you that two copies are genuinely the same.
#
# The COGs already solved their half of this by keying the object on a content
# hash (see content_hash_encoded()). This is the Parquet equivalent: a digest
# of what a table CONTAINS, from which a per-release manifest is built and a
# sync becomes "transfer the tables whose digest differs".
#
# THE DIGEST
#
#   bit_xor over md5_number(row_struct::VARCHAR || 0x1f || row_multiplicity)
#   grouped by the row, plus the row count and a schema digest.
#
# Three properties, each deliberate and each covered by a test:
#
#   * ORDER-INDEPENDENT. bit_xor is commutative, so the same rows in any
#     physical order give the same digest. A re-sort, a re-partition or a
#     parallel write cannot change it.
#   * MTIME-INDEPENDENT. Nothing about the file is read except its rows, so
#     rewriting an identical table is a no-op to the digest.
#   * DUPLICATE-AWARE. XOR alone cancels in pairs, so two identical rows would
#     vanish. Grouping to (row, count) first makes every hashed value distinct
#     and encodes multiplicity, which is what makes this a MULTISET hash rather
#     than a set hash.
#
# NULL is distinct from the empty string (the struct cast renders them
# differently), and the schema digest catches a rename or retype that leaves
# every value identical.
#
# Verified stable across DuckDB 1.5.2 (laptop) and 1.5.5 (server) — the two
# engines return the same digest for the same data, which is the whole point of
# comparing across machines.

.dig_con <- function(con = NULL) {
  if (!is.null(con)) return(list(con = con, close = FALSE))
  list(con = DBI::dbConnect(duckdb::duckdb()), close = TRUE)
}

#' Parquet source expression for a file, directory or glob
#'
#' A directory is read recursively with Hive partitioning ON, because the
#' partition columns of a dataset like `model_cell/mdl_id=*/` are part of its
#' data — a digest that ignored them could not tell two partitionings apart.
#'
#' @param path a `.parquet` file, a directory of them, or a glob
#' @return a DuckDB `read_parquet(...)` expression
#' @keywords internal
.parquet_src <- function(path) {
  p <- path.expand(path)
  glob <- if (grepl("[*?]", p)) p else if (dir.exists(p))
    file.path(sub("/+$", "", p), "**", "*.parquet") else p
  sprintf("read_parquet('%s', hive_partitioning = 1, union_by_name = 1)",
          gsub("'", "''", glob))
}

#' Content digest of a Parquet table or dataset
#'
#' A hash of what the table CONTAINS, independent of row order, file mtime,
#' file count and partitioning — see the note at the top of `sync.R` for why
#' byte comparison cannot do this job.
#'
#' @param path a `.parquet` file, a directory of them, or a glob
#' @param con optional DuckDB connection to reuse
#' @return a one-row data frame: `n_rows`, `n_groups` (distinct rows),
#'   `n_files`, `bytes`, `schema`, `schema_digest`, `data_digest`, `digest`
#' @export
#' @concept sync
parquet_digest <- function(path, con = NULL) {
  h <- .dig_con(con); on.exit(if (h$close) DBI::dbDisconnect(h$con, shutdown = TRUE))
  src <- .parquet_src(path)

  # schema first: a rename or retype that leaves every value identical is still
  # a different table, and the data digest alone would call them equal
  sch <- DBI::dbGetQuery(h$con, sprintf("SELECT * FROM %s LIMIT 0", src))
  schema <- paste(sprintf("%s:%s", names(sch), vapply(sch, function(x) class(x)[1], "")),
                  collapse = ",")
  schema_digest <- substr(digest::digest(schema, algo = "md5", serialize = FALSE), 1, 16)

  d <- DBI::dbGetQuery(h$con, sprintf("
    SELECT coalesce(sum(cnt), 0)::HUGEINT AS n_rows,
           coalesce(count(*), 0)::HUGEINT AS n_groups,
           printf('%%032x', bit_xor(md5_number(r::VARCHAR || '\x1f' || cnt::VARCHAR))) AS data_digest
      FROM (SELECT t AS r, count(*) AS cnt FROM %s t GROUP BY 1)", src))

  # an empty table has no rows to xor: bit_xor returns NULL, and every empty
  # table would otherwise digest the same as an unreadable one
  if (is.na(d$data_digest[1])) d$data_digest[1] <- strrep("0", 32)

  files <- .parquet_files(path)
  data.frame(
    n_rows        = as.numeric(d$n_rows[1]),
    n_groups      = as.numeric(d$n_groups[1]),
    n_files       = length(files),
    bytes         = if (length(files)) sum(file.size(files)) else 0,
    schema        = schema,
    schema_digest = schema_digest,
    data_digest   = d$data_digest[1],
    digest        = substr(digest::digest(paste0(schema_digest, d$data_digest[1]),
                                          algo = "md5", serialize = FALSE), 1, 16),
    stringsAsFactors = FALSE)
}

.parquet_files <- function(path) {
  p <- path.expand(path)
  if (dir.exists(p)) list.files(p, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  else Sys.glob(p)
}

#' Digest every Parquet table in a release directory
#'
#' One row per table, where a "table" is either a `{name}.parquet` file or a
#' `{name}/` directory of partitions. This is the per-release manifest the sync
#' compares — the Parquet counterpart of the score-COG registry.
#'
#' @param dir directory holding the release's Parquet
#' @param con optional DuckDB connection to reuse
#' @return a data frame, one row per table, sorted by name
#' @export
#' @concept sync
parquet_manifest <- function(dir, con = NULL) {
  dir <- path.expand(dir)
  stopifnot("dir does not exist" = dir.exists(dir))
  h <- .dig_con(con); on.exit(if (h$close) DBI::dbDisconnect(h$con, shutdown = TRUE))

  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  dirs  <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  dirs  <- dirs[vapply(dirs, function(d)
    length(list.files(d, pattern = "\\.parquet$", recursive = TRUE)) > 0, logical(1))]

  tabs <- c(setNames(files, sub("\\.parquet$", "", basename(files))),
            setNames(dirs,  basename(dirs)))
  if (!length(tabs)) return(data.frame())
  tabs <- tabs[order(names(tabs))]

  do.call(rbind, lapply(seq_along(tabs), function(i) {
    d <- parquet_digest(tabs[[i]], con = h$con)
    cbind(data.frame(table = names(tabs)[i],
                     path  = sub(paste0("^", dir, "/?"), "", tabs[[i]]),
                     stringsAsFactors = FALSE), d)
  }))
}

#' What differs between two Parquet manifests
#'
#' The whole point of the digest: a sync is "move the tables whose digest
#' differs", not "move everything because the bytes are never equal".
#'
#' @param src,dst manifests from [parquet_manifest()]
#' @return a data frame with `table` and `status`: `same`, `changed`,
#'   `missing` (in `dst`) or `extra` (in `dst` only)
#' @export
#' @concept sync
parquet_sync_plan <- function(src, dst) {
  s <- if (nrow(src)) src[, c("table", "digest", "n_rows", "bytes")] else
    data.frame(table = character(), digest = character(), n_rows = numeric(), bytes = numeric())
  d <- if (nrow(dst)) dst[, c("table", "digest")] else
    data.frame(table = character(), digest = character())
  m <- merge(s, d, by = "table", all = TRUE, suffixes = c("_src", "_dst"))
  m$status <- ifelse(is.na(m$digest_src), "extra",
              ifelse(is.na(m$digest_dst), "missing",
              ifelse(m$digest_src == m$digest_dst, "same", "changed")))
  m[order(m$status != "changed", m$status != "missing", m$table),
    c("table", "status", "digest_src", "digest_dst", "n_rows", "bytes")]
}
