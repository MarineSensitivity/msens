# cog_store.R — the content-addressed COG store, shared by every MST version
#
# Publishing every model of every release naively is ~226,000 COGs / ~70 GB. But
# raw source surfaces DO NOT change between releases — only merged (`ms_merge`)
# surfaces do, and only where a merge rule changed. Measured: 3,000 raw AquaMaps
# models are payload-identical across v3, v6 and v7. So a model's COG is stored
# under a hash of its CONTENT and every release that produces that same surface
# simply points at the existing object.
#
# Hash the PAYLOAD, never the .tif. A GeoTIFF is not byte-reproducible — GDAL
# stamps TIFFTAG_DATETIME/TIFFTAG_SOFTWARE and the COG driver's IFD layout varies
# — so a file digest changes on every rebuild and would defeat dedup entirely.
#
# The reduction is the same order-independent one the target manifests use
# (see manifest.R): count + bit_xor(hash) + sum(hash). bit_xor and sum are
# commutative, so partition order, row-group layout and Parquet metadata cannot
# perturb it; count and sum guard the even-multiplicity cancellation that
# bit_xor alone suffers on exact-duplicate rows.

#' Grouped content-fingerprint SQL
#'
#' One pass, no sort and no spill — the whole point, since these tables run to
#' 1.2 billion rows (measured: 325M rows in 7.8 s, so a full release hashes in
#' under a minute).
#'
#' @param from table name or `read_parquet(...)` expression
#' @param by grouping column — the per-release model id (`mdl_seq` or `mdl_id`)
#' @param cols payload columns to hash, in a FIXED order
#' @return a SQL string yielding `by`, `n`, `x`, `s`
#' @importFrom glue glue
#' @export
#' @concept cog_store
content_hash_sql <- function(from, by, cols = c("cell_id", "val")) {
  hcols <- paste(sprintf('"%s"', cols), collapse = ", ")
  glue::glue(
    'SELECT "{by}", count(*) n, ',
    'bit_xor(hash({hcols}))::VARCHAR x, sum(hash({hcols}))::VARCHAR s ',
    'FROM {from} GROUP BY "{by}"')
}

#' Content fingerprint of every model in a surface table
#'
#' @param con open DuckDB connection
#' @param from table name or `read_parquet(...)` expression
#' @param by grouping column — the per-release model id
#' @param cols payload columns (default `c("cell_id","val")`; v1-v7 use `"value"`)
#' @return a data frame with `by`, `n` and a 16-char `content_hash`
#' @importFrom DBI dbGetQuery
#' @export
#' @concept cog_store
content_hashes <- function(con, from, by, cols = c("cell_id", "val")) {
  d <- DBI::dbGetQuery(con, content_hash_sql(from, by, cols))
  .assert_hash_chr(d)
  data.frame(d[by], n = d$n,
             content_hash = vapply(seq_len(nrow(d)), function(i)
               .fold_hash(d[i, , drop = FALSE]), character(1)),
             stringsAsFactors = FALSE)
}

# DuckDB's hash() returns UBIGINT. The R driver hands that back as a DOUBLE,
# which silently truncates to ~15 significant digits (e.g. 1.315468e+19) and
# would alias DISTINCT models onto one COG — a wrong-species map with no error
# anywhere. The ::VARCHAR cast in the SQL prevents it; this asserts the cast
# actually survived, because the failure is otherwise invisible.
.assert_hash_chr <- function(d) {
  bad <- intersect(c("x", "s"), names(d))
  bad <- bad[!vapply(d[bad], is.character, logical(1))]
  if (length(bad))
    stop(sprintf(
      "content hash column(s) %s came back as %s, not character - the ::VARCHAR cast was lost. ",
      paste(bad, collapse = ", "), paste(vapply(d[bad], typeof, character(1)), collapse = ", ")),
      "A double-typed UBIGINT truncates and would collide distinct models onto one COG.",
      call. = FALSE)
  invisible(TRUE)
}

#' Object key for a content-addressed COG
#'
#' `grid_id` is part of the key because `cell_id` is meaningless without it: the
#' same id names a different place on `usa05` than on `global05`, so two surfaces
#' could hash alike yet be entirely different maps.
#'
#' @param grid_id grid the cells belong to (see [grid_registry()])
#' @param content_hash from [content_hashes()]
#' @param ext file extension (default `"tif"`)
#' @return relative object key(s), e.g. `"cog/usa05/9f3c….tif"`
#' @export
#' @concept cog_store
content_key <- function(grid_id, content_hash, ext = "tif") {
  stopifnot(length(grid_id) == 1 || length(grid_id) == length(content_hash))
  sprintf("cog/%s/%s.%s", grid_id, content_hash, ext)
}

#' Public URL of a stored COG
#'
#' @param grid_id,content_hash see [content_key()]
#' @param base atlas base URL from [atlas_base_url()]
#' @param ext file extension
#' @return absolute `https://` URL(s)
#' @export
#' @concept cog_store
content_url <- function(grid_id, content_hash, base = atlas_base_url(), ext = "tif")
  sprintf("%s/%s", base, content_key(grid_id, content_hash, ext))

#' Index the existing COG store
#'
#' ONE recursive listing into a set, so "is this already published?" is a lookup
#' rather than a HEAD request per model — at ~30k models per release the
#' per-object form is the difference between seconds and hours.
#'
#' @param grid_id restrict to one grid's prefix, or `NULL` for the whole store
#' @param bucket S3 URI of the atlas root
#' @param aws path to the `aws` CLI
#' @return character vector of `content_hash` values already present
#' @export
#' @concept cog_store
cog_store_index <- function(grid_id = NULL,
                            bucket = "s3://oceanmetrics.io-public/marine-atlas",
                            aws = "aws") {
  pre <- if (is.null(grid_id)) sprintf("%s/cog/", bucket) else sprintf("%s/cog/%s/", bucket, grid_id)
  out <- suppressWarnings(system2(aws, c("s3", "ls", "--recursive", shQuote(pre)),
                                  stdout = TRUE, stderr = TRUE))
  # `aws s3 ls` exits 1 with NO output when the prefix simply does not exist yet,
  # which is the normal state before the first publish -- an empty store, not a
  # failure. Only a non-zero exit that actually said something is an error.
  txt <- out[nzchar(trimws(out))]
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    if (length(txt))
      stop(sprintf("listing '%s' failed: %s", pre, paste(utils::tail(txt, 3), collapse = " ")),
           call. = FALSE)
    return(character())
  }
  keys <- sub("^.*\\s+", "", out[nzchar(out)])
  unique(sub("\\.[^.]*$", "", basename(keys[grepl("\\.tif$", keys)])))
}
