# asset_store.R — one content-addressed store for EVERY distribution file of EVERY release ----
#
# A release never owns a `.tif` or a `.pmtiles`; it owns pointers (`native_asset`) into an
# unversioned store, and the store has a catalog (`assets.parquet`, one row per object):
#
#   cog/{grid_id}/{hash}.tif              rasters ON an analysis grid: per-input "model", merged, score
#   native/{ds_key}/{hash}.{tif|pmtiles}  source-resolution originals (raster or vector)
#
# The key is the SOURCE content (rows or geometry) folded with the file family's ENCODING tag,
# known BEFORE the file is built - so an unchanged surface costs neither a build nor an upload.
# The object's MD5 is an integrity column, never the key (see cog_store.R: a rebuilt GeoTIFF is
# not byte-identical, so a bytes key would make every re-publish look new).

# encoding tags, one per file family ----
# a tag names the CONTAINER the payload is written in (datatype, nodata, overviews, tiling);
# changing any of those must change the key, or one URL serves two different files (the GDAL
# /vsicurl header cache then 500s low zooms - see content_hash_encoded()). The QUANTISATION a
# writer applies (terra truncates a double into INT1U) is not a tag: the hashed content is the
# pixel values as the file stores them (pixel_hashes()), so identical pixels always share a key
# and trunc-vs-round differences get different keys.
.ASSET_ENC <- c(
  cog_model_usa05 = "int1u-nd0-noovr",            # v1-v7 species COGs (backfill_versions.qmd)
  cog_model       = "int1u-nd0-ovr",              # publish_cog() defaults: INT1U, nodata 0, DEFLATE, 256 blocks, NEAREST overviews
  cog_score       = "flt4s-nd9999-noovr",         # publish_score_cogs.qmd
  native_am       = "int1u-nd0-ovr-0.5deg",       # AquaMaps HCAF probability*100 on the 720x360 grid, via publish_cog()
  native_ax       = "flt4s-nd9999-ovr-delivered", # AquaX band 1 as delivered (Float32), msens::cog_from_tif()
  native_pmtiles  = "mvt-z0-10-simp10")           # publish_pmtiles(): tippecanoe z0-10, --simplification 10 (low zooms only)

#' Encoding tag of a distribution-file family
#'
#' The tag folded into every store key ([content_hash_encoded()]). One per file family: the
#' same content written another way is a different object, deliberately.
#'
#' @param family one of `names(asset_enc())`; `NULL` returns the whole named vector
#' @return the tag (a string), or the named vector of all tags
#' @examples
#' asset_enc("native_pmtiles")
#' @export
#' @concept cog_store
asset_enc <- function(family = NULL) {
  if (is.null(family)) return(.ASSET_ENC)
  stopifnot(length(family) == 1L)
  if (!family %in% names(.ASSET_ENC))
    stop(sprintf("unknown asset family '%s'; known: %s", family,
                 paste(names(.ASSET_ENC), collapse = ", ")), call. = FALSE)
  unname(.ASSET_ENC[family])
}

# content hashes of the inputs ----

#' Content fingerprint of in-memory `(cell_id, val)` rows
#'
#' [content_hashes()] for a data frame rather than a table: the same DuckDB reduction, so a hash
#' computed from decoded raster pixels equals one computed from the Parquet the raster was
#' painted from.
#'
#' @param df data frame holding `by` and `cols`
#' @param by grouping column (e.g. `mdl_key`)
#' @param cols payload columns, in a FIXED order
#' @return data frame `by`, `n`, `content_hash`, sorted by `by`
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom duckdb duckdb duckdb_register
#' @export
#' @concept cog_store
content_hashes_df <- function(df, by = "mdl_key", cols = c("cell_id", "val")) {
  stopifnot(is.data.frame(df), all(c(by, cols) %in% names(df)))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "rows_in", df[c(by, cols)])
  # canonical payload types: every Parquet surface in the pipeline is (cell_id INTEGER, val DOUBLE),
  # and DuckDB's hash() depends on the type, so an R double cell_id would hash differently
  sel <- sprintf('"%s"', cols); sel[cols == "cell_id"] <- 'CAST("cell_id" AS INTEGER) AS "cell_id"'
  sel[cols == "val"] <- 'CAST("val" AS DOUBLE) AS "val"'
  h <- content_hashes(con, sprintf('(SELECT "%s", %s FROM rows_in) AS canon', by, paste(sel, collapse = ", ")), by, cols)
  h <- h[order(h[[by]]), , drop = FALSE]                                     # GROUP BY order is unspecified
  rownames(h) <- NULL
  h
}

#' SQL for the pixel value a writer stores
#'
#' The value a COG holds for a source value: `"trunc"` is what terra does when it writes a double
#' into INT1U (verified on 512,073 pixels of an AquaMaps model: every pixel equals `trunc(val)`,
#' only 54% equal `round(val)`); `"round_trunc"` rounds half-to-even first, as `ingest_aquax.qmd`
#' and the suitability-only merged paint do (`round()` in R is half-to-even, so `round_even`
#' here); `"none"` keeps the value (a Float32 as-delivered raster). Integer modes cap at 255.
#'
#' @param expr SQL expression for the source value (default `"val"`)
#' @param quant `"trunc"`, `"round_trunc"` or `"none"`
#' @return a SQL expression string yielding a DOUBLE
#' @examples
#' pixel_quant_sql("val", "trunc")
#' @export
#' @concept cog_store
pixel_quant_sql <- function(expr = "val", quant = c("trunc", "round_trunc", "none")) {
  quant <- match.arg(quant)
  switch(quant,
    trunc       = sprintf("CAST(trunc(LEAST(%s, 255)) AS DOUBLE)", expr),
    round_trunc = sprintf("CAST(trunc(LEAST(round_even(%s, 0), 255)) AS DOUBLE)", expr),
    none        = sprintf("CAST(%s AS DOUBLE)", expr))
}

#' Pixel-content fingerprint of every model in a surface table
#'
#' [content_hashes()] over the PIXELS a COG would hold: `val` quantised as the writer does
#' ([pixel_quant_sql()]), pixels below 1 dropped (they are NoData in the integer families), and -
#' for a dataset whose models can repeat a cell - collapsed to one value per cell by `max()`, the
#' rule the merge consumes (`turtle_sql()` / `merge_sql()` take `max(val)` per cell). The result is
#' exactly what `content_hashes_df(native_raster_rows(<the painted COG>))` returns, so "does this
#' object hold the rows its key says?" is one equality, and the key is known before any file is built.
#'
#' @param con open DuckDB connection
#' @param from table name, `read_parquet(...)` expression or parenthesised subquery with `by`,
#'   `cell_id` and `val`
#' @param by grouping column (e.g. `mdl_key`)
#' @param quant see [pixel_quant_sql()]
#' @param min_pixel keep only pixels with a quantised value `>=` this (default 1; `-Inf` keeps all)
#' @param dedup `"none"` or `"max"` (collapse repeated cells by `max(val)` BEFORE quantising)
#' @return data frame `by`, `n`, `content_hash`
#' @export
#' @concept cog_store
pixel_hashes <- function(con, from, by = "mdl_key", quant = c("trunc", "round_trunc", "none"),
                         min_pixel = 1, dedup = c("none", "max")) {
  quant <- match.arg(quant); dedup <- match.arg(dedup)
  stopifnot(is.numeric(min_pixel), length(min_pixel) == 1L)
  src <- if (dedup == "max")
    sprintf('(SELECT "%s", cell_id, max(val) AS val FROM %s GROUP BY "%s", cell_id) d', by, from, by)
  else from
  q <- pixel_quant_sql("val", quant)
  flt <- if (is.finite(min_pixel)) sprintf("WHERE %s >= %s", q, format(min_pixel, scientific = FALSE)) else ""
  sql <- sprintf('(SELECT "%s", CAST(cell_id AS INTEGER) AS cell_id, %s AS val FROM %s %s) px', by, q, src, flt)
  h <- content_hashes(con, sql, by)
  h <- h[order(h[[by]]), , drop = FALSE]
  rownames(h) <- NULL
  h
}

#' `(cell_id, val)` rows of a raster on an analysis grid
#'
#' The decoded pixels of a (possibly cropped) COG as the `(cell_id, val)` rows the pipeline hashes:
#' non-NoData pixels only, `cell_id` the row-major 1-based index on `grid`. For a COG that is a
#' bit-exact copy of its source (AquaX `ax_native`) these are the source's own rows, so the store
#' hash can be computed from the delivered TIF without painting the COG first.
#'
#' @param r a terra SpatRaster (band 1 is used)
#' @param grid grid spec ([grid_spec()] / [grid_spec_for()]) the raster's pixels index
#' @return data frame `cell_id` (double), `val` (double)
#' @importFrom terra values xyFromCell
#' @export
#' @concept cog_store
native_raster_rows <- function(r, grid) {
  stopifnot(inherits(r, "SpatRaster"), all(c("nc", "nr", "xmin", "ymax", "resx", "resy") %in% names(grid)))
  v  <- terra::values(r[[1]], mat = FALSE)
  i  <- which(!is.na(v))
  if (!length(i)) return(data.frame(cell_id = double(), val = double()))
  xy  <- terra::xyFromCell(r[[1]], i)
  col <- round((xy[, 1] - grid$xmin) / grid$resx + 0.5)
  row <- round((grid$ymax - xy[, 2]) / grid$resy + 0.5)
  if (any(col < 1 | col > grid$nc | row < 1 | row > grid$nr))
    stop("native_raster_rows(): pixels fall outside the grid (wrong grid for this raster?)", call. = FALSE)
  data.frame(cell_id = (row - 1) * as.double(grid$nc) + col, val = as.double(v[i]))
}

#' Content hash of one model's vector features (what reaches the tile)
#'
#' Normalises exactly as [publish_pmtiles()] does before tiling - EPSG:4326 (assumed when
#' missing), XY only, empty geometries dropped - then hashes the sorted, hex-encoded WKB of every
#' feature together with the attributes carried into the tiles (`mdl_key`, `ds_key`). Feature
#' order, row names and extra source columns cannot move it; any change to a geometry or to a
#' tile attribute does. Fold the result with `asset_enc("native_pmtiles")` for the store key.
#'
#' @param x an sf holding ONE model's features (call per `mdl_key`; see [native_vector_hashes()])
#' @param attrs attribute columns that reach the tile; `ds_key` defaults to the `mdl_key` prefix
#' @return a 16-hex content hash
#' @importFrom sf st_crs st_transform st_zm st_is_empty st_geometry st_as_binary st_drop_geometry
#' @importFrom digest digest
#' @export
#' @concept cog_store
native_vector_hash <- function(x, attrs = c("mdl_key", "ds_key")) {
  stopifnot(inherits(x, "sf"), "mdl_key" %in% names(x))
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
  epsg <- sf::st_crs(x)$epsg
  if (is.na(epsg) || !identical(as.integer(epsg), 4326L)) x <- sf::st_transform(x, 4326)
  x <- sf::st_zm(x, drop = TRUE)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!"ds_key" %in% names(x)) x$ds_key <- sub("\\|.*$", "", as.character(x$mdl_key))
  stopifnot("native_vector_hash(): no non-empty features" = nrow(x) > 0,
            all(attrs %in% names(x)))
  wkb  <- vapply(sf::st_as_binary(sf::st_geometry(x), hex = TRUE), as.character, "")
  a    <- do.call(paste, c(lapply(sf::st_drop_geometry(x)[attrs], as.character), sep = "\r"))
  digest::digest(paste(sort(paste(wkb, a, sep = "\n")), collapse = "\n\n"), algo = "xxhash64", serialize = FALSE)
}

#' Content hash of every model in a multi-model sf
#'
#' @param x an sf with a `mdl_key` column
#' @param ... passed to [native_vector_hash()]
#' @return data frame `mdl_key`, `n_features`, `content_hash`
#' @export
#' @concept cog_store
native_vector_hashes <- function(x, ...) {
  stopifnot(inherits(x, "sf"), "mdl_key" %in% names(x))
  parts <- split(x, as.character(x$mdl_key))
  data.frame(mdl_key = names(parts),
             n_features = vapply(parts, nrow, 1L, USE.NAMES = FALSE),
             content_hash = vapply(parts, native_vector_hash, "", ..., USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# object keys and the catalog ----

#' Object key in the unified store
#'
#' `store = "cog"`: `scope` is the `grid_id`; `store = "native"`: `scope` is the `ds_key`.
#'
#' @param store `"cog"` or `"native"`
#' @param scope `grid_id` (cog) or `ds_key` (native)
#' @param hash 16-hex store hash (source content folded with the encoding tag)
#' @param ext `"tif"` or `"pmtiles"`
#' @return relative key(s), e.g. `"cog/global05/9f3c....tif"`
#' @export
#' @concept cog_store
asset_key <- function(store = c("cog", "native"), scope, hash, ext = "tif") {
  store <- match.arg(store)
  stopifnot(!anyNA(hash), all(grepl("^[0-9a-f]{16}$", hash)), all(grepl("^[A-Za-z0-9_]+$", scope)),
            all(ext %in% c("tif", "pmtiles")))
  sprintf("%s/%s/%s.%s", store, scope, hash, ext)
}

#' The store key a pointer URL names, or `NA` when it is not a store object
#'
#' A versioned path (`.../v8/native/am/x.tif`, `file.marinesensitivity.org/pmtiles/v8/...`) is NOT
#' in the store and returns `NA`; that is how a release still holding versioned pointers is found.
#'
#' @param url asset URL(s); a `?v=` query is ignored
#' @return key(s) like `"native/bl/3f2a....pmtiles"` / `"cog/global05/....tif"`, `NA` otherwise
#' @export
#' @concept cog_store
asset_key_from_url <- function(url) {
  u <- sub("\\?.*$", "", as.character(url))
  k <- ifelse(grepl("/marine-atlas/", u, fixed = TRUE), sub("^.*/marine-atlas/", "", u),
       ifelse(grepl("/pmtiles/native/", u, fixed = TRUE), sub("^.*/pmtiles/", "", u), NA_character_))
  ifelse(!is.na(k) & grepl("^(cog/[A-Za-z0-9]+|native/[A-Za-z0-9_]+)/[0-9a-f]{16}\\.(tif|pmtiles)$", k),
         k, NA_character_)
}

.ASSET_CATALOG_COLS <- c("store", "key", "content_hash", "enc", "asset_type", "grid_id", "ds_key",
                         "bytes", "md5", "created", "first_ver")

#' Validate a store catalog (`assets.parquet`)
#'
#' One row per stored object. Errors (never warns) on: a missing column; a duplicated `key`; a
#' key that is not `{cog/<grid>|native/<ds>}/<16 hex>.<tif|pmtiles>`; `store`/`grid_id`/`ds_key`
#' that disagree with the key; a `.pmtiles` that is not `asset_type = "pmtiles"`; and, for every
#' row with a `content_hash`, a key hash that is not `content_hash_encoded(content_hash, enc)`
#' - the property that makes "already stored?" a computation instead of a listing.
#'
#' @param x catalog data frame (see `.ASSET_CATALOG_COLS`: store, key, content_hash, enc,
#'   asset_type, grid_id, ds_key, bytes, md5, created, first_ver)
#' @return `x`, invisibly
#' @export
#' @concept cog_store
asset_catalog_check <- function(x) {
  miss <- setdiff(.ASSET_CATALOG_COLS, names(x))
  if (length(miss)) stop("asset catalog lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(x$key))
    stop("asset catalog has a duplicated key (e.g. ", x$key[duplicated(x$key)][1], ")", call. = FALSE)
  m <- regmatches(x$key, regexec("^(cog|native)/([A-Za-z0-9_]+)/([0-9a-f]{16})\\.(tif|pmtiles)$", x$key))
  bad <- lengths(m) == 0L
  if (any(bad)) stop("asset catalog key is not {cog|native}/<scope>/<16 hex>.<ext>: ", x$key[bad][1], call. = FALSE)
  M <- do.call(rbind, m)
  if (!all(x$store == M[, 2])) stop("asset catalog `store` disagrees with its key", call. = FALSE)
  is_cog <- x$store == "cog"
  if (!all(ifelse(is_cog, x$grid_id == M[, 3], x$ds_key == M[, 3]), na.rm = FALSE))
    stop("asset catalog `grid_id`/`ds_key` disagrees with its key", call. = FALSE)
  if (!all((M[, 5] == "pmtiles") == (x$asset_type == "pmtiles")))
    stop("asset catalog `asset_type` disagrees with the key's extension", call. = FALSE)
  has <- !is.na(x$content_hash)
  if (any(has)) {
    want <- mapply(function(h, e) content_hash_encoded(h, e), x$content_hash[has], x$enc[has], USE.NAMES = FALSE)
    if (!all(want == M[has, 4]))
      stop("asset catalog key hash is not content_hash_encoded(content_hash, enc) for ",
           sum(want != M[has, 4]), " row(s), e.g. ", x$key[has][which(want != M[has, 4])[1]], call. = FALSE)
  }
  invisible(x)
}

#' Add objects to the catalog (idempotent)
#'
#' Rows whose `key` is already catalogued are skipped - same key, same content, nothing to store.
#' A key present with a DIFFERENT `content_hash` or `enc` is an error: two contents may never
#' share a key.
#'
#' @param catalog the current catalog (zero rows for a new store)
#' @param new rows to add, same columns
#' @return the union; the keys actually added are in `attr(, "added")`
#' @export
#' @concept cog_store
asset_catalog_add <- function(catalog, new) {
  asset_catalog_check(catalog); asset_catalog_check(new)
  new <- new[!duplicated(new$key), , drop = FALSE]
  j <- match(new$key, catalog$key)
  both <- which(!is.na(j))
  differ <- both[!mapply(function(a, b) identical(a, b),
                         paste(new$content_hash[both], new$enc[both]),
                         paste(catalog$content_hash[j[both]], catalog$enc[j[both]]))]
  if (length(differ))
    stop("asset_catalog_add(): key ", new$key[differ[1]], " is already catalogued with different content",
         call. = FALSE)
  add <- new[is.na(j), .ASSET_CATALOG_COLS, drop = FALSE]
  out <- rbind(catalog[.ASSET_CATALOG_COLS], add)
  rownames(out) <- NULL
  asset_catalog_check(out)
  attr(out, "added") <- add$key
  out
}

#' Write the catalog as Parquet (zstd), validated
#'
#' @param x catalog data frame
#' @param path output `.parquet`
#' @return `path`, invisibly
#' @importFrom arrow write_parquet
#' @export
#' @concept cog_store
asset_catalog_write <- function(x, path) {
  asset_catalog_check(x)
  arrow::write_parquet(x[.ASSET_CATALOG_COLS], path, compression = "zstd")
  invisible(path)
}

#' Read the public catalog
#'
#' `assets.parquet` at the atlas root, readable anonymously (the bucket denies anonymous listing,
#' which is why "already stored?" is an anti-join against this file and not an `aws s3 ls`).
#'
#' @param base atlas base URL ([atlas_base_url()]) or a local directory
#' @return the validated catalog
#' @importFrom arrow read_parquet
#' @export
#' @concept cog_store
asset_catalog_read <- function(base = atlas_base_url()) {
  x <- as.data.frame(arrow::read_parquet(sprintf("%s/assets.parquet", base)))
  asset_catalog_check(x)
}

#' Catalogued objects that no release points at
#'
#' Garbage is catalog rows absent from every release's pointer table; nothing is deleted except
#' through this. `pointers` is a list of data frames (one `native_asset` per release, plus any
#' score-COG table) each with an `asset_url` column, or of character vectors of URLs.
#'
#' @param catalog the store catalog
#' @param pointers list of pointer tables / URL vectors, one per release
#' @return the catalog rows no pointer names
#' @export
#' @concept cog_store
store_unreferenced <- function(catalog, pointers) {
  asset_catalog_check(catalog)
  stopifnot(is.list(pointers), length(pointers) > 0)
  used <- unique(stats::na.omit(unlist(lapply(pointers, function(p)
    asset_key_from_url(if (is.data.frame(p)) p$asset_url else p)))))
  catalog[!catalog$key %in% used, , drop = FALSE]
}

#' Keys that name more than one content
#'
#' The migration invariant: one key, one `(content_hash, enc)`. Returns the offending rows
#' (zero rows = clean).
#'
#' @param m data frame with `key`, `content_hash`, `enc`
#' @return rows of `m` whose key maps to more than one distinct `(content_hash, enc)`
#' @export
#' @concept cog_store
asset_key_collisions <- function(m) {
  stopifnot(all(c("key", "content_hash", "enc") %in% names(m)))
  id <- paste(m$content_hash, m$enc)
  n  <- tapply(id, m$key, function(z) length(unique(z)))
  m[m$key %in% names(n)[n > 1L], , drop = FALSE]
}
