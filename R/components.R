#' The scored components: one registry of keys and labels
#'
#' The scored components of a release (the seven species categories plus the
#' environmental `primprod`, and the legacy v1-v7 `other` bucket) are named in ONE
#' table, `components.csv`, read by this package, the app bundle and the docs, so a
#' key or a label is spelled in one place. Columns: `key` (the `sp_cat` /
#' `extrisk_{key}` / `primprod` spelling -- never changes), `label` (the v11 display
#' name), `short`, `order` (display order), `colour` (the Atlas categorical palette,
#' `--cat-*` in its `tokens.css`; may be empty), `icon` (may be empty), `kind`
#' (`"species"` or `"environment"`) and `label_v10` (the labels v1-v10 published).
#'
#' The source of truth is `workflows/data/components.csv`; the copy in
#' `inst/extdata/components.csv` must be identical (`test-components.R` compares the
#' two when the workflows checkout sits beside this package).
#'
#' @param path character; a components CSV. Default `NULL` reads the copy shipped in
#'   the package (`system.file("extdata/components.csv", package = "msens")`).
#' @return a tibble of the components, sorted by `order`. Errors on duplicate keys or
#'   orders, a `kind` outside `species`/`environment`, an empty `key`, `label`,
#'   `short` or `label_v10`, or a missing column.
#' @concept score
#' @export
#' @examples
#' components_registry()
components_registry <- function(path = NULL) {
  if (is.null(path))
    path <- system.file("extdata/components.csv", package = "msens")
  stopifnot(is.character(path), length(path) == 1L, nzchar(path), file.exists(path))
  d <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = character(),
                       colClasses = "character")
  need <- c("key", "label", "short", "order", "colour", "icon", "kind", "label_v10")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop("components csv lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  d$order <- suppressWarnings(as.integer(d$order))
  empty <- function(x) is.na(x) | !nzchar(trimws(x))
  for (f in c("key", "label", "short", "label_v10"))
    if (any(empty(d[[f]]))) stop("components csv has an empty `", f, "`", call. = FALSE)
  if (anyNA(d$order)) stop("components csv `order` must be integer", call. = FALSE)
  if (anyDuplicated(d$key)) stop("components csv has duplicate `key`", call. = FALSE)
  if (anyDuplicated(d$order)) stop("components csv has duplicate `order`", call. = FALSE)
  if (!all(d$kind %in% c("species", "environment")))
    stop("components csv `kind` must be 'species' or 'environment'", call. = FALSE)
  tibble::as_tibble(d[order(d$order), need])
}

#' The display label of scored components
#'
#' @param key character; component keys (`sp_cat` values or `primprod`).
#' @param v11 logical; `FALSE` (default) returns the labels v1-v10 published
#'   (`label_v10`), `TRUE` the v11 names (`label`: `primary_producer` is "Flora",
#'   `primprod` "Productivity").
#' @return a character vector, `NA` for a key the registry does not list.
#' @concept score
#' @export
#' @examples
#' component_label(c("primary_producer", "bird"))
#' component_label("primary_producer", v11 = TRUE)
component_label <- function(key, v11 = FALSE) {
  stopifnot(is.logical(v11), length(v11) == 1L, !is.na(v11))
  reg <- components_registry()
  unname(stats::setNames(reg[[if (v11) "label" else "label_v10"]], reg$key)[as.character(key)])
}
