# version_ui.R — the version picker both apps share
#
# One app renders any published release, so users need to see WHICH release they
# are looking at and move between them. Building that list in each app would
# guarantee the two drift, so the markup is generated here from the same
# `versions.json` that drives the pipeline and the docs.

#' HTML for the version picker
#'
#' Lists every published version newest-first, marks the one on screen, and links
#' to the others via `?ver=`. Retired and pre-release versions are labelled
#' rather than hidden: a reader following a citation to v4 needs to find it, and
#' needs to be told it is superseded.
#'
#' @param current the version being displayed
#' @param versions data frame from [atlas_versions()]; fetched if `NULL`
#' @param href a function of the version label returning its link
#'   (default `?ver={v}`)
#' @return an [htmltools::tagList()]
#' @importFrom htmltools tagList tags HTML
#' @export
#' @concept version
version_picker_html <- function(current, versions = NULL,
                                href = function(v) sprintf("?ver=%s", v)) {
  if (is.null(versions)) versions <- atlas_versions()
  ord <- order(versions$released %||% versions$ver, decreasing = TRUE)
  versions <- versions[ord, , drop = FALSE]

  badge <- function(status) switch(
    status,
    released   = "",
    prerelease = " <span class='badge bg-warning text-dark'>pre-release</span>",
    retired    = " <span class='badge bg-secondary'>retired</span>", "")

  rows <- vapply(seq_len(nrow(versions)), function(i) {
    v   <- versions$ver[i]
    ttl <- if ("title" %in% names(versions)) versions$title[i] else ""
    dt  <- if ("released" %in% names(versions)) versions$released[i] else ""
    if (identical(v, current))
      sprintf(paste0("<li class='list-group-item active d-flex justify-content-between",
                     " align-items-start'><div><b>%s</b>%s<div class='small'>%s</div></div>",
                     "<span class='small'>%s &middot; showing</span></li>"),
              v, badge(versions$status[i]), ttl, dt)
    else
      sprintf(paste0("<li class='list-group-item d-flex justify-content-between",
                     " align-items-start'><div><a href='%s'><b>%s</b></a>%s",
                     "<div class='small text-muted'>%s</div></div>",
                     "<span class='small text-muted'>%s</span></li>"),
              href(v), v, badge(versions$status[i]), ttl, dt)
  }, character(1))

  htmltools::tagList(
    htmltools::HTML(paste0("<ul class='list-group list-group-flush'>",
                           paste(rows, collapse = ""), "</ul>")))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
