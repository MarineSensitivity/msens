# urls.R — where the project's bytes live, named once
#
# The rule: BULK bytes (.tif .tiff .pmtiles .parquet .gpkg .duckdb .zip .gz .nc)
# are fetched from the object store; the VM serves only COMPUTED responses
# (tiles, API, apps) and small HTML/JSON pages. A sync client that re-read PMTiles
# from the VM host cost $450 of egress, and published pointer tables still carry
# thousands of `https://file.marinesensitivity.org/pmtiles/...` URLs. So there is
# ONE place that names the hosts (atlas_bases) and ONE function that classifies a
# URL against them (url_audit), which publish notebooks gate on
# (url_audit_assert). No network is touched here.

# bulk extensions; matched against the URL path only (never the query string)
URL_BULK_EXT <- c("tif", "tiff", "pmtiles", "parquet", "gpkg", "duckdb", "zip", "gz", "nc")

# first host label of a computed-response host (tile servers, APIs, tile proxies)
URL_COMPUTED_LABELS <- c("api", "tile", "tiles", "tilecache", "titilecache",
                         "h3t", "pmtiles", "stac-api")

#' Base URLs of every host the project serves from
#'
#' The single place the hosts are named. The defaults are
#' `store` (the S3 object store, path-style: the dotted bucket breaks vhost TLS),
#' `atlas` (`<store>/marine-atlas`), `file`, `storage`, `titiler`,
#' `titiler_legacy`, `stac`, `stac_api`, `app` and `preview`.
#'
#' Override with `options(msens.atlas_bases = list(store = "http://localhost:9000/b"))`:
#' named entries replace the defaults, and `atlas` follows `store` unless it is
#' given too. Trailing slashes are dropped.
#'
#' @return a named list of base URLs (no trailing slash)
#' @export
#' @concept urls
#' @examples
#' atlas_bases()$atlas
atlas_bases <- function() {
  b <- list(
    store          = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public",
    atlas          = NULL,
    file           = "https://file.marinesensitivity.org",
    storage        = "https://storage.marinesensitivity.org",
    titiler        = "https://titiler-v8.marinesensitivity.org",
    titiler_legacy = "https://titiler.marinesensitivity.org/msens",
    stac           = "https://file.marinesensitivity.org/stac",
    stac_api       = "https://stac-api.marinesensitivity.org",
    app            = "https://app.marinesensitivity.org",
    preview        = "https://preview.marinesensitivity.org")

  o <- getOption("msens.atlas_bases")
  if (!is.null(o)) {
    stopifnot(
      "`msens.atlas_bases` must be a named list of character strings" =
        is.list(o), !length(o) || !is.null(names(o)),
      all(vapply(o, function(x) is.character(x) && length(x) == 1 && !is.na(x), logical(1))))
    unknown <- setdiff(names(o), names(b))
    stopifnot("`msens.atlas_bases` has unknown entries" = !length(unknown))
    b[names(o)] <- o
  }
  b <- lapply(b, \(x) if (is.null(x)) x else sub("/+$", "", x))
  if (is.null(b$atlas)) b$atlas <- paste0(b$store, "/marine-atlas")
  b
}

# host (lowercase, no userinfo/port) and path (no query/fragment) of each URL
.url_parts <- function(url) {
  has_scheme <- grepl("^[a-z][a-z0-9+.-]*://", url, ignore.case = TRUE)
  rest <- sub("^[a-z][a-z0-9+.-]*://", "", url, ignore.case = TRUE)
  auth <- sub("[/?#].*$", "", rest)
  path <- sub("^[^/?#]*", "", rest)
  path <- sub("[?#].*$", "", path)
  host <- tolower(sub(":[0-9]*$", "", sub("^.*@", "", auth)))
  host[!has_scheme] <- NA_character_
  list(host = host, path = path)
}

# is `url` the base itself or something under it (boundary-aware)?
.url_under <- function(url, base) {
  u <- tolower(url); b <- tolower(sub("/+$", "", base))
  startsWith(u, b) & (nchar(u) == nchar(b) | substring(u, nchar(b) + 1, nchar(b) + 1) %in% c("/", "?", "#"))
}

.host_of <- function(base) .url_parts(base)$host

#' Classify URLs by what serves them
#'
#' Pure string work, vectorised, no network. Each URL is one of:
#'
#' * `store`: under the `store` base, any extension.
#' * `computed`: a tile / API host (titiler, legacy titiler, STAC API, `api`,
#'   `tile`, `tilecache`, `titilecache`, `h3t`, `pmtiles` tile-proxy hosts). A
#'   bulk extension inside the query string (`?url=x.tif`) does not matter.
#' * `vm_bulk`: any OTHER `*.marinesensitivity.org` / `*.oceanmetrics.io` host
#'   (file, storage, app, preview, ...) whose path ends in a bulk extension
#'   (`.tif .tiff .pmtiles .parquet .gpkg .duckdb .zip .gz .nc`; query and
#'   fragment ignored, case-insensitive), or a directory URL (ends in `/`) under
#'   `/pmtiles/` or `/derived/` on the `file` host. This is the egress leak.
#' * `page`: the remaining URLs on those project hosts (HTML, JSON, STAC JSON, no
#'   extension) and on `marinesensitivity.org` itself.
#' * `external`: anything else (other organisations' hosts).
#'
#' `NA` input gives class `NA`.
#'
#' @param urls character vector of URLs
#' @param bases named list from [atlas_bases()]
#' @return a data frame with `url`, `host` and `class`
#' @importFrom stats na.omit
#' @export
#' @concept urls
#' @examples
#' url_audit(c("https://file.marinesensitivity.org/pmtiles/v9/rng_iucn/1.pmtiles?v=1",
#'             "https://file.marinesensitivity.org/stac/v9/collection.json"))
url_audit <- function(urls, bases = atlas_bases()) {
  stopifnot(is.character(urls) || all(is.na(urls)), is.list(bases))
  urls <- as.character(urls)
  p    <- .url_parts(urls)

  file_host <- .host_of(bases$file)
  proj_hosts <- unique(na.omit(unlist(lapply(
    bases[c("file", "storage", "app", "preview", "stac")], .host_of))))
  comp_hosts <- unique(na.omit(unlist(lapply(
    bases[c("titiler", "titiler_legacy", "stac_api")], .host_of))))

  is_proj <- !is.na(p$host) &
    (p$host %in% proj_hosts | p$host %in% comp_hosts |
       grepl("(^|[.])(marinesensitivity[.]org|oceanmetrics[.]io)$", p$host))
  label1  <- sub("[.].*$", "", p$host)
  is_comp <- !is.na(p$host) &
    (p$host %in% comp_hosts |
       (is_proj & (label1 %in% URL_COMPUTED_LABELS | grepl("^titiler", label1))))

  bulk_re <- paste0("[.](", paste(URL_BULK_EXT, collapse = "|"), ")$")
  is_bulk <- grepl(bulk_re, tolower(p$path)) |
    (!is.na(p$host) & p$host == file_host & grepl("^/(pmtiles|derived)/(.*/)?$", tolower(p$path)))

  in_store <- !is.na(urls) & .url_under(urls, bases$store)

  class <- ifelse(is.na(urls), NA_character_,
           ifelse(in_store,    "store",
           ifelse(is_comp,     "computed",
           ifelse(is_proj & is_bulk, "vm_bulk",
           ifelse(is_proj,     "page", "external")))))

  data.frame(url = urls, host = p$host, class = class, stringsAsFactors = FALSE)
}

#' Stop when a URL would be served as bulk bytes from the VM
#'
#' The publish-time gate: pointer tables, manifests and bundles must name the
#' object store for bulk bytes. Errors with the count and up to five examples if
#' any URL is `vm_bulk` ([url_audit()]) and matches no regex in `allow`.
#'
#' @param urls character vector of URLs
#' @param what what is being checked, for the message
#' @param allow regular expressions of `vm_bulk` URLs that are accepted
#' @param bases named list from [atlas_bases()]
#' @return the [url_audit()] table, invisibly
#' @importFrom utils head
#' @export
#' @concept urls
url_audit_assert <- function(urls, what = "urls", allow = character(), bases = atlas_bases()) {
  stopifnot(is.character(what), length(what) == 1, is.character(allow))
  a   <- url_audit(urls, bases)
  bad <- !is.na(a$class) & a$class == "vm_bulk"
  if (length(allow) && any(bad))
    bad <- bad & !Reduce(`|`, lapply(allow, \(re) grepl(re, a$url)))
  if (any(bad)) {
    ex <- head(unique(a$url[bad]), 5)
    stop(sprintf(
      "%s: %s URL(s) would serve bulk bytes from the VM instead of the object store (%s), e.g.\n  %s",
      what, format(sum(bad), big.mark = ","), bases$store, paste(ex, collapse = "\n  ")),
      call. = FALSE)
  }
  invisible(a)
}
