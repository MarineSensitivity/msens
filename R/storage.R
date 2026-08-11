# storage.R — a browsable front door for the public S3 bucket
#
# S3 serves OBJECTS, not directories. A URL ending in "/" 404s unless an object
# literally has that key, and anonymous ListBucket is denied on this bucket
# (verified: 403 AccessDenied), so a browser cannot enumerate anything. Each
# directory therefore gets a real index.html object, generated here with
# credentials at publish time, and a Caddy vhost rewrites folder URLs to it.
#
# Ported from the CalCOFI storage index (workflows/libs/gcs_index.R), which
# learned two things the hard way and both are kept:
#   - never index the generated index.html objects themselves;
#   - a zero exit status does NOT mean the objects landed — verify by reading
#     the deepest page back.
#
# Scale differs from CalCOFI: this bucket holds per-model COGs and Hive-
# partitioned Parquet, so `serve/model_cell/` alone is ~17,765 partition
# directories. Indexing every one would produce tens of thousands of pages
# nobody reads, so machine-only trees are summarized at their parent rather than
# walked (see `skip` and `max_depth`).

#' List every object under a prefix
#'
#' Uses the credentialed `aws` CLI rather than the anonymous XML API, because
#' this bucket denies anonymous `ListBucket`.
#'
#' @param bucket S3 URI (e.g. `s3://oceanmetrics.io-public`)
#' @param prefix key prefix to list ("" for the whole bucket)
#' @param aws path to the `aws` CLI
#' @return a data frame of `key` and `size` (bytes)
#' @export
#' @concept storage
s3_list_all <- function(bucket = "s3://oceanmetrics.io-public", prefix = "", aws = "aws") {
  uri <- sub("/+$", "", bucket)
  if (nzchar(prefix)) uri <- paste0(uri, "/", sub("^/+", "", prefix))
  out <- suppressWarnings(system2(aws, c("s3", "ls", "--recursive", shQuote(paste0(uri, "/"))),
                                  stdout = TRUE, stderr = TRUE))
  txt <- out[nzchar(trimws(out))]
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    if (length(txt))
      stop(sprintf("listing '%s' failed: %s", uri, paste(utils::tail(txt, 3), collapse = " ")),
           call. = FALSE)
    return(data.frame(key = character(), size = numeric(), stringsAsFactors = FALSE))
  }
  # "2026-08-10 16:16:06        3 marine-atlas/latest.txt"
  m <- regmatches(txt, regexec("^\\S+\\s+\\S+\\s+(\\d+)\\s+(.+)$", txt))
  ok <- vapply(m, length, integer(1)) == 3L
  if (!any(ok)) return(data.frame(key = character(), size = numeric(), stringsAsFactors = FALSE))
  data.frame(key  = vapply(m[ok], `[`, character(1), 3),
             size = as.numeric(vapply(m[ok], `[`, character(1), 2)),
             stringsAsFactors = FALSE)
}

.fmt_size <- function(b) {
  ifelse(is.na(b), "",
    ifelse(b >= 1024^3, sprintf("%.1f GB", b / 1024^3),
    ifelse(b >= 1024^2, sprintf("%.1f MB", b / 1024^2),
    ifelse(b >= 1024,   sprintf("%.0f KB", b / 1024), sprintf("%.0f B", b)))))
}
.esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' One self-contained storage index page
#'
#' Self-contained by necessity: served straight off a bucket, so no external
#' stylesheet, font or script can be referenced.
#'
#' @param title,subtitle page heading
#' @param body_html the listing table
#' @param crumb breadcrumb HTML
#' @return an HTML string
#' @export
#' @concept storage
storage_page <- function(title, subtitle = "", body_html = "", crumb = "") {
  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>", .esc(title), "</title><style>",
    ":root{--fg:#1a1a1a;--muted:#6b7280;--bd:#e5e7eb;--bg:#fff;--acc:#0b6bcb}",
    "@media(prefers-color-scheme:dark){:root{--fg:#e5e7eb;--muted:#9ca3af;--bd:#374151;",
    "--bg:#111827;--acc:#60a5fa}}",
    "body{margin:0;padding:2rem 1rem;background:var(--bg);color:var(--fg);",
    "font:15px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif}",
    "main{max-width:60rem;margin:0 auto}h1{font-size:1.4rem;margin:0 0 .25rem}",
    ".sub,.crumb{color:var(--muted);font-size:.9rem}.crumb{margin-bottom:1rem}",
    "a{color:var(--acc);text-decoration:none}a:hover{text-decoration:underline}",
    ".scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;margin-top:1rem}",
    "th,td{text-align:left;padding:.4rem .6rem;border-bottom:1px solid var(--bd);",
    "white-space:nowrap}th{font-size:.8rem;text-transform:uppercase;color:var(--muted)}",
    "td.num{text-align:right;font-variant-numeric:tabular-nums}",
    ".chip{background:var(--bd);border-radius:999px;padding:.05rem .5rem;font-size:.8rem;",
    "color:var(--muted)}footer{margin-top:2rem;color:var(--muted);font-size:.85rem}",
    ".readme{margin:1rem 0 .5rem;padding:.75rem 1rem;border:1px solid var(--bd);",
    "border-radius:8px;background:color-mix(in srgb,var(--bd) 25%,transparent)}",
    ".readme h1,.readme h2,.readme h3{font-size:1rem;margin:.4rem 0}",
    ".readme p{margin:.4rem 0}.readme code{font-size:.85em;background:var(--bd);",
    "padding:.05rem .3rem;border-radius:4px}",
    ".readme pre{overflow-x:auto;background:var(--bd);padding:.5rem;border-radius:6px}",
    ".readme pre code{background:none;padding:0}.readme ul{margin:.4rem 0 .4rem 1.1rem}",
    "</style></head><body><main>",
    "<h1>", .esc(title), "</h1>",
    if (nzchar(subtitle)) paste0("<div class='sub'>", subtitle, "</div>") else "",
    if (nzchar(crumb))    paste0("<div class='crumb'>", crumb, "</div>") else "",
    "<div class='scroll'>", body_html, "</div>",
    "<footer>MarineSensitivity public storage</footer>",
    "</main></body></html>")
}

#' Generate index pages for a bucket tree
#'
#' A page is generated for EVERY directory, however many files it holds: the cost
#' of this index is the number of PAGES, not the number of objects, so a
#' directory of 20,000 files is still one page. Only a directory whose CHILDREN
#' are themselves numerous directories is collapsed - `serve/model_cell/` is
#' ~17,765 Hive partition directories, which would be ~17,765 pages nobody reads.
#' Those children are still listed with counts and labelled, so the reason is
#' visible rather than mysterious.
#'
#' @param objs data frame from [s3_list_all()]
#' @param site_url public base of the browse host
#' @param obj_url public base objects are fetched from
#' @param max_child_dirs a directory with more than this many SUBDIRECTORIES has
#'   its children listed but not expanded into pages of their own
#' @param max_rows most rows rendered on one page, so a huge directory does not
#'   emit a multi-megabyte document
#' @param readme named list of prefix -> markdown, rendered above the listing.
#'   A file listing says WHAT is there but never what it means, how it was made,
#'   or how to reach it without a browser; this is where that goes. The same text
#'   is published as `README.md` beside the data, so `aws s3 cp` and `curl` users
#'   get it too.
#' @return a data frame of `key` (the index.html object key) and `html`
#' @export
#' @concept storage
build_storage_index <- function(objs,
                                site_url = "https://storage.marinesensitivity.org",
                                obj_url  = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public",
                                max_child_dirs = 500L,
                                max_rows = 2000L,
                                readme = list()) {
  objs <- objs[!grepl("(^|/)index[.]html$", objs$key), , drop = FALSE]
  objs <- objs[!grepl("/$", objs$key), , drop = FALSE]   # 0-byte directory markers
  if (!nrow(objs)) return(data.frame(key = character(), html = character()))

  parts <- strsplit(objs$key, "/", fixed = TRUE)
  anc <- unique(c("", unlist(lapply(parts, function(p)
    if (length(p) > 1)
      vapply(seq_len(length(p) - 1), function(i) paste(p[seq_len(i)], collapse = "/"), character(1))
    else NULL))))

  parent_of <- function(d) ifelse(grepl("/", d), sub("/[^/]*$", "", d), "")
  kid_dirs  <- table(parent_of(anc[nzchar(anc)]))
  crowded   <- names(kid_dirs)[kid_dirs > max_child_dirs]

  under_crowded <- function(d) {
    if (!length(crowded)) return(rep(FALSE, length(d)))
    vapply(d, function(x) any(vapply(crowded, function(cc)
      nzchar(cc) && startsWith(x, paste0(cc, "/")), logical(1))), logical(1), USE.NAMES = FALSE)
  }
  keep <- anc[!under_crowded(anc)]

  n_kids <- function(k) { v <- kid_dirs[k]; if (is.na(v)) 0L else as.integer(v) }

  pages <- lapply(keep, function(d) {
    pre  <- if (nzchar(d)) paste0(d, "/") else ""
    here <- objs[startsWith(objs$key, pre), , drop = FALSE]
    rel  <- substring(here$key, nchar(pre) + 1)
    seg  <- sub("/.*$", "", rel)
    is_dir <- grepl("/", rel)

    kids  <- sort(unique(seg))
    shown <- utils::head(kids, max_rows)

    rows <- vapply(shown, function(k) {
      sel <- seg == k
      if (any(is_dir[sel])) {
        n <- sum(sel); sz <- sum(here$size[sel])
        expandable <- !under_crowded(paste0(pre, k))
        nm <- paste0(.esc(k), "/")
        cell <- if (expandable) {
          sprintf("<a href='%s/%s%s/'>%s</a>", site_url, pre, .esc(k), nm)
        } else if (n == 1L) {
          # a Hive partition holds exactly one object -- link straight to it
          # rather than to a page that would say only "here is one file"
          sprintf("<a href='%s/%s'>%s</a>", obj_url, here$key[sel][1], nm)
        } else {
          sprintf("%s <span class='chip' title='%s partition directories - listed here rather than expanded into a page each'>partitioned data</span>",
                  nm, format(n_kids(paste0(pre, k)), big.mark = ","))
        }
        sprintf("<tr><td>%s</td><td class='num'>%s</td><td class='num'>%s</td></tr>",
                cell, format(n, big.mark = ","), .fmt_size(sz))
      } else {
        i <- which(sel)[1]
        sprintf("<tr><td><a href='%s/%s%s'>%s</a></td><td class='num'></td><td class='num'>%s</td></tr>",
                obj_url, pre, .esc(k), .esc(k), .fmt_size(here$size[i]))
      }
    }, character(1))

    if (length(kids) > length(shown))
      rows <- c(rows, sprintf("<tr><td colspan='3' class='chip'>showing %s of %s entries</td></tr>",
                              format(length(shown), big.mark = ","),
                              format(length(kids), big.mark = ",")))

    crumbs <- if (nzchar(d)) {
      sg <- strsplit(d, "/", fixed = TRUE)[[1]]
      paste0("<a href='", site_url, "/'>root</a> / ",
             paste(vapply(seq_along(sg), function(i)
               sprintf("<a href='%s/%s/'>%s</a>", site_url,
                       paste(sg[seq_len(i)], collapse = "/"), .esc(sg[i])), character(1)),
               collapse = " / "))
    } else ""

    intro <- .render_readme(readme[[if (nzchar(d)) d else "."]])
    body <- paste0(intro,
                   "<table><thead><tr><th>name</th><th class='num'>items</th>",
                   "<th class='num'>size</th></tr></thead><tbody>",
                   paste(rows, collapse = ""), "</tbody></table>")
    list(key  = paste0(pre, "index.html"),
         html = storage_page(if (nzchar(d)) d else "oceanmetrics.io-public",
                             sprintf("%s object(s), %s", format(nrow(here), big.mark = ","),
                                     .fmt_size(sum(here$size))),
                             body, crumbs))
  })

  data.frame(key  = vapply(pages, `[[`, character(1), "key"),
             html = vapply(pages, `[[`, character(1), "html"),
             stringsAsFactors = FALSE)
}

# Markdown -> HTML for the per-directory README. commonmark is present in both
# the laptop and container libraries; without it the text is still shown, just
# unformatted, because an explanation that fails to render is worse than a plain
# one that does.
.render_readme <- function(md) {
  if (is.null(md) || !nzchar(trimws(md))) return("")
  html <- if (requireNamespace("commonmark", quietly = TRUE))
    commonmark::markdown_html(md, extensions = TRUE)
  else paste0("<pre>", .esc(md), "</pre>")
  paste0("<div class='readme'>", html, "</div>")
}
