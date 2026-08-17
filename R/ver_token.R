# ver_token.R — bind a Shiny session to the version its PAGE was served for
#
# A Shiny app learns "which version?" from the client: `session$clientData$url_search`
# and `url_pathname` are whatever the browser's JavaScript sends over the
# websocket. That is fine for a public app that renders public releases, but it
# is exactly the wrong thing to trust once WHICH version a visitor may see is a
# policy: on the signed-in preview host the version is the URL PATH
# (`/v9/scores/`, gated per version by Cloudflare Access), and a reviewer allowed
# on v9 must not be able to steer the shared preview process to v10 by editing
# what the client sends.
#
# The page GET is the request the server decides on: Caddy rewrites
# `/v9/scores/` to `/scores/?ver=v9` (forcing `ver`), `ui(req)` resolves the
# version through the instance policy (atlas_allow_access()) and embeds a
# TOKEN -- `ver.expiry.hmac` -- signed with a secret only the server holds. The
# server function trusts that token alone (`isolate(input$ms_ver_token)` is
# available at session start, exactly like clientData), re-applies the policy,
# and renders THAT version. A client can drop or edit the token; it cannot mint
# one, so it cannot widen what it was served.
#
# Secret: `MS_TOKEN_SECRET` (server .env; reaches the app processes through
# rocker's Renviron.site) so tokens survive a restart and both Shiny Server
# instances agree; else a per-process random secret from /dev/urandom, which is
# still correct -- a page and its websocket are served by the same process --
# and only means a session that straddles a process restart is asked to reload.

.tok <- new.env(parent = emptyenv())

#' Secret used to sign version tokens
#'
#' `MS_TOKEN_SECRET` when set (recommended in production; generate with
#' `openssl rand -hex 32`), else a per-process random secret. Memoised.
#'
#' @return a character scalar (never printed by anything in this package)
#' @export
#' @concept version
ver_token_secret <- function() {
  s <- Sys.getenv("MS_TOKEN_SECRET", "")
  if (nzchar(s)) return(s)
  if (is.null(.tok$secret)) {
    .tok$secret <- tryCatch({
      con <- file("/dev/urandom", "rb", raw = TRUE); on.exit(close(con))
      paste(format(as.hexmode(as.integer(readBin(con, "raw", 32))), width = 2), collapse = "")
    }, error = function(e)
      # no /dev/urandom (Windows): still per-process and unguessable enough for a
      # dev laptop, and MS_TOKEN_SECRET is what production uses anyway
      digest::digest(list(Sys.time(), Sys.getpid(), tempfile(), stats::runif(64)), algo = "sha256"))
  }
  .tok$secret
}

# constant-time-ish string equality (an HMAC compare must not short-circuit)
.const_eq <- function(a, b) {
  a <- charToRaw(a); b <- charToRaw(b)
  if (length(a) != length(b)) return(FALSE)
  sum(bitwXor(as.integer(a), as.integer(b))) == 0
}

#' Sign a version token for a page
#'
#' Called from `ui(req)` once the version has been resolved through the instance
#' policy; the value goes into a hidden input the server function reads back with
#' [ver_token_verify()].
#'
#' @param ver a version label (`v8`, `v4b`, ...)
#' @param secret from [ver_token_secret()]
#' @param ttl seconds until the token expires (default 24 h: a page left open
#'   longer than that is asked to reload)
#' @param now the current time (injectable for tests)
#' @return `"{ver}.{expiry}.{hmac-sha256}"`
#' @export
#' @concept version
ver_token_sign <- function(ver, secret = ver_token_secret(), ttl = 86400, now = Sys.time()) {
  if (!is.character(ver) || length(ver) != 1 || !.is_ver(ver))
    stop(sprintf("ver_token_sign: '%s' is not a version label", ver), call. = FALSE)
  exp  <- as.integer(floor(as.numeric(now)) + ttl)
  body <- sprintf("%s.%d", ver, exp)
  sprintf("%s.%s", body, digest::hmac(secret, body, algo = "sha256"))
}

#' Verify a version token; `NULL` unless it is genuine and unexpired
#'
#' Verification is the *candidate*: the caller must still resolve the returned
#' version through [atlas_resolve_ver()] with the instance's
#' [atlas_allow_access()], so a token minted by another instance (the preview
#' host, for a restricted version) never widens what this process renders.
#'
#' @param token the string from the page's hidden input (may be `NULL`/`""`)
#' @param secret from [ver_token_secret()]
#' @param now the current time (injectable for tests)
#' @return the version label, or `NULL` (missing, malformed, tampered, expired)
#' @export
#' @concept version
ver_token_verify <- function(token, secret = ver_token_secret(), now = Sys.time()) {
  if (is.null(token) || length(token) != 1 || is.na(token) || !nzchar(token)) return(NULL)
  p <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(p) != 3) return(NULL)
  ver <- p[1]; exp <- suppressWarnings(as.integer(p[2])); mac <- p[3]
  if (!.is_ver(ver) || is.na(exp) || !nzchar(mac)) return(NULL)
  want <- digest::hmac(secret, sprintf("%s.%d", ver, exp), algo = "sha256")
  if (!.const_eq(mac, want)) return(NULL)
  if (as.numeric(now) > exp) return(NULL)
  ver
}

#' URLs on the signed-in preview host
#'
#' The version is the PATH there — `{base}/{ver}/scores/`, `{base}/docs/{ver}/` —
#' never `?ver=`, because Cloudflare Access scopes its per-version policies by
#' path. One place for that shape, so the apps' picker and modal, the docs' app
#' links, the landing page and the release checks cannot disagree.
#'
#' @param app `scores` or `species`
#' @param ver version label
#' @param base from [atlas_preview_url()]
#' @return a URL string with a trailing slash
#' @export
#' @concept version
preview_app_url <- function(app = c("scores", "species"), ver, base = atlas_preview_url()) {
  app <- match.arg(app)
  if (!.is_ver(ver)) stop(sprintf("'%s' is not a version label", ver), call. = FALSE)
  sprintf("%s/%s/%s/", base, ver, app)
}

#' @rdname preview_app_url
#' @export
preview_docs_url <- function(ver, base = atlas_preview_url()) {
  if (!.is_ver(ver)) stop(sprintf("'%s' is not a version label", ver), call. = FALSE)
  sprintf("%s/docs/%s/", base, ver)
}

#' Where this release's sibling products live
#'
#' The MST is four things a reader moves between — two apps, the book, and the
#' project home — and each has been a dead end from the others: the scores table
#' links out to a species map with no way back, and the book's app links sit only
#' on its Preface (apps#11, docs#6). This is the one definition of that link set,
#' so a nav added to any of them cannot disagree with the rest.
#'
#' **Version-preserving is the point.** A bare link to `/scores` from a v7 session
#' silently moves the reader to the promoted release, which is precisely the
#' confusion these navs exist to remove — so `?ver=` is always carried. On a
#' `restricted` release the version is the URL PATH on the signed-in preview host
#' instead ([preview_app_url()]), because Cloudflare Access scopes its per-version
#' policies by path; sending such a reviewer to `?ver=` on the public host would
#' show them nothing.
#'
#' @param ver version label, e.g. `v8`
#' @param access `public` or `restricted` — from [atlas_ver_access()]
#' @param app_base public app host
#' @param docs_base public docs base (GitHub Pages)
#' @param home project landing page
#' @return named character vector: `scores`, `species`, `docs`, `home`
#' @export
#' @concept version
#' @examples
#' product_urls("v7")
#' product_urls("v9", access = "restricted")
product_urls <- function(ver, access = "public",
                         app_base  = "https://app.marinesensitivity.org",
                         docs_base = "https://marinesensitivity.org/docs",
                         home      = "https://marinesensitivity.org") {
  if (!.is_ver(ver)) stop(sprintf("'%s' is not a version label", ver), call. = FALSE)
  access <- match.arg(access, c("public", "restricted"))
  if (identical(access, "restricted"))
    return(c(scores  = preview_app_url("scores",  ver),
             species = preview_app_url("species", ver),
             docs    = preview_docs_url(ver),
             # the landing page is neither versioned nor gated
             home    = home))
  app_base  <- sub("/+$", "", app_base)
  docs_base <- sub("/+$", "", docs_base)
  c(scores  = sprintf("%s/scores/?ver=%s",  app_base, ver),
    species = sprintf("%s/species/?ver=%s", app_base, ver),
    docs    = sprintf("%s/%s/", docs_base, ver),
    home    = home)
}
