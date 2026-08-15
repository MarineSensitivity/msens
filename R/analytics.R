# analytics.R — usage tracking for the browser-facing products ----
#
# TWO CHANNELS, one code path. Both are driven from the browser so the Shiny
# reactive thread never performs network I/O (a synchronous POST per interaction
# would stall the app on every species selection):
#
#   1. GA4 (gtag)  — aggregate, bounded-cardinality behaviour: page views, which
#      tab, which layer, which download. Free, and the same measurement ID across
#      every product so a session spans marinesensitivity.org -> app.* .
#   2. Sheet log   — the detail GA4 is bad at: the ~16k species names and the
#      free-text search strings that GA4 would bucket into "(other)" once the
#      daily cardinality limit is hit, plus report parameters and errors. A
#      Google Apps Script web app bound to a Sheet exposes a POST endpoint; the
#      BROWSER posts to it, batched, via `navigator.sendBeacon()`.
#
# WHY THE BROWSER SENDS BOTH: `sendBeacon` is fire-and-forget by definition, is
# queued by the browser rather than the page, and survives unload. Server-side R
# facts (the resolved scientific name behind a picker value, a report's
# parameters, a row count) reach it via `ms_track()`, which only pushes a small
# websocket message the session already has open — no `httr2`, no blocking, no
# background worker, no flush queue in R.
#
# BATCHING: events accumulate client-side and flush when the queue reaches
# `.MS_LOG_BATCH` events, every `.MS_LOG_INTERVAL_MS`, or when the page is
# hidden/unloaded. This keeps well inside the Apps Script execution quota, which
# a per-interaction POST would burn through.
#
# CORS: the beacon body is sent as `text/plain;charset=UTF-8` so it stays a CORS
# "simple request" and triggers no preflight — an Apps Script `/exec` endpoint
# does not answer `OPTIONS`, so an `application/json` body would be dropped.
#
# SETUP is documented in `vignettes`/the workflows repo; the short version:
#   1. Create a Sheet whose first row is the header written by `ms_log_header()`.
#   2. Extensions -> Apps Script, paste `ms_apps_script()`, Deploy -> New
#      deployment -> Web app, execute as "Me", access "Anyone". Copy the /exec URL.
#   3. Set `MSENS_LOG_URL` to that URL for the app. Unset => the Sheet leg is a
#      silent no-op and only GA4 receives events.

# the production GA4 measurement ID (property 413466008). ONE id across every
# product: gtag scopes the `_ga` cookie to the registrable domain, so a single
# stream already spans marinesensitivity.org and app.marinesensitivity.org.
# Per-product ids would fragment sessions and make cross-product journeys
# (homepage -> docs -> app) unrecoverable.
.MS_GA_ID <- "G-9HW6L751XG"

# client-side batching: flush at N queued events or after T ms, whichever first
.MS_LOG_BATCH       <- 10L
.MS_LOG_INTERVAL_MS <- 15000L

# GA4 hard limits — enforced client-side before the gtag leg (the Sheet leg
# always receives the untruncated value).
.MS_GA_MAX_EVENT_CHARS <- 40L
.MS_GA_MAX_PARAM_CHARS <- 100L

#' Column header for the usage-log Sheet
#'
#' The exact first row the Google Sheet must carry for [ms_apps_script()] to
#' append into. Kept here so the Sheet, the Apps Script, and the client payload
#' cannot drift.
#'
#' @return character vector of column names, in order
#' @examples
#' ms_log_header()
#' @export
#' @concept analytics
ms_log_header <- function()
  c("timestamp", "ip", "session", "event", "params", "n_rows", "ms", "status",
    "error", "app_version", "app", "client_id", "session_id", "page",
    "referrer", "user_agent")

# reserved parameter names hoisted out of `params` into their own Sheet columns,
# so they stay numeric/filterable instead of being buried in the params JSON.
.MS_LOG_METRICS <- c("n_rows", "ms", "status", "error")

#' Build a tracking-event payload
#'
#' The pure payload constructor shared by [ms_track()] and the tests. Normalises
#' the event name to GA4's rules (lowercase, `[a-z0-9_]`, leading letter,
#' truncated to 40 characters) and drops `NULL`/`NA`/empty parameters so the
#' Sheet's `params` column stays readable.
#'
#' Parameter *values* are NOT truncated here — the Sheet leg wants the full
#' string (a long search term, a full scientific name). The client truncates to
#' 100 characters for the gtag leg only.
#'
#' @param event event name, e.g. `"select_species"`; coerced to GA4-safe form
#' @param ... named event parameters (scalars); `NULL`/`NA`/`""` are dropped
#' @return list with `event` (character scalar) and `params` (named list)
#' @examples
#' ms_event("Select Species", scientific_name = "Dermochelys coriacea", n = 3)
#' ms_event("download", file = NULL, format = "csv")   # NULL dropped
#' @export
#' @concept analytics
ms_event <- function(event, ...) {
  stopifnot(length(event) == 1L, !is.na(event), nzchar(event))

  # GA4: lowercase, non-alphanumerics -> "_", must start with a letter, <= 40 chars
  nm <- tolower(as.character(event))
  nm <- gsub("[^a-z0-9_]+", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- gsub("^_+|_+$", "", nm)
  if (!grepl("^[a-z]", nm)) nm <- paste0("e_", nm)
  nm <- substr(nm, 1L, .MS_GA_MAX_EVENT_CHARS)

  params <- list(...)
  if (length(params) > 0) {
    if (is.null(names(params)) || any(!nzchar(names(params))))
      stop("all `...` parameters to ms_event() must be named")
    # drop NULL / NA / "" so absent facts don't clutter the Sheet's params column
    keep <- vapply(
      params,
      function(v) length(v) >= 1L && !all(is.na(v)) && any(nzchar(as.character(v))),
      logical(1))
    params <- params[keep]
  }

  # hoist the reserved metric names into their own bag. n_rows / ms stay NUMERIC:
  # Apps Script setValues() writes a JS string as text, which would make the
  # column unchartable.
  metrics <- params[intersect(.MS_LOG_METRICS, names(params))]
  params  <- params[setdiff(names(params), .MS_LOG_METRICS)]
  if (!is.null(metrics$n_rows)) metrics$n_rows <- as.integer(metrics$n_rows)
  if (!is.null(metrics$ms))     metrics$ms     <- round(as.numeric(metrics$ms), 1)
  for (k in intersect(c("status", "error"), names(metrics)))
    metrics[[k]] <- as.character(metrics[[k]])

  # everything else is text; a multi-value parameter collapses to one readable
  # cell rather than a nested JSON array
  params <- lapply(params, function(v) paste(as.character(v), collapse = ", "))
  list(event = nm, params = params, metrics = metrics)
}

#' Send a tracking event from the Shiny server to the browser
#'
#' Pushes an event over the session's existing websocket; the client-side
#' handler installed by [ga_js()] forwards it to GA4 and queues it for the Sheet
#' log. **Non-blocking** — no HTTP request is made on the R side, so a slow or
#' unreachable log endpoint can never stall a reactive.
#'
#' Use it for facts only the server knows (the scientific name behind a picker
#' value, a report's parameters, a row count, an error); pure UI interactions are
#' better tracked client-side by [ga_js()]'s delegated handlers.
#'
#' @param session the Shiny `session` object
#' @param event event name, passed to [ms_event()]
#' @param ... named event parameters, passed to [ms_event()]
#' @return the payload, invisibly
#' @examples
#' \dontrun{
#' ms_track(session, "select_species",
#'          mdl_key = input$sel_sp, scientific_name = sp$scientific_name)
#' }
#' @export
#' @concept analytics
ms_track <- function(session, event, ...) {
  payload <- ms_event(event, ...)
  # never let instrumentation break the app: a closed session or a missing
  # handler must be silent, exactly like the Sheet leg being unconfigured.
  try(session$sendCustomMessage("msTrack", payload), silent = TRUE)
  invisible(payload)
}

#' Best-effort client IP from a Shiny request
#'
#' Reads `X-Forwarded-For` (set by the Caddy reverse proxy the apps sit behind)
#' and falls back to the direct `REMOTE_ADDR`. Never errors.
#'
#' **Pass the `req` of a `ui` function, not a `session`, when you can.**
#' shiny-server does not proxy the websocket upgrade — it opens a fresh
#' localhost connection to the R worker — so a session's `request` carries no
#' `X-Forwarded-For` and its `REMOTE_ADDR` is always `127.0.0.1`, no matter how
#' correctly Caddy is configured (verified on msens1: the page GET shows the real
#' address while the websocket handshake shows `HTTP_HOST 127.0.0.1:<worker port>`
#' and no forwarded header at all). The page's HTTP request, which
#' `ui = function(req)` receives, is the only place the real client IP survives.
#' See the `ip` argument of [ga_js()].
#'
#' Behind Cloudflare (the signed-in preview host is proxied through its edge)
#' the connecting peer is a Cloudflare address, and the visitor is named by
#' `CF-Connecting-IP`; that wins when present, then the first `X-Forwarded-For`
#' hop, then `REMOTE_ADDR`. Analytics only -- nothing here is trusted for policy.
#'
#' @param x a Shiny `session`, or the `req` environment handed to a `ui`
#'   function (anything carrying the request fields directly)
#' @return character scalar, or `NA_character_` if unavailable
#' @examples
#' ms_client_ip(list(request = list(HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1")))
#' ms_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))   # a ui(req)
#' ms_client_ip(list(HTTP_CF_CONNECTING_IP = "198.51.100.4",
#'                   HTTP_X_FORWARDED_FOR = "198.51.100.4, 172.70.0.1"))
#' @export
#' @concept analytics
ms_client_ip <- function(x) {
  tryCatch({
    # a session carries the fields under $request; a ui() req carries them itself
    req <- if (is.null(x$request)) x else x$request
    cf  <- req[["HTTP_CF_CONNECTING_IP"]]
    xff <- req[["HTTP_X_FORWARDED_FOR"]]
    if (!is.null(cf) && nzchar(cf)) trimws(cf)
    else if (!is.null(xff) && nzchar(xff)) trimws(strsplit(xff, ",")[[1]][1])
    else {
      addr <- req[["REMOTE_ADDR"]]
      if (is.null(addr) || !nzchar(addr)) NA_character_ else addr
    }
  }, error = function(e) NA_character_)
}

#' Hand the browser the session facts only the server knows
#'
#' The Shiny session token cannot be read in JavaScript, so the server pushes it
#' once at session start; the client then stamps it on every queued row (the
#' `session` column of [ms_log_header()]). Call it at the top of the `server`
#' function, before any [ms_track()] call, so no event is written without it.
#'
#' The token is authoritative, the IP is only a **fallback**: behind shiny-server
#' a session sees `127.0.0.1`, so an `ip` already baked into the page by
#' [ga_js()] wins and this one is ignored. Without that rule the websocket's
#' useless address would silently overwrite the good one a moment after the page
#' supplied it.
#'
#' @param session the Shiny `session` object
#' @return the sent list, invisibly
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   msens::ms_track_session(session)
#'   ...
#' }
#' }
#' @export
#' @concept analytics
ms_track_session <- function(session) {
  msg <- list(
    ip      = ms_client_ip(session),
    session = tryCatch(session$token, error = function(e) NA_character_))
  try(session$sendCustomMessage("msTrackSession", msg), silent = TRUE)
  invisible(msg)
}

#' Time a query, log its shape, and re-raise any error
#'
#' Wraps an expression so the Sheet records how long it took, how many rows it
#' returned, and whether it failed — in the dedicated `ms` / `n_rows` / `status`
#' / `error` columns, which stay numeric and chartable rather than being buried
#' in the `params` JSON.
#'
#' The result passes through untouched (including a lazy `dbplyr` table, whose
#' row count is deliberately NOT forced), and an error is re-raised after being
#' logged, so wrapping a call never changes behaviour.
#'
#' @param session the Shiny `session` object
#' @param event event name, passed to [ms_event()]
#' @param params named list of event parameters
#' @param expr the expression to time
#' @return whatever `expr` returns
#' @examples
#' \dontrun{
#' d <- ms_track_query(session, "download_species_csv", list(area = hdr),
#'                     species_for_zone(con, "programarea_key", key))
#' }
#' @export
#' @concept analytics
ms_track_query <- function(session, event, params = list(), expr) {
  t0  <- Sys.time()
  res <- tryCatch(force(expr), error = function(e) e)
  ms  <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000

  send <- function(extra) {
    payload <- do.call(ms_event, c(list(event), params, extra))
    try(session$sendCustomMessage("msTrack", payload), silent = TRUE)
  }
  if (inherits(res, "error")) {
    send(list(ms = ms, status = "error", error = conditionMessage(res)))
    stop(res)
  }
  # a lazy dbplyr table has no row count until collected — don't force one
  n <- tryCatch(if (is.data.frame(res)) nrow(res) else NA_integer_,
                error = function(e) NA_integer_)
  send(list(n_rows = n, ms = ms, status = "ok"))
  res
}

#' Analytics `<head>` snippet (GA4 + batched Sheet beacon)
#'
#' Generates the self-contained HTML/JS installed once per page: the GA4 gtag
#' loader, the client-side event queue that beacons to the usage-log Sheet, a
#' `Shiny.addCustomMessageHandler("msTrack", ...)` handler so [ms_track()] works,
#' and a `window.msTrack(event, params)` helper for page JS.
#'
#' The same snippet serves every product; `content_group` is what separates them
#' in GA4 reporting, so no product needs its own measurement ID.
#'
#' @param app short app/product id recorded on every event, e.g. `"scores"`
#' @param content_group GA4 content group for reporting; defaults to `app`
#' @param app_version version string recorded on every event — the deployed git
#'   commit, so a Sheet row ties back to the exact code that produced it
#' @param ip client IP to stamp on every logged row, from
#'   `ms_client_ip(req)` in a `ui = function(req)`. Behind shiny-server this is
#'   the ONLY place a real address exists — see [ms_client_ip()].
#' @param measurement_id GA4 measurement ID; defaults to the project property
#' @param log_url Apps Script `/exec` endpoint for the Sheet log. Defaults to the
#'   `MSENS_LOG_URL` environment variable; empty means the Sheet leg is a silent
#'   no-op (GA4 still receives events).
#' @return character scalar of raw HTML (`<script>` tags)
#' @examples
#' substr(ga_js(app = "scores", app_version = "v8"), 1, 60)
#' @export
#' @concept analytics
ga_js <- function(app,
                  content_group  = app,
                  app_version    = "",
                  ip             = "",
                  measurement_id = .MS_GA_ID,
                  log_url        = Sys.getenv("MSENS_LOG_URL", "")) {
  stopifnot(length(app) == 1L, nzchar(app))
  # ms_client_ip() returns NA when it cannot tell; the page wants an empty string
  if (length(ip) != 1L || is.na(ip)) ip <- ""

  # JSON-encode every interpolated value so a stray quote can't break the script
  j <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

  glue::glue(
    '<!-- Google tag (gtag.js) + MarineSensitivity usage log -->
<script async src="https://www.googletagmanager.com/gtag/js?id=<<measurement_id>>"></script>
<script>
(function () {
  var GA_ID    = <<j(measurement_id)>>;
  var APP      = <<j(app)>>;
  var APP_VER  = <<j(app_version)>>;
  var GROUP    = <<j(content_group)>>;
  var LOG_URL  = <<j(log_url)>>;
  var BATCH    = <<.MS_LOG_BATCH>>;
  var INTERVAL = <<.MS_LOG_INTERVAL_MS>>;

  // ---- GA4 ----------------------------------------------------------------
  window.dataLayer = window.dataLayer || [];
  function gtag() { dataLayer.push(arguments); }
  window.gtag = window.gtag || gtag;
  gtag("js", new Date());
  gtag("config", GA_ID, { content_group: GROUP, app_name: APP, app_version: APP_VER });

  // ---- identity -----------------------------------------------------------
  // Independent of GA so the Sheet log still stitches sessions when gtag is
  // blocked. client_id persists across visits; session_id is per tab.
  function uid() {
    return (Date.now().toString(36) + Math.random().toString(36).slice(2, 10));
  }
  function stored(store, key) {
    try {
      var v = store.getItem(key);
      if (!v) { v = uid(); store.setItem(key, v); }
      return v;
    } catch (e) { return "na"; }   // private mode / storage disabled
  }
  var CLIENT_ID  = stored(window.localStorage,   "msens_client_id");
  var SESSION_ID = stored(window.sessionStorage, "msens_session_id");

  // The client IP and the Shiny session token are server-only facts. The IP is
  // baked in from the PAGE request — the only request that still carries
  // X-Forwarded-For, because shiny-server rebuilds the websocket handshake as a
  // fresh localhost connection — and the token arrives from ms_track_session().
  var SERVER_IP = <<j(ip)>>, SERVER_SESSION = "";

  // ---- Sheet log: queue + batched beacon -----------------------------------
  var queue = [];

  function flush() {
    if (!LOG_URL || !queue.length) return;
    var rows = queue.splice(0, queue.length);
    var body = JSON.stringify({ rows: rows });
    try {
      // text/plain keeps this a CORS "simple request": Apps Script /exec does
      // not answer OPTIONS, so an application/json preflight would be dropped.
      var blob = new Blob([body], { type: "text/plain;charset=UTF-8" });
      if (!(navigator.sendBeacon && navigator.sendBeacon(LOG_URL, blob))) {
        fetch(LOG_URL, { method: "POST", body: body, keepalive: true,
                         mode: "no-cors", headers: { "Content-Type": "text/plain" } })
          .catch(function () {});
      }
    } catch (e) { /* logging must never surface to the user */ }
  }

  setInterval(flush, INTERVAL);
  // sendBeacon survives unload; "hidden" also fires on mobile backgrounding,
  // which "unload" does not.
  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden") flush();
  });
  window.addEventListener("pagehide", flush);

  // ---- the one entry point -------------------------------------------------
  // Sends to BOTH legs: GA4 (truncated to its 100-char param limit) and the
  // Sheet queue (full values).
  window.msTrack = function (event, params, metrics) {
    params  = params  || {};
    metrics = metrics || {};
    try {
      var ga = { content_group: GROUP, app_name: APP, app_version: APP_VER };
      Object.keys(params).forEach(function (k) {
        var v = params[k];
        ga[k] = (typeof v === "string") ? v.slice(0, <<.MS_GA_MAX_PARAM_CHARS>>) : v;
      });
      if (window.gtag) gtag("event", event, ga);
    } catch (e) {}

    if (!LOG_URL) return;
    function m(k) { return (metrics[k] === undefined || metrics[k] === null) ? "" : metrics[k]; }
    queue.push({
      timestamp:   new Date().toISOString(),
      ip:          SERVER_IP,
      session:     SERVER_SESSION,
      event:       event,
      params:      JSON.stringify(params),
      n_rows:      m("n_rows"),
      ms:          m("ms"),
      status:      m("status"),
      error:       m("error"),
      app_version: APP_VER,
      app:         APP,
      client_id:   CLIENT_ID,
      session_id:  SESSION_ID,
      page:        location.pathname + location.search,
      referrer:    document.referrer || "",
      user_agent:  navigator.userAgent || ""
    });
    if (queue.length >= BATCH) flush();
  };

  // ---- server -> browser (msens::ms_track) ---------------------------------
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    // an EMPTY R list serialises as [] rather than {}, which would land in the
    // params column of the Sheet as "[]" — normalise both bags to objects.
    function obj(x) { return (!x || Array.isArray(x)) ? {} : x; }
    Shiny.addCustomMessageHandler("msTrack", function (m) {
      window.msTrack(m.event, obj(m.params), obj(m.metrics));
    });
    Shiny.addCustomMessageHandler("msTrackSession", function (m) {
      // the IP reported here is a FALLBACK, never an override: behind
      // shiny-server a session always sees 127.0.0.1, which would clobber the
      // real address the page request supplied.
      if (!SERVER_IP && m.ip) SERVER_IP = m.ip;
      SERVER_SESSION = m.session || "";
    });
  }
})();
</script>',
    .open = "<<", .close = ">>")
}

#' Analytics `<head>` snippet as a Shiny tag
#'
#' [ga_js()] wrapped in [htmltools::HTML()] for use inside `tags$head()`.
#'
#' @inheritParams ga_js
#' @return an `html` object
#' @examples
#' \dontrun{
#' ui <- bslib::page_sidebar(
#'   tags$head(msens::ga_head("scores", app_version = "v8")), ...)
#' }
#' @export
#' @concept analytics
ga_head <- function(app,
                    content_group  = app,
                    app_version    = "",
                    ip             = "",
                    measurement_id = .MS_GA_ID,
                    log_url        = Sys.getenv("MSENS_LOG_URL", ""))
  htmltools::HTML(ga_js(app, content_group, app_version, ip, measurement_id, log_url))

#' Apps Script source for the usage-log Sheet
#'
#' The `doPost()` handler to paste into the Sheet's bound Apps Script project.
#' It appends a **batch** of rows in one `setValues()` call (the client sends
#' `{rows: [...]}`), which is what keeps the write cost — and the Apps Script
#' execution quota — flat regardless of interaction rate.
#'
#' Column order is taken from [ms_log_header()], so the Sheet, the script, and
#' the client payload cannot drift.
#'
#' @return character scalar of JavaScript source
#' @examples
#' cat(ms_apps_script())
#' @export
#' @concept analytics
ms_apps_script <- function() {
  cols <- ms_log_header()
  glue::glue(
    '// Code.gs — MarineSensitivity usage log (bound to the log Sheet).
// Deploy: Deploy > New deployment > type "Web app",
//         execute as "Me", who has access "Anyone". Copy the /exec URL into
//         the MSENS_LOG_URL environment variable for the Shiny apps.
//
// The client (msens::ga_js) posts {rows:[{...}, ...]} as text/plain so the
// request stays CORS-simple (this endpoint does not answer OPTIONS).

var COLS = <<jsonlite::toJSON(cols)>>;

// Health check. Without this, opening the /exec URL in a browser returns
// "Script function not found: doGet", which looks like a broken or unauthorized
// deployment and sends you hunting through deployment settings. It is not — the
// client only ever POSTs. A GET now answers {ok:true,...} so the endpoint can be
// verified at a glance. Reports the row count so you can confirm writes land.
function doGet(e) {
  try {
    var sh = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    return ContentService
      .createTextOutput(JSON.stringify({ ok: true, endpoint: "msens-usage-log", rows: sh.getLastRow() - 1 }))
      .setMimeType(ContentService.MimeType.JSON);
  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ ok: false, error: String(err) }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function doPost(e) {
  try {
    var sh   = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    var body = JSON.parse(e.postData.contents);
    var rows = body.rows || [body];
    if (!rows.length) return _ok(0);

    // one setValues() for the whole batch — far cheaper than appendRow() per event
    var values = rows.map(function (r) {
      return COLS.map(function (c) { return r[c] === undefined ? "" : r[c]; });
    });
    sh.getRange(sh.getLastRow() + 1, 1, values.length, COLS.length).setValues(values);
    return _ok(values.length);
  } catch (err) {
    return ContentService
      .createTextOutput(JSON.stringify({ ok: false, error: String(err) }))
      .setMimeType(ContentService.MimeType.JSON);
  }
}

function _ok(n) {
  return ContentService
    .createTextOutput(JSON.stringify({ ok: true, n: n }))
    .setMimeType(ContentService.MimeType.JSON);
}',
    .open = "<<", .close = ">>")
}
