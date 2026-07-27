test_that("ms_event normalises event names to GA4 rules", {
  # lowercase + non-alphanumerics collapsed to a single underscore
  expect_equal(ms_event("Select Species")$event, "select_species")
  expect_equal(ms_event("download-CSV")$event,   "download_csv")
  expect_equal(ms_event("tab::Plot of Scores")$event, "tab_plot_of_scores")
  # leading/trailing separators trimmed
  expect_equal(ms_event(" _map click_ ")$event, "map_click")
  # must start with a letter
  expect_equal(ms_event("404_error")$event, "e_404_error")
  # <= 40 chars
  expect_equal(nchar(ms_event(strrep("a", 60))$event), 40L)
  # already-valid names pass through untouched
  expect_equal(ms_event("select_layer")$event, "select_layer")
})

test_that("ms_event keeps scalar params and drops absent ones", {
  p <- ms_event("select_species",
                mdl_key         = "ms_merge|WORMS:137209",
                scientific_name = "Dermochelys coriacea",
                n_datasets      = 3)$params
  expect_equal(names(p), c("mdl_key", "scientific_name", "n_datasets"))
  expect_equal(p$mdl_key, "ms_merge|WORMS:137209")
  expect_equal(p$n_datasets, "3")   # coerced to character for the payload

  # NULL / NA / "" are dropped so the Sheet's params column stays readable
  p2 <- ms_event("download", file = NULL, fmt = NA, label = "", ver = "v8")$params
  expect_equal(names(p2), "ver")

  # no params at all -> empty list, not NULL (JSON encodes as {})
  expect_equal(ms_event("page_view")$params, list())
  expect_equal(ms_event("page_view")$metrics, list())
})

test_that("ms_event rejects bad input", {
  expect_error(ms_event(""))
  expect_error(ms_event(NA))
  expect_error(ms_event(c("a", "b")))
  expect_error(ms_event("ok", "unnamed"), "must be named")
})

test_that("ga_js embeds the configured ids and app metadata", {
  js <- ga_js(app = "species", app_version = "v8",
              log_url = "https://script.google.com/macros/s/AAA/exec")
  expect_true(grepl("G-9HW6L751XG", js, fixed = TRUE))          # default property
  expect_true(grepl('var APP      = "species"', js, fixed = TRUE))
  expect_true(grepl('var APP_VER  = "v8"', js, fixed = TRUE))
  expect_true(grepl("https://script.google.com/macros/s/AAA/exec", js, fixed = TRUE))
  # content_group defaults to app, and is what separates products in one property
  expect_true(grepl('var GROUP    = "species"', js, fixed = TRUE))
  expect_true(grepl("content_group", js, fixed = TRUE))
  # the server -> browser bridge ms_track() depends on
  expect_true(grepl('addCustomMessageHandler("msTrack"', js, fixed = TRUE))
  # REGRESSION: ms_event() returns list() for a no-param event, which serialises
  # as [] not {} — without normalising, the Sheet's params column reads "[]".
  # One obj() helper now normalises BOTH the params and metrics bags.
  expect_true(grepl("function obj(x) { return (!x || Array.isArray(x)) ? {} : x; }",
                    js, fixed = TRUE))
  expect_true(grepl("obj(m.params), obj(m.metrics)", js, fixed = TRUE))
})

test_that("ga_js keeps the Sheet beacon a CORS-simple request", {
  # REGRESSION: an application/json body triggers a preflight OPTIONS, which an
  # Apps Script /exec endpoint does not answer -> every event silently dropped.
  js <- ga_js("scores", log_url = "https://example.com/exec")
  expect_true(grepl('{ type: "text/plain;charset=UTF-8" }', js, fixed = TRUE))
  expect_true(grepl('"Content-Type": "text/plain"', js, fixed = TRUE))
  expect_false(grepl('"Content-Type": "application/json"', js, fixed = TRUE))
  # batched, not one request per interaction
  expect_true(grepl("navigator.sendBeacon", js, fixed = TRUE))
  expect_true(grepl("queue.length >= BATCH", js, fixed = TRUE))
})

test_that("ga_js with no log_url still emits GA4 but no beacon target", {
  js <- withr::with_envvar(c(MSENS_LOG_URL = ""), ga_js("scores"))
  expect_true(grepl('var LOG_URL  = ""', js, fixed = TRUE))
  expect_true(grepl("G-9HW6L751XG", js, fixed = TRUE))
  # the guard that makes the Sheet leg a silent no-op
  expect_true(grepl("if (!LOG_URL) return;", js, fixed = TRUE))
})

test_that("ga_js JSON-encodes interpolated values", {
  # a quote in an app id must not break out of the JS string literal
  js <- ga_js(app = 'a"b')
  expect_true(grepl('var APP      = "a\\"b"', js, fixed = TRUE))
})

test_that("ga_js validates app", {
  expect_error(ga_js(""))
  expect_error(ga_js(c("a", "b")))
})

test_that("ms_track pushes a msTrack message without any network I/O", {
  sent <- NULL
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      sent <<- list(type = type, message = message)
      invisible(TRUE)
    })
  out <- ms_track(fake_session, "Select Layer", layer = "score_composite")
  expect_equal(sent$type, "msTrack")
  expect_equal(sent$message$event, "select_layer")
  expect_equal(sent$message$params$layer, "score_composite")
  expect_equal(out, sent$message)
})

test_that("ms_track is silent when the session cannot receive", {
  # REGRESSION: instrumentation must never take down the app. A closed session
  # (sendCustomMessage errors) has to be swallowed.
  bad_session <- list(
    sendCustomMessage = function(type, message) stop("session closed"))
  expect_silent(ms_track(bad_session, "select_layer", layer = "x"))
  expect_error(ms_track(list(), "select_layer"), NA)   # no handler at all
})

test_that("the Sheet header, Apps Script, and client payload agree", {
  hdr <- ms_log_header()
  expect_equal(hdr[1], "timestamp")
  expect_true(all(c("app", "event", "params", "client_id", "session_id") %in% hdr))

  # the Apps Script writes exactly ms_log_header()'s columns, in order
  gs <- ms_apps_script()
  expect_true(grepl(as.character(jsonlite::toJSON(hdr)), gs, fixed = TRUE))
  # batched write, not appendRow() per event (Apps Script quota)
  expect_true(grepl("setValues(values)", gs, fixed = TRUE))
  expect_false(grepl("sh.appendRow", gs, fixed = TRUE))

  # a GET health check must exist: without doGet, opening the /exec URL returns
  # "Script function not found: doGet", which reads as a broken deployment and
  # costs real debugging time even though only POST is ever used.
  expect_true(grepl("function doGet(e)", gs, fixed = TRUE))
  expect_true(grepl("function doPost(e)", gs, fixed = TRUE))

  # every header column is populated by the client payload builder in ga_js()
  js <- ga_js("scores", log_url = "https://example.com/exec")
  for (col in hdr)
    expect_true(grepl(paste0(col, ":"), js, fixed = TRUE),
                info = paste("ga_js() queue row is missing column:", col))
})

# ---- client IP: the shiny-server websocket loses X-Forwarded-For -------------

test_that("ms_client_ip reads a ui(req) as well as a session", {
  # a ui(req) carries the fields directly; a session nests them under $request
  expect_equal(ms_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7")), "203.0.113.7")
  expect_equal(ms_client_ip(list(request = list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))),
               "203.0.113.7")
  # X-Forwarded-For is a CHAIN: the client is the first entry, not the proxy
  expect_equal(ms_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1, 172.16.0.2")),
               "203.0.113.7")
  # no forwarded header -> the direct peer, which behind shiny-server is useless
  expect_equal(ms_client_ip(list(REMOTE_ADDR = "127.0.0.1")), "127.0.0.1")
})

test_that("ms_client_ip never errors", {
  expect_true(is.na(ms_client_ip(list())))
  expect_true(is.na(ms_client_ip(list(HTTP_X_FORWARDED_FOR = ""))))
  expect_true(is.na(ms_client_ip(NULL)))
})

test_that("ga_js bakes the page IP in, and the session IP is only a FALLBACK", {
  # REGRESSION, and the whole point of the ui(req) change: shiny-server does not
  # proxy the websocket upgrade, so session$request has no X-Forwarded-For and
  # REMOTE_ADDR is 127.0.0.1. If ms_track_session()'s IP OVERRODE the page value,
  # that 127.0.0.1 would silently clobber the real address moments after the page
  # supplied it — undoing the fix with no visible symptom.
  js <- ga_js("scores", ip = "203.0.113.7", log_url = "https://example.com/exec")
  expect_true(grepl('var SERVER_IP = "203.0.113.7"', js, fixed = TRUE))
  expect_true(grepl("if (!SERVER_IP && m.ip) SERVER_IP = m.ip;", js, fixed = TRUE))
  # ... i.e. ALWAYS guarded — no line may assign SERVER_IP unconditionally
  expect_false(grepl("(?m)^\\s*SERVER_IP\\s*=\\s*m\\.ip", js, perl = TRUE))
  expect_true(grepl('addCustomMessageHandler("msTrackSession"', js, fixed = TRUE))
})

test_that("an NA ip becomes an empty string rather than the text \"NA\"", {
  js <- ga_js("scores", ip = NA_character_)
  expect_true(grepl('var SERVER_IP = ""', js, fixed = TRUE))
  expect_false(grepl('"NA"', js, fixed = TRUE))
})

test_that("ms_track_session sends the token and a fallback ip", {
  sent <- NULL
  sess <- list(token = "tok123",
               request = list(REMOTE_ADDR = "127.0.0.1"),
               sendCustomMessage = function(type, message) sent <<- list(type, message))
  out <- ms_track_session(sess)
  expect_equal(sent[[1]], "msTrackSession")
  expect_equal(sent[[2]]$session, "tok123")
  expect_equal(sent[[2]]$ip, "127.0.0.1")
  expect_equal(out, sent[[2]])
})

# ---- metrics get their own columns ------------------------------------------

test_that("reserved metric names are hoisted out of params", {
  # n_rows / ms must stay NUMERIC: Apps Script setValues() writes a JS string as
  # text, which would make the column unchartable.
  p <- ms_event("download_species_csv", area = "GAA",
                n_rows = 6471, ms = 1234.567, status = "ok")
  expect_equal(names(p$params), "area")
  expect_setequal(names(p$metrics), c("n_rows", "ms", "status"))
  expect_type(p$metrics$n_rows, "integer")
  expect_equal(p$metrics$ms, 1234.6)          # rounded to 0.1 ms
  expect_type(p$metrics$status, "character")
})

test_that("multi-value params collapse to one readable cell", {
  p <- ms_event("report_submit", areas = c("Area 1", "Area 2"))
  expect_equal(p$params$areas, "Area 1, Area 2")
})

test_that("the queued row carries every ms_log_header() column", {
  js <- ga_js("scores", log_url = "https://example.com/exec")
  for (col in ms_log_header())
    expect_true(grepl(paste0(col, ":"), js, fixed = TRUE),
                info = paste("ga_js() queue row is missing column:", col))
})

test_that("ms_track_query records shape and re-raises errors", {
  sent <- list()
  sess <- list(sendCustomMessage = function(type, message) sent[[length(sent)+1]] <<- message)

  d <- ms_track_query(sess, "q", list(area = "GAA"), data.frame(a = 1:3))
  expect_equal(nrow(d), 3L)                        # result passes through
  expect_equal(sent[[1]]$metrics$n_rows, 3L)
  expect_equal(sent[[1]]$metrics$status, "ok")
  expect_true(sent[[1]]$metrics$ms >= 0)
  expect_equal(sent[[1]]$params$area, "GAA")

  expect_error(ms_track_query(sess, "q", list(), stop("boom")), "boom")   # re-raised
  expect_equal(sent[[2]]$metrics$status, "error")
  expect_match(sent[[2]]$metrics$error, "boom")
})
