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
  expect_true(grepl("if (!p || Array.isArray(p)) p = {};", js, fixed = TRUE))
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

  # every header column is populated by the client payload builder in ga_js()
  js <- ga_js("scores", log_url = "https://example.com/exec")
  for (col in hdr)
    expect_true(grepl(paste0(col, ":"), js, fixed = TRUE),
                info = paste("ga_js() queue row is missing column:", col))
})
