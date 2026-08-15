# The picker is what tells a reader WHICH release is on screen. Two things must
# never happen: the current version failing to be marked (so a v4 chart reads as
# current), and a retired or pre-release version being shown without saying so.

fake_versions <- function() data.frame(
  ver      = c("v9", "v8", "v7", "v6"),
  status   = c("prerelease", "released", "released", "retired"),
  released = c("2026-09-01", "2026-07-28", "2026-06-12", "2026-04-09"),
  title    = c("Next", "Marine Atlas", "Validity decoupled", "IUCN outside EEZ"),
  stringsAsFactors = FALSE)

test_that("the version on screen is marked, and the others link", {
  h <- as.character(version_picker_html("v7", fake_versions()))
  expect_match(h, "list-group-item active")
  expect_match(h, "showing")
  expect_match(h, "href='\\?ver=v8'")
  expect_match(h, "href='\\?ver=v6'")
  # the current one must NOT also be a link away from itself
  expect_false(grepl("href='\\?ver=v7'", h))
})

test_that("retired and pre-release are labelled, not hidden", {
  h <- as.character(version_picker_html("v8", fake_versions()))
  expect_match(h, "pre-release")   # v9
  expect_match(h, "retired")       # v6
  expect_match(h, "v6")            # still listed: citations point at old versions
})

test_that("newest first, regardless of input order", {
  v <- fake_versions()[c(3, 1, 4, 2), ]
  h <- as.character(version_picker_html("v8", v))
  expect_lt(regexpr("v9", h), regexpr("v6", h))
})

test_that("the link shape is caller-controlled (docs use paths, apps use ?ver=)", {
  h <- as.character(version_picker_html("v8", fake_versions(),
                                        href = function(v) sprintf("/docs/%s/", v)))
  expect_match(h, "href='/docs/v7/'")
  expect_false(grepl("\\?ver=", h))
})

test_that("a restricted version is shown locked, and linked to the preview host when asked", {
  v <- fake_versions()                       # v9 prerelease -> restricted by default
  # public app: restricted rows link OUT to the signed-in preview host
  h <- as.character(version_picker_html(
    "v8", v, href_restricted = function(x) sprintf("https://preview.example.org/scores/?ver=%s", x)))
  expect_match(h, "restricted")
  expect_match(h, "href='https://preview.example.org/scores/\\?ver=v9'")
  expect_match(h, "href='\\?ver=v7'")      # public rows keep the in-app link
  expect_false(grepl("href='\\?ver=v9'", h))
  # preview instance: no href_restricted -> restricted rows render in place
  h2 <- as.character(version_picker_html("v8", v))
  expect_match(h2, "href='\\?ver=v9'")
  expect_match(h2, "restricted")            # still labelled, so a reviewer knows
  # an explicit public pre-release is NOT locked
  v$access <- c("public", "public", "public", "public")
  expect_false(grepl("restricted", as.character(version_picker_html("v8", v))))
})
