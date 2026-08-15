# The version token is what binds a Shiny session to the version its PAGE was
# served for. Everything the client sends (url_search, url_pathname) is
# forgeable; the token is not. So every test here is about what a client
# CANNOT do: mint, tamper, extend, or replay across the policy.

test_that("a token round-trips its version and nothing else", {
  s <- "test-secret"
  t <- ver_token_sign("v9", secret = s)
  expect_match(t, "^v9\\.[0-9]+\\.[0-9a-f]{64}$")
  expect_equal(ver_token_verify(t, secret = s), "v9")
  expect_equal(ver_token_verify(ver_token_sign("v4b", secret = s), secret = s), "v4b")
})

test_that("a tampered token is rejected", {
  s <- "test-secret"
  t <- ver_token_sign("v9", secret = s)
  p <- strsplit(t, ".", fixed = TRUE)[[1]]
  # change the version, keep the signature: a reviewer allowed on v9 asking for v10
  expect_null(ver_token_verify(paste("v10", p[2], p[3], sep = "."), secret = s))
  # extend the expiry
  expect_null(ver_token_verify(paste(p[1], as.integer(p[2]) + 999999, p[3], sep = "."), secret = s))
  # flip a signature byte
  bad <- paste0(substr(p[3], 1, 63), if (substr(p[3], 64, 64) == "0") "1" else "0")
  expect_null(ver_token_verify(paste(p[1], p[2], bad, sep = "."), secret = s))
  # wrong secret (a token minted by a process with another secret)
  expect_null(ver_token_verify(t, secret = "other"))
})

test_that("garbage, empty and NULL tokens are NULL, never an error", {
  expect_null(ver_token_verify(NULL))
  expect_null(ver_token_verify(""))
  expect_null(ver_token_verify(NA_character_))
  expect_null(ver_token_verify("v9"))
  expect_null(ver_token_verify("v9.abc.def"))
  expect_null(ver_token_verify("../etc.1.aa"))
  expect_null(ver_token_verify(c("a", "b")))
})

test_that("an expired token is rejected, an unexpired one accepted", {
  s <- "test-secret"
  t0 <- as.POSIXct("2026-08-15 12:00:00", tz = "UTC")
  t <- ver_token_sign("v9", secret = s, ttl = 3600, now = t0)
  expect_equal(ver_token_verify(t, secret = s, now = t0 + 3599), "v9")
  expect_null(ver_token_verify(t, secret = s, now = t0 + 3601))
})

test_that("ver_token_sign refuses a non-version", {
  expect_error(ver_token_sign("latest"), "not a version label")
  expect_error(ver_token_sign("v9; DROP"), "not a version label")
})

test_that("the secret comes from MS_TOKEN_SECRET when set, else is per-process random", {
  withr::local_envvar(MS_TOKEN_SECRET = "from-env")
  expect_equal(ver_token_secret(), "from-env")
  withr::local_envvar(MS_TOKEN_SECRET = "")
  a <- ver_token_secret()
  expect_match(a, "^[0-9a-f]{64}$")        # 32 random bytes from /dev/urandom
  expect_equal(ver_token_secret(), a)      # memoised within the process
  # a token signed under the default secret verifies under the default secret
  expect_equal(ver_token_verify(ver_token_sign("v8")), "v8")
})

test_that("preview URLs put the version in the PATH, never in the query", {
  expect_equal(preview_app_url("scores", "v9", base = "https://p.example.org"),
               "https://p.example.org/v9/scores/")
  expect_equal(preview_app_url("species", "v4b", base = "https://p.example.org"),
               "https://p.example.org/v4b/species/")
  expect_equal(preview_docs_url("v9", base = "https://p.example.org"),
               "https://p.example.org/docs/v9/")
  expect_error(preview_app_url("scores", "latest"), "not a version label")
  expect_error(preview_app_url("admin", "v9"))
  withr::local_envvar(MS_PREVIEW_URL = "https://review.example.org/")
  expect_equal(preview_app_url("scores", "v9"), "https://review.example.org/v9/scores/")
})
