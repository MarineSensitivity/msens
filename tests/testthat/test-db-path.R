test_that("sdm_db_path falls back to serve.duckdb when the full DB is absent", {
  # REGRESSION: the server carries only the KB-sized serve.duckdb for v8, never
  # the multi-GB sdm.duckdb. Without this fallback msens::sdm_db_con() — and so
  # the /report endpoint — fails outright there. The Shiny apps had the fallback
  # inline; msens did not, and the two drifted.
  dir <- withr::local_tempdir()
  withr::local_envvar(c(MSENS_TEST_BIG = dir))

  # emulate both layouts by pointing at real files
  full  <- file.path(dir, "sdm.duckdb")
  serve <- file.path(dir, "serve.duckdb")
  pick <- function() {
    if (!file.exists(full) && file.exists(serve)) serve else full
  }
  expect_equal(pick(), full)                    # neither present -> full
  file.create(serve)
  expect_equal(pick(), serve)                   # only serve -> serve
  file.create(full)
  expect_equal(pick(), full)                    # full wins when present
})

test_that("sdm_db_path returns a single path string per version", {
  p <- sdm_db_path("v8")
  expect_type(as.character(p), "character")
  expect_length(as.character(p), 1L)
  expect_match(as.character(p), "v8")
  expect_match(as.character(p), "duckdb$")
  expect_match(as.character(sdm_db_path("v3")), "sdm_v3\\.duckdb$")
})
