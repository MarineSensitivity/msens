# The content hash decides whether two releases share a COG. Both failure modes
# are silent: a hash that varies with row order publishes 226k near-duplicates,
# and a hash that collides serves the WRONG SPECIES' map under the right name.

mk_con <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

# a tiny surface table: model 1 and 2 differ, model 3 is model 1 shuffled
seed_surface <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE mc (mdl_seq INTEGER, cell_id BIGINT, val TINYINT)")
  DBI::dbExecute(con, "INSERT INTO mc VALUES
    (1, 10, 5), (1, 11, 7), (1, 12, 9),
    (2, 10, 5), (2, 11, 7), (2, 12, 8),
    (3, 12, 9), (3, 10, 5), (3, 11, 7)")   # same payload as 1, inserted out of order
  con
}

test_that("the fingerprint is order-independent but content-sensitive", {
  h <- content_hashes(seed_surface(mk_con()), "mc", "mdl_seq")
  h <- h[order(h$mdl_seq), ]
  # model 3 is model 1 with rows shuffled -> MUST dedup to the same object
  expect_equal(h$content_hash[h$mdl_seq == 3], h$content_hash[h$mdl_seq == 1])
  # model 2 differs in a single cell's value -> MUST NOT
  expect_false(h$content_hash[h$mdl_seq == 2] == h$content_hash[h$mdl_seq == 1])
  expect_equal(h$n, c(3, 3, 3))
  expect_true(all(nchar(h$content_hash) == 16L))
})

test_that("a cell moving between models changes both hashes", {
  con <- mk_con()
  DBI::dbExecute(con, "CREATE TABLE a (mdl_seq INTEGER, cell_id BIGINT, val TINYINT)")
  DBI::dbExecute(con, "INSERT INTO a VALUES (1, 10, 5), (1, 11, 5)")
  DBI::dbExecute(con, "CREATE TABLE b (mdl_seq INTEGER, cell_id BIGINT, val TINYINT)")
  DBI::dbExecute(con, "INSERT INTO b VALUES (1, 10, 5), (1, 12, 5)")
  expect_false(identical(content_hashes(con, "a", "mdl_seq")$content_hash,
                         content_hashes(con, "b", "mdl_seq")$content_hash))
})

test_that("duplicate rows do not cancel out (bit_xor alone would)", {
  # bit_xor(h, h) == 0, so an xor-only fingerprint makes an exact-duplicated
  # surface look identical to an empty one. count + sum are what prevent that.
  con <- mk_con()
  DBI::dbExecute(con, "CREATE TABLE d (mdl_seq INTEGER, cell_id BIGINT, val TINYINT)")
  DBI::dbExecute(con, "INSERT INTO d VALUES (1, 10, 5), (2, 10, 5), (2, 10, 5)")
  h <- content_hashes(con, "d", "mdl_seq")
  expect_false(h$content_hash[h$mdl_seq == 1] == h$content_hash[h$mdl_seq == 2])
})

test_that("v1-v7's `value` column hashes as readily as v8's `val`", {
  con <- mk_con()
  DBI::dbExecute(con, "CREATE TABLE v7 (mdl_seq INTEGER, cell_id BIGINT, value TINYINT)")
  DBI::dbExecute(con, "INSERT INTO v7 VALUES (1, 10, 5), (1, 11, 7)")
  expect_equal(nrow(content_hashes(con, "v7", "mdl_seq", cols = c("cell_id", "value"))), 1L)
})

test_that("a double-typed hash is rejected rather than silently truncated", {
  # DuckDB hash() is UBIGINT; without ::VARCHAR the R driver returns a double and
  # loses ~4 digits of precision, aliasing distinct models onto one COG.
  expect_error(.assert_hash_chr(data.frame(n = 3, x = 1.315468e+19, s = 2.2e+19)),
               "::VARCHAR cast was lost")
  expect_true(.assert_hash_chr(data.frame(n = 3, x = "13154680000000000000", s = "22")))
})

test_that("the SQL keeps the cast and the column order it was given", {
  sql <- content_hash_sql("mc", "mdl_seq", cols = c("cell_id", "val"))
  expect_match(sql, "bit_xor\\(hash\\(\"cell_id\", \"val\"\\)\\)::VARCHAR")
  expect_match(sql, "sum\\(hash\\(.*\\)\\)::VARCHAR")
  expect_match(sql, 'GROUP BY "mdl_seq"')
})

test_that("object keys are namespaced by grid, since cell_id needs one to mean anything", {
  expect_equal(content_key("usa05", "abc123"), "cog/usa05/abc123.tif")
  # the same hash on a different grid is a DIFFERENT map and must not collide
  expect_false(content_key("usa05", "abc123") == content_key("global05", "abc123"))
  expect_equal(content_key("usa05", c("a", "b")), c("cog/usa05/a.tif", "cog/usa05/b.tif"))
  expect_match(content_url("usa05", "abc123", base = "https://x/marine-atlas"),
               "^https://x/marine-atlas/cog/usa05/abc123\\.tif$")
})
