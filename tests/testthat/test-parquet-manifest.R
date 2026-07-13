# tests for the v8 Parquet writers + content-addressed manifest change detection

test_that("write_atlas_parquet writes Parquet V2 + zstd", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  d  <- withr::local_tempdir()
  pq <- file.path(d, "a.parquet")
  x  <- data.frame(mdl_key = paste0("am|", 1:500), cell_id = 1:500L,
                   val = as.double((1:500) %% 100))
  write_atlas_parquet(x, pq)
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  meta <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT format_version FROM parquet_file_metadata(%s)", DBI::dbQuoteString(con, pq)))
  comp <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT compression FROM parquet_metadata(%s)", DBI::dbQuoteString(con, pq)))
  expect_equal(as.integer(meta$format_version), 2L)
  expect_true(all(comp$compression == "ZSTD"))
})

test_that("hash_parquet is order-independent and change-sensitive", {
  skip_if_not_installed("arrow")
  d  <- withr::local_tempdir()
  x  <- data.frame(mdl_key = paste0("rng|", 1:1000), cell_id = 1:1000L,
                   val = as.double((1:1000) %% 100))
  p1 <- file.path(d, "a.parquet"); p2 <- file.path(d, "b.parquet"); p3 <- file.path(d, "c.parquet")
  write_atlas_parquet(x, p1)
  write_atlas_parquet(x[sample(nrow(x)), ], p2)          # same content, shuffled
  x3 <- x; x3$val[1] <- 999
  write_atlas_parquet(x3, p3)                            # one value changed
  expect_identical(hash_parquet(p1), hash_parquet(p2))   # order-independent
  expect_false(identical(hash_parquet(p1), hash_parquet(p3)))
})

test_that("copy_atlas_parquet handles unordered/sorted/partitioned and restores setting", {
  skip_if_not_installed("duckdb")
  d   <- withr::local_tempdir()
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE t AS SELECT ('m|'||i) mdl_key, i cell_id, (i%50)::DOUBLE val, (i%4)::INT mdl_id FROM range(1,5000) tbl(i)")
  before <- DBI::dbGetQuery(con, "SELECT current_setting('preserve_insertion_order') v")$v
  copy_atlas_parquet(con, "SELECT mdl_key, cell_id, val FROM t", file.path(d, "u.parquet"))
  copy_atlas_parquet(con, "SELECT mdl_key, cell_id, val FROM t", file.path(d, "s.parquet"), order_by = "mdl_key")
  copy_atlas_parquet(con, "SELECT mdl_key, cell_id, val, mdl_id FROM t", file.path(d, "p"), partition_by = "mdl_id")
  after <- DBI::dbGetQuery(con, "SELECT current_setting('preserve_insertion_order') v")$v
  expect_identical(before, after)                                          # setting restored
  expect_identical(hash_parquet(file.path(d, "u.parquet"), con),
                   hash_parquet(file.path(d, "s.parquet"), con))           # sorted == unordered content
  expect_true(dir.exists(file.path(d, "p")))
  expect_identical(hash_query(con, "SELECT mdl_key, cell_id, val FROM t"),
                   hash_parquet(file.path(d, "u.parquet"), con))           # table == parquet content
})

test_that("write_manifest is deterministic, idempotent, timestamp-free", {
  d  <- withr::local_tempdir()
  mf <- file.path(d, "m.json")
  write_manifest(mf, "tgt", content_hash = "abc123", stats = list(ver = "v8", n = 5L))
  m0 <- file.info(mf)$mtime
  Sys.sleep(1.1)
  # same content, keys in a different order -> no rewrite, mtime preserved
  write_manifest(mf, "tgt", content_hash = "abc123", stats = list(n = 5L, ver = "v8"))
  expect_identical(m0, file.info(mf)$mtime)
  # no wall-clock field ever
  expect_false(any(grepl("built|time", names(jsonlite::fromJSON(mf)))))
  # force rewrites
  Sys.sleep(1.1)
  write_manifest(mf, "tgt", content_hash = "abc123", stats = list(ver = "v8", n = 5L), force = TRUE)
  expect_gt(file.info(mf)$mtime, m0)
})

test_that("force_target reads env flags", {
  withr::local_envvar(MSENS_FORCE = "", MSENS_FORCE_ALL = "")
  expect_false(force_target("x"))
  withr::local_envvar(MSENS_FORCE = "a,x,c")
  expect_true(force_target("x"))
  expect_false(force_target("y"))
  withr::local_envvar(MSENS_FORCE = "", MSENS_FORCE_ALL = "1")
  expect_true(force_target("anything"))
})

test_that("require_duckdb enforces the version floor", {
  expect_true(require_duckdb("1.5.0"))
  expect_error(require_duckdb("99.0.0"), "duckdb")
})
