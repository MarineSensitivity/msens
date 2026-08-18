# Content-addressed Parquet: the digest must see the DATA and nothing else.
#
# Each test names a way two copies of a release can differ physically while
# meaning the same thing (or look the same while differing) -- that is exactly
# the class of mistake byte comparison and mtime comparison get wrong.

skip_if_not_installed("duckdb")

wr <- function(df, path, order_by = NULL, ...) {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "t", df)
  ob <- if (is.null(order_by)) "" else paste("ORDER BY", order_by)
  DBI::dbExecute(con, sprintf("COPY (SELECT * FROM t %s) TO '%s' (FORMAT PARQUET)", ob, path))
  path
}

fx <- function(n = 50) {
  d <- data.frame(
    id  = 1:n,
    nm  = letters[(0:(n - 1)) %% 26 + 1],
    val = as.numeric(1:n) / 4,
    flg = rep(c(TRUE, FALSE), length.out = n),
    stringsAsFactors = FALSE)
  # an EXACT copy of row 3, so the fixture is a genuine multiset. Appending a
  # row that merely shared an id was the first attempt, and it silently was not
  # a duplicate at all -- which the n_groups < n_rows assertion caught.
  rbind(d, d[3, ])
}

test_that("row order does not change the digest", {
  d <- fx(); td <- withr::local_tempdir()
  a <- parquet_digest(wr(d, file.path(td, "a.parquet"), order_by = "id"))
  b <- parquet_digest(wr(d, file.path(td, "b.parquet"), order_by = "val DESC, nm"))
  expect_equal(a$digest, b$digest)
  expect_equal(a$n_rows, b$n_rows)
  # and the fixture really does contain a duplicate, so this is the multiset case
  expect_lt(a$n_groups, a$n_rows)
})

test_that("rewriting the same table is a no-op to the digest", {
  d <- fx(); td <- withr::local_tempdir()
  p <- file.path(td, "x.parquet")
  a <- parquet_digest(wr(d, p))
  Sys.setenv(TZ = "UTC")
  b <- parquet_digest(wr(d, p))          # new file, new mtime, same rows
  expect_equal(a$digest, b$digest)
})

test_that("a duplicated row changes the digest (XOR alone would cancel it)", {
  d <- fx(); td <- withr::local_tempdir()
  a <- parquet_digest(wr(d, file.path(td, "a.parquet")))
  # add a SECOND copy of an already-duplicated row: a set hash cannot see this,
  # and a bare bit_xor would cancel the pair back to the original digest
  b <- parquet_digest(wr(rbind(d, d[d$id == 3L, ][1, ]), file.path(td, "b.parquet")))
  expect_false(identical(a$digest, b$digest))
  expect_equal(b$n_rows, a$n_rows + 1)
  expect_equal(b$n_groups, a$n_groups)   # same distinct rows, different multiplicity
})

test_that("a single changed value changes the digest", {
  d <- fx(); td <- withr::local_tempdir()
  e <- d; e$val[10] <- e$val[10] + 1e-9
  expect_false(identical(
    parquet_digest(wr(d, file.path(td, "a.parquet")))$digest,
    parquet_digest(wr(e, file.path(td, "b.parquet")))$digest))
})

test_that("NULL is not the empty string", {
  td <- withr::local_tempdir()
  a <- data.frame(id = 1:3, nm = c("x", NA,  "z"), stringsAsFactors = FALSE)
  b <- data.frame(id = 1:3, nm = c("x", "", "z"), stringsAsFactors = FALSE)
  expect_false(identical(
    parquet_digest(wr(a, file.path(td, "a.parquet")))$digest,
    parquet_digest(wr(b, file.path(td, "b.parquet")))$digest))
})

test_that("a rename with identical values is still a different table", {
  d <- fx(); td <- withr::local_tempdir()
  e <- d; names(e)[names(e) == "nm"] <- "name"
  a <- parquet_digest(wr(d, file.path(td, "a.parquet")))
  b <- parquet_digest(wr(e, file.path(td, "b.parquet")))
  expect_false(identical(a$schema_digest, b$schema_digest))
  expect_false(identical(a$digest, b$digest))   # schema participates in `digest`
})

test_that("splitting one file into many does not change the digest", {
  d <- fx(100); td <- withr::local_tempdir()
  one <- parquet_digest(wr(d, file.path(td, "one.parquet")))
  many <- file.path(td, "many"); dir.create(many)
  wr(d[1:40, ],   file.path(many, "p1.parquet"))
  wr(d[41:70, ],  file.path(many, "p2.parquet"))
  wr(d[71:101, ], file.path(many, "p3.parquet"))
  m <- parquet_digest(many)
  expect_equal(one$digest, m$digest)
  expect_equal(m$n_files, 3)
  expect_equal(one$n_files, 1)
})

test_that("an empty table digests as empty rather than as unreadable", {
  td <- withr::local_tempdir()
  e <- parquet_digest(wr(fx()[0, ], file.path(td, "e.parquet")))
  expect_equal(e$n_rows, 0)
  expect_equal(e$data_digest, strrep("0", 32))
  expect_true(nzchar(e$digest))
})

test_that("the manifest names one row per table, file or directory", {
  td <- withr::local_tempdir()
  wr(fx(), file.path(td, "taxon.parquet"))
  wr(fx(20), file.path(td, "zone.parquet"))
  d <- file.path(td, "model_cell"); dir.create(d)
  wr(fx(10), file.path(d, "part-0.parquet")); wr(fx(11)[1:5, ], file.path(d, "part-1.parquet"))
  m <- parquet_manifest(td)
  expect_equal(sort(m$table), c("model_cell", "taxon", "zone"))
  expect_equal(m$n_files[m$table == "model_cell"], 2)
  expect_true(all(nzchar(m$digest)))
})

test_that("the sync plan reports only what actually differs", {
  td <- withr::local_tempdir()
  s <- file.path(td, "src"); d <- file.path(td, "dst"); dir.create(s); dir.create(d)
  wr(fx(),   file.path(s, "same.parquet"));    wr(fx(),   file.path(d, "same.parquet"))
  # re-sorted on disk: byte-different, digest-identical -- the case rsync gets wrong
  wr(fx(),   file.path(s, "resorted.parquet"), order_by = "id")
  wr(fx(),   file.path(d, "resorted.parquet"), order_by = "val DESC")
  wr(fx(),   file.path(s, "changed.parquet")); wr(fx(30), file.path(d, "changed.parquet"))
  wr(fx(),   file.path(s, "only_src.parquet"))
  wr(fx(),   file.path(d, "only_dst.parquet"))

  p <- parquet_sync_plan(parquet_manifest(s), parquet_manifest(d))
  st <- setNames(p$status, p$table)
  expect_equal(st[["same"]],     "same")
  expect_equal(st[["resorted"]], "same")     # <- the point of the whole file
  expect_equal(st[["changed"]],  "changed")
  expect_equal(st[["only_src"]], "missing")
  expect_equal(st[["only_dst"]], "extra")
  # changed/missing sort to the top, because that is the work to do
  expect_true(all(p$status[1:2] %in% c("changed", "missing")))
})
