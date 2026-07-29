test_that("the R and SQL tile formulas agree exactly", {
  # CRITICAL: the release notebook partitions with cell_model_tile_sql() and
  # readers prune with cell_model_tiles(). If they disagree, queries silently
  # return NOTHING — the partition filter excludes the very rows being sought.
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ids <- c(1L, 7200L, 7201L, 1080221L, 15480268L, 25920000L,
           360000L, 360001L, 3600000L)
  DBI::dbWriteTable(con, "t", data.frame(cell_id = ids))
  sql <- DBI::dbGetQuery(
    con, paste("SELECT cell_id,", cell_model_tile_sql("cell_id"), "AS tile FROM t ORDER BY cell_id"))
  r <- vapply(sort(ids), function(i) cell_model_tiles(i), integer(1))
  expect_equal(as.integer(sql$tile), unname(r))
})

test_that("cell_model_tiles collapses a compact area to few tiles", {
  # a 10x10 block of cells inside one 50x50 tile -> exactly one tile
  base <- 1080221L
  block <- as.vector(outer(0:9, (0:9) * 7200L, "+")) + base
  expect_length(cell_model_tiles(block), 1L)

  # neighbouring cells across a tile boundary -> two tiles
  expect_length(cell_model_tiles(c(1L, 51L)), 2L)
  # ... and along the row axis too
  expect_length(cell_model_tiles(c(1L, 50L * 7200L + 1L)), 2L)
})

test_that("cell_model_tiles is deduplicated, sorted and integer", {
  t <- cell_model_tiles(c(5L, 5L, 1L, 100000L))
  expect_type(t, "integer")
  expect_false(anyDuplicated(t) > 0)
  expect_equal(t, sort(t))
})

test_that("SQL tile expression uses INTEGER division", {
  # REGRESSION: DuckDB's `/` is FLOAT division. Using it here yields a distinct
  # "tile" per cell — a partition per cell — which is how this was first written.
  s <- cell_model_tile_sql("cell_id")
  expect_true(grepl("//", s, fixed = TRUE))
  expect_false(grepl("[^/]/[^/]", s))
})

test_that("a round trip through a partitioned dataset finds the right rows", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  dir <- withr::local_tempdir()
  cells <- c(1080221L, 1080222L, 1080230L, 9000000L)
  DBI::dbWriteTable(con, "mc", data.frame(
    cell_id = rep(cells, each = 2),
    mdl_id  = rep(1:2, times = length(cells)),
    val     = seq_len(2 * length(cells))))
  DBI::dbExecute(con, sprintf(
    "COPY (SELECT %s AS tile, mdl_id, cell_id, val FROM mc) TO '%s' (FORMAT PARQUET, PARTITION_BY tile)",
    cell_model_tile_sql("cell_id"), dir))

  want   <- c(1080221L, 1080230L)
  tiles  <- cell_model_tiles(want)
  got <- DBI::dbGetQuery(con, sprintf(
    "SELECT cell_id, mdl_id FROM read_parquet('%s/**/*.parquet', hive_partitioning=1)
     WHERE tile IN (%s) AND cell_id IN (%s) ORDER BY cell_id, mdl_id",
    dir, paste(tiles, collapse = ","), paste(want, collapse = ",")))
  expect_equal(sort(unique(got$cell_id)), want)
  expect_equal(nrow(got), 4L)          # 2 cells x 2 models
})

# --- grid-aware tiling (v7 uses a 3103-wide grid, v8 a 7200-wide one) --------

test_that("writer and reader agree on tiles for a NON-default grid", {
  # the existing agreement test covers the 7200 default; v7's grid is 3103, and a
  # writer/reader mismatch would prune away the very rows sought — silently,
  # because a wrong tile id is still a valid tile id
  ncol <- 3103L
  ids  <- c(1L, 990L, 3103L, 3104L, 155150L, 6222562L)

  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "c", data.frame(cell_id = ids))
  sql <- DBI::dbGetQuery(con, sprintf(
    "SELECT cell_id, %s AS tile FROM c ORDER BY cell_id",
    cell_model_tile_sql("cell_id", ncol = ncol)))

  expect_equal(sort(unique(sql$tile)), cell_model_tiles(ids, ncol = ncol))
})

test_that("the wrong grid width yields DIFFERENT tiles — the silent-failure mode", {
  ids <- c(990L, 3103L, 3104L, 155150L)
  expect_false(identical(
    cell_model_tiles(ids, ncol = 3103L),
    cell_model_tiles(ids, ncol = 7200L)))
})

test_that("cell_grid_ncol reads the sidecar table, else falls back to 7200", {
  expect_equal(cell_grid_ncol(NULL), 7200L)

  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(cell_grid_ncol(con), 7200L)          # no cell_grid table (every v8 db)

  DBI::dbWriteTable(con, "cell_grid", data.frame(ncol = 3103L))
  expect_equal(cell_grid_ncol(con), 3103L)
})
