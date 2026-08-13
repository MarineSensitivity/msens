# The grid a serving database tiles on must be RECORDED, not guessed.
#
# Regression origin: cell_grid_ncol() fell back to global05's 7200 whenever the database
# held no `cell_grid` table — and no release ever wrote one. cell_model is partitioned by a
# tile id derived from that width, so on every usa05 release (v1-v7) the pruning filter
# named a different, perfectly valid tile and the clicked-cell species list returned EMPTY.
# A v7 cell holding 477 models reported 0 species; v8 passed only because 7200 was right.

mem_con <- function() DBI::dbConnect(duckdb::duckdb())

test_that("cell_grid_write records the grid and cell_grid_ncol reads it back", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_equal(cell_grid_ncol(con), 7200L)          # no table yet -> the old guess
  cell_grid_write(con, "usa05")
  expect_equal(cell_grid_ncol(con), 3103L)          # now the truth
  expect_equal(DBI::dbGetQuery(con, "SELECT grid_id FROM cell_grid")$grid_id, "usa05")

  cell_grid_write(con, "global05")                  # overwrite, not append
  expect_equal(cell_grid_ncol(con), 7200L)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM cell_grid")), 1L)
})

# build a cell_model whose stored tile ids were computed with `ncol_stored`
fake_cell_model <- function(con, cells, ncol_stored) {
  tiles <- vapply(cells, function(x) as.integer(cell_model_tiles(x, ncol = ncol_stored)[1]), integer(1))
  DBI::dbWriteTable(con, "cell_model", data.frame(
    cell_id = as.integer(cells), mdl_seq = 1L, val = 50, tile = tiles), overwrite = TRUE)
}

test_that("tile check passes when the recorded width matches the stored tiles", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  fake_cell_model(con, c(2493507L, 100000L, 5000001L), ncol_stored = 3103L)
  cell_grid_write(con, "usa05")
  expect_true(cell_model_tile_check(con))
})

test_that("tile check FAILS on the exact v7 mismatch, naming both tile ids", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # tiles stored on the usa05 grid, but the database claims global05 (the shipped state)
  fake_cell_model(con, c(2493507L, 100000L, 5000001L), ncol_stored = 3103L)
  cell_grid_write(con, "global05")

  expect_error(cell_model_tile_check(con), "do not match the grid width")
  expect_error(cell_model_tile_check(con), "ncol = 7200")
  expect_error(cell_model_tile_check(con), "silently return NO rows")
  # the message must carry the concrete disagreement, not just an assertion failure
  # (which cell it names depends on scan order, so match the shape, not the id)
  msg <- tryCatch(cell_model_tile_check(con), error = conditionMessage)
  expect_match(msg, "cell_id \\d+ is stored in tile \\d+ but computes to tile \\d+")
  expect_match(msg, "3 of 3 sampled cells disagree")
})

test_that("an unwritten cell_grid is caught too — the state every release shipped in", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  fake_cell_model(con, c(2493507L, 100000L), ncol_stored = 3103L)   # usa05 data
  expect_error(cell_model_tile_check(con), "do not match the grid width")   # no cell_grid -> 7200
})

test_that("a global05 database with no cell_grid still passes (v8's coincidence)", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  fake_cell_model(con, c(2493507L, 100000L), ncol_stored = 7200L)
  expect_true(cell_model_tile_check(con))
})

test_that("the check is a no-op when there is no cell_model at all", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_true(cell_model_tile_check(con))
})

test_that("an empty cell_model is an error, not a pass", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "cell_model", data.frame(
    cell_id = integer(), mdl_seq = integer(), val = double(), tile = integer()))
  expect_error(cell_model_tile_check(con), "empty")
})

test_that("usa05 and global05 give DIFFERENT tiles for the same cell — the whole hazard", {
  expect_false(identical(cell_model_tiles(2493507L, ncol = 3103L),
                         cell_model_tiles(2493507L, ncol = 7200L)))
})
