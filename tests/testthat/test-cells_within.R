# Guards cells_within_sql() / cell_id_of() (v11 Q1/Q2: the buffered-record range dataset and the
# zone dilation of the occurrence rule). The SQL's windowed search is checked against a brute-force
# haversine scan of EVERY cell on a coarse regular global grid (36 x 18 cells of 10 degrees, so the
# scan is 648 cells), at the places that break naive implementations: both sides of the
# antimeridian, the equator, a high latitude where the longitude reach widens, and both poles.

skip_if_not_installed("duckdb")

NC <- 36L; NR <- 18L; DEG <- 10
R_KM <- 6371.0088

brute <- function(con, cell, d_km) {
  DBI::dbGetQuery(con, glue::glue("
    WITH a AS (SELECT 90 - (({cell} - 1) // {NC} + 0.5) * {DEG} AS lat0, -180 + (({cell} - 1) % {NC} + 0.5) * {DEG} AS lon0),
    g AS (SELECT CAST(i + 1 AS INTEGER) AS cell_id, 90 - (i // {NC} + 0.5) * {DEG} AS lat1, -180 + (i % {NC} + 0.5) * {DEG} AS lon1
          FROM range(0, {NC * NR}) t(i))
    SELECT g.cell_id FROM g, a
    WHERE 2 * {R_KM} * asin(sqrt(least(1, pow(sin(radians(lat1 - lat0) / 2), 2) +
          cos(radians(lat0)) * cos(radians(lat1)) * pow(sin(radians(lon1 - lon0) / 2), 2)))) <= {d_km}
    ORDER BY g.cell_id"))$cell_id
}

test_that("cell_id_of: corners, the antimeridian edge and the poles", {
  expect_equal(cell_id_of(-180, 90, NC, NR, DEG), 1L)
  expect_equal(cell_id_of(179.9, 90, NC, NR, DEG), NC)
  expect_equal(cell_id_of(179.9, -89.9, NC, NR, DEG), NC * NR)
  expect_equal(cell_id_of(180, -90, NC, NR, DEG), NC * NR)       # the far edges clamp into the last column / row
  expect_equal(cell_id_of(-175, 85, NC, NR, DEG), 1L)
  expect_equal(cell_id_of(-165, 85, NC, NR, DEG), 2L)
  expect_equal(cell_id_of(0, 0, 7200L, 3600L, 0.05), 1800L * 7200L + 3600L + 1L)   # global05
})

test_that("cells_within_sql matches a brute-force scan of every cell at the hard places", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cases <- list(
    list(lon = 175, lat = 5,   d = 1200),   # east of the antimeridian: must reach -175
    list(lon = -175, lat = 5,  d = 1200),   # west of it
    list(lon = 5,  lat = 5,    d = 1500),   # equator
    list(lon = 5,  lat = 75,   d = 1500),   # high latitude: the longitude reach widens
    list(lon = 5,  lat = 85,   d = 1500),   # the cap holds the pole: every column
    list(lon = 5,  lat = -85,  d = 1500),   # the south pole, rows clipped
    list(lon = 5,  lat = 5,    d = 1))      # tiny: the cell itself only
  for (k in cases) {
    cell <- cell_id_of(k$lon, k$lat, NC, NR, DEG)
    DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TABLE src AS SELECT {cell} AS cell_id"))
    got <- sort(DBI::dbGetQuery(con, cells_within_sql("src", k$d, nc = NC, nr = NR, deg = DEG))$cell_id)
    expect_equal(got, brute(con, cell, k$d), info = paste(k$lon, k$lat, k$d))
    expect_true(cell %in% got)
  }
})

test_that("grouping carries through and the union of two sources is what each gives", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  c1 <- cell_id_of(175, 5, NC, NR, DEG); c2 <- cell_id_of(5, 5, NC, NR, DEG)
  DBI::dbWriteTable(con, "src", data.frame(key = c("a", "a", "b"), cell_id = c(c1, c2, c2)))
  d <- DBI::dbGetQuery(con, cells_within_sql("src", 1200, by = "key", nc = NC, nr = NR, deg = DEG))
  a <- sort(d$cell_id[d$key == "a"]); b <- sort(d$cell_id[d$key == "b"])
  expect_equal(a, sort(unique(c(brute(con, c1, 1200), brute(con, c2, 1200)))))
  expect_equal(b, brute(con, c2, 1200))
  expect_equal(anyDuplicated(paste(d$key, d$cell_id)), 0L)
})

test_that("bad arguments error", {
  expect_error(cells_within_sql("src", 0), "d_km")
  expect_error(cells_within_sql("src", 10, nc = 100L, nr = 18L, deg = 10), "regular global grid")
  expect_error(cells_within_sql("", 10))
})
