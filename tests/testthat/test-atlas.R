# atlas connection + views ----
# attach_atlas() must create the named views that the calc/score helpers reference by bare name
# (scores_for_pra -> tbl(con,"zone"|"zone_metric"|"metric"), species_for_cells -> "model_cell"|
# "taxon"|"cell"), so the whole read/score API composes off one connection. Regression guard for
# the gap where attach_atlas only set up httpfs/S3 and left those helpers unusable.

test_that("atlas_views errors without a configured base", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_error(atlas_views(con, base = NULL), "attach_atlas")
})

test_that("attach_atlas creates the composable view set + val->value alias (live release)", {
  skip_if_not_installed("duckdb")
  skip_if_offline("s3.us-east-1.amazonaws.com")

  con <- tryCatch(attach_atlas(anon = TRUE), error = function(e) skip(conditionMessage(e)))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  views <- DBI::dbGetQuery(con,
    "SELECT table_name FROM information_schema.tables WHERE table_schema = 'main'")$table_name
  expect_true(all(
    c("cell", "taxon", "dataset", "model", "metric", "cell_metric",
      "zone", "zone_cell", "zone_metric", "native_asset") %in% views))

  # scoring tables expose the `value` alias of `val` (apps/calc reference `value`)
  zm_cols <- DBI::dbGetQuery(con,
    "SELECT column_name FROM information_schema.columns WHERE table_name = 'zone_metric'")$column_name
  expect_true(all(c("val", "value") %in% zm_cols))
})

test_that("scores_for_pra composes off an attach_atlas connection (live release)", {
  skip_if_not_installed("duckdb")
  skip_if_offline("s3.us-east-1.amazonaws.com")

  con <- tryCatch(attach_atlas(anon = TRUE), error = function(e) skip(conditionMessage(e)))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # pick any Program Area key present in the release, then score it
  pk <- DBI::dbGetQuery(con,
    "SELECT value FROM zone WHERE fld = 'programarea_key' LIMIT 1")$value
  skip_if(length(pk) == 0, "no program-area zones in release")

  sc <- scores_for_pra(con, pk)
  expect_s3_class(sc, "data.frame")
  expect_true(all(c("component", "score", "even") %in% names(sc)))
  expect_gt(nrow(sc), 0)
  expect_false("all" %in% sc$component)          # the aggregate row is dropped
})
