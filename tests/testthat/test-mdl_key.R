test_that("mdl_key_raw composes native per-dataset keys", {
  expect_equal(mdl_key_raw("am", "Fis-29291"), "am|Fis-29291")
  expect_equal(mdl_key_raw("gm", 1234, "01"), "gm|1234|01")
  expect_equal(mdl_key_raw("nc", "kelp-guild", "summer"), "nc|kelp-guild|summer")
  # vectorised over sp_id
  expect_equal(mdl_key_raw("am", c("A", "B")), c("am|A", "am|B"))
  # dataset_key must be a scalar without the separator
  expect_error(mdl_key_raw(c("am", "gm"), "x"))
  expect_error(mdl_key_raw("a|b", "x"))
})

test_that("mdl_key_merged composes taxadb-prefixed keys", {
  expect_equal(mdl_key_merged("WORMS", 137209), "ms_merge|WORMS:137209")
  expect_equal(mdl_key_merged("botw", 22694927), "ms_merge|BOTW:22694927")  # case-insensitive
  expect_equal(mdl_key_merged("WORMS", c(1, 2)), c("ms_merge|WORMS:1", "ms_merge|WORMS:2"))
  expect_error(mdl_key_merged("FOO", 1))   # unknown authority
})

test_that("mdl_key_parse splits into components", {
  d <- mdl_key_parse(c("am|Fis-29291", "gm|1234|01", "ms_merge|WORMS:137209"))
  expect_equal(d$dataset_key, c("am", "gm", "ms_merge"))
  expect_equal(d$sp_id, c("Fis-29291", "1234", "WORMS:137209"))
  expect_equal(d$interval, c(NA, "01", NA))
  expect_equal(d$taxon_authority, c(NA, NA, "WORMS"))
  expect_equal(d$taxon_id, c(NA, NA, "137209"))
})

test_that("mdl_key_parse round-trips the composers", {
  expect_equal(mdl_key_parse(mdl_key_raw("am", "Fis-29291"))$sp_id, "Fis-29291")
  expect_equal(mdl_key_parse(mdl_key_raw("gm", "1234", "01"))$interval, "01")
  p <- mdl_key_parse(mdl_key_merged("BOTW", 22694927))
  expect_equal(p$taxon_authority, "BOTW")
  expect_equal(p$taxon_id, "22694927")
})

test_that("mdl_key_raw is vectorised, like mdl_key_merged", {
  # a backfill mints tens of thousands of keys at once; the scalar-only form
  # failed inside mutate() with an opaque stopifnot
  expect_equal(mdl_key_raw("am", c("Fis-1", "Fis-2")), c("am|Fis-1", "am|Fis-2"))
  expect_equal(mdl_key_raw(c("am", "bl"), c("Fis-1", "2269")), c("am|Fis-1", "bl|2269"))
  expect_equal(mdl_key_raw("am", "Fis-1"), "am|Fis-1")            # scalar unchanged
  expect_error(mdl_key_raw(c("a|b", "c"), c("1", "2")), "separator|grepl")
  expect_error(mdl_key_raw(c("a", "b", "c"), c("1", "2")))        # length mismatch
})

test_that("normalize_ds_key maps the legacy AquaMaps spelling and nothing else", {
  # v1-v7 spell it am_0.05; minting from the raw string gives am_0.05|Fis-29291,
  # which matches nothing in v8 -- a crosswalk built without this silently fails
  # to join the two generations
  expect_equal(normalize_ds_key("am_0.05"), "am")
  expect_equal(normalize_ds_key(c("am_0.05", "bl", "rng_iucn", "ms_merge")),
               c("am", "bl", "rng_iucn", "ms_merge"))
  # not a loose prefix match: only the exact legacy key is rewritten
  expect_equal(normalize_ds_key(c("am", "am_0.05_x", "xam_0.05")),
               c("am", "am_0.05_x", "xam_0.05"))
  expect_equal(normalize_ds_key(character()), character())
})

test_that("a legacy key normalises into a mdl_key that matches the v8 grammar", {
  expect_equal(mdl_key_raw(normalize_ds_key("am_0.05"), "Fis-29291"), "am|Fis-29291")
})

# Round-trip against REAL published rows -------------------------------------
# The composers are unit-tested above with synthetic input; this asserts the
# grammar still matches what the backfill actually wrote for historical
# releases. model_asset.parquet is public over HTTPS, so no server is needed.

test_that("mdl_key_raw round-trips on real v1/v3/v7 model_asset rows", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  base <- atlas_base_url()
  for (v in c("v1", "v3", "v7")) {
    d <- tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb())
      # disconnect explicitly, not via on.exit: inside a loop on.exit registers
      # once per iteration against the LAST `con`, so it closes one handle three
      # times and warns "Connection already closed"
      on.exit(NULL)
      out <- DBI::dbGetQuery(con, sprintf(
        "SELECT mdl_key, ds_key, sp_id FROM read_parquet('%s/%s/tables/model_asset.parquet')
         WHERE ds_key <> 'ms_merge' AND sp_id IS NOT NULL LIMIT 200", base, v))
      DBI::dbDisconnect(con, shutdown = TRUE)
      out
    }, error = function(e) NULL)
    skip_if(is.null(d) || !nrow(d), paste("model_asset unreachable for", v))

    # what the backfill stored must be exactly what the composer mints today
    expect_equal(mdl_key_raw(normalize_ds_key(d$ds_key), d$sp_id), d$mdl_key,
                 info = paste(v, "-", nrow(d), "rows"))
    # and no legacy spelling survived into a published key
    expect_false(any(grepl("^am_0\\.05\\|", d$mdl_key)), info = v)
  }
})

test_that("a v6 mdl_seq and the v7 mdl_key name the same species", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  base <- atlas_base_url()
  d <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb())
    out <- DBI::dbGetQuery(con, sprintf(
      "SELECT a.mdl_key, a.mdl_seq AS seq6, b.mdl_seq AS seq7
         FROM read_parquet('%s/v6/tables/model_asset.parquet') a
         JOIN read_parquet('%s/v7/tables/model_asset.parquet') b USING (mdl_key)
        WHERE a.ds_key <> 'ms_merge' LIMIT 500", base, base))
    DBI::dbDisconnect(con, shutdown = TRUE)
    out
  }, error = function(e) NULL)
  skip_if(is.null(d) || !nrow(d), "model_asset unreachable for v6/v7")

  # mdl_key is the STABLE identifier across releases; mdl_seq is not, and that
  # instability is why deep links had to move off it
  expect_gt(nrow(d), 100)
  expect_true(all(nzchar(d$mdl_key)))
})
