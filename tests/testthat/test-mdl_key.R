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

# assign_mdl_id() — mdl_id is the SERVE PARTITION key, so it must never renumber
# a key that is already published (see the roxygen note: ingesting gm+nc moved
# 45,499 of v8's 80,261 ids, which would have made titiler serve the wrong species)
test_that("assign_mdl_id() is dense_rank when nothing is published yet", {
  expect_equal(assign_mdl_id(c("b", "a", "c")), c(2L, 1L, 3L))
  expect_equal(assign_mdl_id(c("a", "a", "b")), c(1L, 1L, 2L))
  expect_equal(assign_mdl_id(character(0)), integer(0))
  # explicit empty registry behaves like none
  expect_equal(assign_mdl_id(c("b", "a"), data.frame(mdl_key = character(0), mdl_id = integer(0))),
               c(2L, 1L))
})

test_that("assign_mdl_id() preserves every published id and appends new ones above the max", {
  pub <- data.frame(mdl_key = c("am|x", "bl|y", "rng_iucn|z"), mdl_id = c(1L, 2L, 3L))
  # a new dataset sorting BETWEEN the published ones is exactly the gm/nc case
  keys <- c("am|x", "bl|y", "gm|new", "rng_iucn|z")
  got  <- assign_mdl_id(keys, pub)
  expect_equal(got[match(pub$mdl_key, keys)], pub$mdl_id)   # published ids untouched
  expect_equal(got[keys == "gm|new"], 4L)                   # new key above the max
  expect_equal(anyDuplicated(got), 0L)
})

test_that("assign_mdl_id() keeps published ids even when they are sparse or unsorted", {
  pub <- data.frame(mdl_key = c("c", "a"), mdl_id = c(7L, 3L))
  got <- assign_mdl_id(c("a", "b", "c", "d"), pub)
  expect_equal(got[1], 3L)
  expect_equal(got[3], 7L)
  expect_equal(sort(got[c(2, 4)]), c(8L, 9L))   # "b", "d" appended above max(7)
})

test_that("assign_mdl_id() rejects a registry that cannot be a partition key", {
  expect_error(assign_mdl_id("a", data.frame(mdl_key = "a")), "mdl_id")
  expect_error(assign_mdl_id("a", data.frame(mdl_key = c("a", "b"), mdl_id = c(1L, 1L))),
               "duplicate")
})

test_that("assign_mdl_id() is a no-op on the PUBLISHED v8 registry, and appends safely", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  base <- atlas_base_url()
  d <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb())
    out <- DBI::dbGetQuery(con, sprintf(
      "SELECT mdl_key, mdl_id FROM read_parquet('%s/v8/tables/model.parquet')", base))
    DBI::dbDisconnect(con, shutdown = TRUE)
    out
  }, error = function(e) NULL)
  skip_if(is.null(d) || !nrow(d), "v8 model registry unreachable")

  # THE invariant: re-deriving ids from the published registry changes nothing.
  # (Note this is deliberately NOT "dense_rank reproduces the published ids".
  # That held while v8's registry was one shot, and stopped holding the moment
  # gm + nc were appended -- which is the whole point: a set-derived id is not
  # stable under adding models, so the published registry is now the input.)
  expect_equal(assign_mdl_id(d$mdl_key, d), as.integer(d$mdl_id))

  # and a new key -- the next dataset ingested -- moves nothing that is published
  keys <- c(d$mdl_key, "zz_new|1")
  got  <- assign_mdl_id(keys, d)
  expect_equal(got[seq_len(nrow(d))], as.integer(d$mdl_id))
  expect_equal(got[length(keys)], max(as.integer(d$mdl_id)) + 1L)
  expect_equal(anyDuplicated(got), 0L)
})

# dataset_is_scored() — "registered" is not "used". v8 ingests gm + nc but excludes
# them from the merge, so they contribute to no score and must not be documented as inputs.
test_that("dataset_is_scored() marks only datasets that fed a merged taxon", {
  edges <- c("am", "am", "bl", "rng_iucn")
  expect_equal(dataset_is_scored(c("ms_merge", "am", "bl", "rng_iucn", "gm", "nc"), edges),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  # ms_merge is scored by construction even with no edges pointing at it
  expect_true(dataset_is_scored("ms_merge", character(0)))
  expect_equal(dataset_is_scored(character(0), edges), logical(0))
})

test_that("dataset_is_scored() assumes every dataset is scored when the relation is absent", {
  # v1/v2-era releases without taxon_model give no way to tell; claiming FALSE would
  # silently drop real inputs from the docs, which is worse than claiming all of them
  expect_equal(dataset_is_scored(c("am", "bl"), NULL), c(TRUE, TRUE))
})

test_that("assign_mdl_id ignores a keyless published row and refuses NA keys", {
  pub <- data.frame(mdl_key = c("a", "c", NA), mdl_id = c(1L, 2L, NA), stringsAsFactors = FALSE)
  expect_warning(ids <- assign_mdl_id(c("b", "a", "c"), pub), "NA mdl_key")
  expect_equal(ids, c(3L, 1L, 2L))            # published kept, new key appended above the real max
  expect_error(assign_mdl_id(c("a", NA)), "NA/empty")
})
