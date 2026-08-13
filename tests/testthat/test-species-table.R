# Species-table rules across the v7 -> v8 schema rename.
#
# REGRESSION: the v8 rewrite renamed is_ok -> is_valid_usa, mdl_seq ->
# ms_merge_key/mdl_key and value -> val, and dropped the precomputed zone_taxon
# table entirely. The app's copy of this query was never migrated, so the v8
# "Table of Species" tab came up empty ("Can't select columns that don't exist")
# and its CSV download could never produce a file. One synthetic database per
# schema, with the SAME numbers, so both must agree.

# build a tiny in-memory sdm.duckdb in either schema
fixture_db <- function(schema = c("v7", "v8")) {
  schema <- match.arg(schema)
  con <- DBI::dbConnect(duckdb::duckdb())

  # two cells: cell 1 fully in the zone, cell 2 only half covered
  DBI::dbWriteTable(con, "cell", data.frame(cell_id = c(1L, 2L), area_km2 = c(100, 200)))
  DBI::dbWriteTable(con, "zone", data.frame(
    zone_seq = 1L, tbl = "z", fld = "programarea_key", val = "AAA"))
  DBI::dbWriteTable(con, "zone_cell", data.frame(
    zone_seq = c(1L, 1L), cell_id = c(1L, 2L), pct_covered = c(100, 50)))

  # one scored species (mammal) + one that must be excluded by the validity flag
  taxon <- data.frame(
    taxon_id        = c(1L, 2L),
    taxon_authority = c("worms", "worms"),
    scientific_name = c("Aaa aaa", "Bbb bbb"),
    common_name     = c("alpha", "beta"),
    sp_cat          = c("mammal", "mammal"),
    extrisk_code    = c("EN", "LC"),
    er_score        = c(50, 20),      # stored 0-100, returned /100
    is_mmpa         = c(TRUE, FALSE),
    is_mbta         = c(FALSE, FALSE))
  mc <- data.frame(cell_id = c(1L, 2L, 1L), val = c(80, 40, 10))

  if (schema == "v7") {
    taxon$is_ok   <- c(TRUE, FALSE)   # v7 baked the marine/category cull into is_ok
    taxon$mdl_seq <- c(10L, 20L)
    mc$mdl_seq    <- c(10L, 10L, 20L)
    names(mc)[names(mc) == "val"] <- "value"
  } else {
    taxon$is_valid_usa <- c(TRUE, FALSE)
    taxon$is_marine    <- c(TRUE, TRUE)   # v8 splits eligibility out of validity
    taxon$ms_merge_key <- c("ms_merge|WORMS:1", "ms_merge|WORMS:2")
    mc$mdl_key         <- c("ms_merge|WORMS:1", "ms_merge|WORMS:1", "ms_merge|WORMS:2")
  }
  DBI::dbWriteTable(con, "taxon", taxon)
  DBI::dbWriteTable(con, "model_cell", mc)
  con
}

# expected for the valid species across the whole zone:
#   area  = 100*100/100 + 200*50/100                = 200
#   suit  = (80*100 + 40*50) / (100+50) / 100       = 0.6666667
#   er    = 50/100                                   = 0.5
expected_area <- 200
expected_suit <- (80 * 100 + 40 * 50) / 150 / 100

test_that("species_for_zone works on the v8 schema", {
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- species_for_zone(con, "programarea_key", "AAA")

  expect_equal(nrow(d), 1L)                        # invalid taxon excluded
  expect_equal(d$sp_scientific, "Aaa aaa")
  expect_equal(d$mdl_key, "ms_merge|WORMS:1")      # v8 key, as character
  expect_equal(d$area_km2, expected_area)
  expect_equal(d$avg_suit, expected_suit)
  expect_equal(d$er_score, 0.5)                    # /100 applied
  expect_equal(d$er_code, "EN")
  expect_true(d$is_mmpa)
})

test_that("species_for_zone works on the v7 schema and agrees with v8", {
  c7 <- fixture_db("v7"); on.exit(DBI::dbDisconnect(c7, shutdown = TRUE), add = TRUE)
  c8 <- fixture_db("v8"); on.exit(DBI::dbDisconnect(c8, shutdown = TRUE), add = TRUE)
  d7 <- species_for_zone(c7, "programarea_key", "AAA")
  d8 <- species_for_zone(c8, "programarea_key", "AAA")

  expect_equal(d7$mdl_key, "10")                   # v7 mdl_seq, as character
  # every value except the schema-specific model id must match exactly
  num <- c("area_km2", "avg_suit", "er_score", "suit_er", "suit_er_area", "pct_cat")
  expect_equal(d7[num], d8[num])
  expect_equal(d7$sp_scientific, d8$sp_scientific)
})

test_that("species_for_cells weights by pct_covered on both schemas", {
  cells <- data.frame(cell_id = c(1L, 2L), pct_covered = c(100, 50))
  for (s in c("v7", "v8")) {
    con <- fixture_db(s)
    d <- species_for_cells(con, cells)
    expect_equal(nrow(d), 1L, info = s)
    expect_equal(d$area_km2, expected_area, info = s)
    expect_equal(d$avg_suit, expected_suit, info = s)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("a single fully-covered cell needs no weighting correction", {
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- species_for_cells(con, data.frame(cell_id = 1L, pct_covered = 100))
  expect_equal(d$area_km2, 100)
  expect_equal(d$avg_suit, 0.8)     # the single cell's val/100
})

test_that("pct_cat sums to 1 within a category", {
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # make the second taxon valid too, so the category has two contributors
  DBI::dbExecute(con, "UPDATE taxon SET is_valid_usa = TRUE WHERE taxon_id = 2")
  d <- species_for_zone(con, "programarea_key", "AAA")
  expect_equal(nrow(d), 2L)
  expect_equal(sum(d$pct_cat), 1)
})

test_that("species_for_cells requires pct_covered", {
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_error(species_for_cells(con, data.frame(cell_id = 1L)))
})

test_that("an unresolvable schema errors loudly rather than returning nothing", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "taxon", data.frame(taxon_id = 1L))          # no validity/model id
  DBI::dbWriteTable(con, "model_cell", data.frame(cell_id = 1L))
  expect_error(species_for_zone(con, "programarea_key", "AAA"), "cannot resolve")
})


test_that("scoring eligibility is enforced, not just cell presence", {
  # REGRESSION: v7 encoded the marine/category cull in `is_ok`, but v8's
  # `is_valid_usa` only means "has >=1 merged cell in US waters". Filtering on
  # validity alone put non-marine and excluded taxa in the table — the first row
  # of the real v8 study-area table was a cane toad (amphibian).
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # make BOTH taxa cell-valid, then disqualify the second two different ways
  DBI::dbExecute(con, "UPDATE taxon SET is_valid_usa = TRUE")

  DBI::dbExecute(con, "UPDATE taxon SET sp_cat = 'amphibian' WHERE taxon_id = 2")
  d <- species_for_zone(con, "programarea_key", "AAA")
  expect_equal(nrow(d), 1L)
  expect_false("amphibian" %in% d$sp_cat)

  DBI::dbExecute(con, "UPDATE taxon SET sp_cat = 'reptile' WHERE taxon_id = 2")
  expect_false("reptile" %in% species_for_zone(con, "programarea_key", "AAA")$sp_cat)

  # non-marine is excluded even when the category is scoreable
  DBI::dbExecute(con, "UPDATE taxon SET sp_cat = 'mammal', is_marine = FALSE WHERE taxon_id = 2")
  d3 <- species_for_zone(con, "programarea_key", "AAA")
  expect_equal(nrow(d3), 1L)
  expect_equal(d3$taxon_id, 1L)
})

test_that("v7 keeps relying on is_ok (no is_marine column present)", {
  con <- fixture_db("v7"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_false("is_marine" %in% DBI::dbListFields(con, "taxon"))
  DBI::dbExecute(con, "UPDATE taxon SET is_ok = TRUE")     # both valid under v7 rules
  expect_equal(nrow(species_for_zone(con, "programarea_key", "AAA")), 2L)
})

test_that("build_zone_taxon precomputes every zone", {
  # REGRESSION: v8 dropped v7's zone_taxon on the assumption the app could
  # aggregate live. It cannot on the server, which holds only serve.duckdb whose
  # model_cell is an S3 view partitioned by mdl_id for point reads — a zone-wide
  # scan there fails with an S3 IO error. So this table must exist.
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "INSERT INTO zone VALUES (2, 'z', 'subregion_key', 'USA')")
  DBI::dbExecute(con, "INSERT INTO zone_cell VALUES (2, 1, 100)")

  n <- build_zone_taxon(con)
  expect_true("zone_taxon" %in% DBI::dbListTables(con))
  zt <- DBI::dbReadTable(con, "zone_taxon")
  expect_equal(nrow(zt), n)
  expect_true(all(c("zone_fld", "zone_value") %in% names(zt)))
  expect_setequal(unique(zt$zone_fld), c("programarea_key", "subregion_key"))

  # each zone's rows must match computing that zone directly
  d_pra <- species_for_zone(con, "programarea_key", "AAA")
  zt_pra <- zt[zt$zone_value == "AAA", ]
  expect_equal(nrow(zt_pra), nrow(d_pra))
  expect_equal(zt_pra$area_km2, d_pra$area_km2)

  # re-running replaces rather than appends
  build_zone_taxon(con)
  expect_equal(nrow(DBI::dbReadTable(con, "zone_taxon")), n)
})

test_that("species_for_zone prefers the precomputed zone_taxon when present", {
  # On the server the live aggregation is impossible (S3 model_cell partitioned
  # by mdl_id), so the precomputed table must take precedence when it exists.
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  live <- species_for_zone(con, "programarea_key", "AAA")   # no zone_taxon yet

  build_zone_taxon(con)
  pre <- species_for_zone(con, "programarea_key", "AAA")    # now reads the table
  expect_equal(pre$sp_scientific, live$sp_scientific)
  expect_equal(pre$area_km2, live$area_km2)
  expect_equal(pre$avg_suit, live$avg_suit)

  # proof it is READING the table, not recomputing: doctor a row and see it back
  DBI::dbExecute(con, "UPDATE zone_taxon SET area_km2 = 12345 WHERE zone_value = 'AAA'")
  expect_equal(species_for_zone(con, "programarea_key", "AAA")$area_km2, 12345)

  # ...and use_precomputed = FALSE goes back to the live aggregation
  expect_equal(species_for_zone(con, "programarea_key", "AAA",
                                use_precomputed = FALSE)$area_km2, expected_area)
})

test_that("build_zone_taxon rebuilds from the cells, not from its own last output", {
  # REGRESSION: species_for_zone() prefers an existing zone_taxon, and
  # build_zone_taxon() drops the old table only AFTER computing every zone — so a
  # re-run read the stale rows back and wrote them out again. Re-running
  # score_zone_metrics.qmd after a scoring change would have reported success
  # while publishing the PREVIOUS release's species table.
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  build_zone_taxon(con)
  DBI::dbExecute(con, "UPDATE zone_taxon SET area_km2 = 12345, avg_suit = 0.999")

  build_zone_taxon(con)                                    # must recompute
  zt <- DBI::dbReadTable(con, "zone_taxon")
  expect_equal(zt$area_km2, expected_area)
  expect_equal(zt$avg_suit, expected_suit)
})

# ---- reading a PUBLISHED zone_taxon of each vintage --------------------------
#
# REGRESSION (apps#7): the precomputed branch returned the stored columns verbatim, so
# species_for_zone() answered in whatever shape the open release happened to use. The v7
# "Table of Species" tab died server-side on `Can't select columns that don't exist.
# x Column er_code doesn't exist`, because v1-v7 name it `rl_code` and the model id
# `mdl_seq`. Every published vintage must come back in the SAME canonical shape.

# a zone_taxon exactly as each generation released it, with the same underlying numbers:
#   area 200 km2, avg_suit 2/3, extinction risk 50% -> the v8 expectations above
zone_taxon_published <- function(vintage = c("v1", "v3", "v8")) {
  vintage <- match.arg(vintage)
  base <- data.frame(
    zone_fld = "programarea_key", zone_value = "AAA",
    sp_cat = "mammal", sp_common = "alpha", sp_scientific = "Aaa aaa",
    taxon_id = 1L, taxon_authority = "worms",
    area_km2 = expected_area, avg_suit = expected_suit)
  shares <- function(d, er) data.frame(d,
    suit_rl = d$avg_suit * er, suit_rl_area = d$avg_suit * er * d$area_km2,
    cat_suit_rl_area = d$avg_suit * er * d$area_km2, pct_cat = 1)
  switch(vintage,
    # v1/v2: the redlist score, already a fraction; no MMPA/MBTA flags at all
    v1 = shares(data.frame(base, mdl_seq = 10L, rl_code = "EN", rl_score = 0.5), 0.5),
    # v3-v7: er_score on the RAW 1-100 scale, still keyed on rl_code/mdl_seq
    v3 = shares(data.frame(base, mdl_seq = 10L, rl_code = "EN", er_score = 50,
                           is_mmpa = TRUE, is_mbta = FALSE), 0.5),
    # v8: the canonical names, er_score already divided by 100
    v8 = data.frame(base, mdl_key = "ms_merge|WORMS:1", er_code = "EN", er_score = 0.5,
                    is_mmpa = TRUE, is_mbta = FALSE,
                    suit_er = 0.5 * expected_suit,
                    suit_er_area = 0.5 * expected_suit * expected_area,
                    cat_suit_er_area = 0.5 * expected_suit * expected_area, pct_cat = 1))
}

# a connection holding ONLY that published table (which is all serve.duckdb has for the
# zone question — its model_cell is an S3 view that cannot answer a zone-wide scan)
published_db <- function(vintage) {
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbWriteTable(con, "zone_taxon", zone_taxon_published(vintage))
  con
}

test_that("a published zone_taxon reads back in the canonical shape, every vintage", {
  canonical <- c("sp_cat", "sp_common", "sp_scientific", "taxon_id", "taxon_authority",
                 "er_code", "er_score", "is_mmpa", "is_mbta", "mdl_key",
                 "area_km2", "avg_suit", "suit_er", "suit_er_area",
                 "cat_suit_er_area", "pct_cat")
  # the id itself stays each release's own — v1-v7 renumber a mdl_seq, v8 keys on a
  # stable string — but it always arrives as character, under the one name
  want_id <- c(v1 = "10", v3 = "10", v8 = "ms_merge|WORMS:1")
  for (v in c("v1", "v3", "v8")) {
    con <- published_db(v)
    d <- species_for_zone(con, "programarea_key", "AAA")

    expect_equal(names(d), canonical, info = v)      # the app selects exactly these
    expect_equal(nrow(d), 1L, info = v)
    expect_equal(d$mdl_key, unname(want_id[v]), info = v)
    expect_equal(d$er_code, "EN", info = v)          # v1-v7 rl_code renamed
    expect_equal(d$er_score, 0.5, info = v)          # 1-100 scale divided; fractions left be
    expect_equal(d$area_km2, expected_area, info = v)
    expect_equal(d$avg_suit, expected_suit, info = v)
    expect_equal(d$suit_er_area, 0.5 * expected_suit * expected_area, info = v)
    expect_equal(d$pct_cat, 1, info = v)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("the v8 mdl_key survives normalisation unchanged", {
  con <- published_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(species_for_zone(con, "programarea_key", "AAA")$mdl_key, "ms_merge|WORMS:1")
})

test_that("v1/v2 have no MMPA/MBTA flags, and say so rather than claiming FALSE", {
  # a missing protection is unknown, not absent: rendering it as FALSE would assert
  # that a marine mammal is NOT MMPA-protected in releases that never recorded it
  con <- published_db("v1"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- species_for_zone(con, "programarea_key", "AAA")
  expect_true(is.na(d$is_mmpa))
  expect_true(is.na(d$is_mbta))
  expect_type(d$is_mmpa, "logical")
})

test_that("an er_score that is not a fraction fails loudly", {
  # the scale is decided by the SCHEMA (rl_code marks the 1-100 vintages). If a future
  # release breaks that rule, the app would silently render 5000% -- so assert instead.
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  zt <- zone_taxon_published("v8"); zt$er_score <- 50      # canonical names, raw scale
  DBI::dbWriteTable(con, "zone_taxon", zt)
  expect_error(species_for_zone(con, "programarea_key", "AAA"), "fraction")
})

test_that("a zone_taxon with no model id column errors rather than dropping it", {
  con <- DBI::dbConnect(duckdb::duckdb()); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  zt <- zone_taxon_published("v8"); zt$mdl_key <- NULL
  DBI::dbWriteTable(con, "zone_taxon", zt)
  expect_error(species_for_zone(con, "programarea_key", "AAA"), "model id")
})

test_that("species_for_cells uses cell_model when present, with identical results", {
  # model_cell is partitioned by mdl_id, so a per-cell query scans everything;
  # cell_model holds the same rows partitioned by spatial tile. Both must give
  # the same answer — and the cell_model path joins mdl_id back through `model`,
  # which is where a wrong join would silently return zero rows.
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cells <- data.frame(cell_id = c(1L, 2L), pct_covered = c(100, 50))
  want  <- species_for_cells(con, cells)          # via model_cell

  # add the cell-oriented twin: same rows, mdl_id instead of mdl_key
  DBI::dbExecute(con, "CREATE TABLE model AS SELECT 1 AS mdl_id, 'ms_merge|WORMS:1' AS mdl_key
                       UNION ALL SELECT 2, 'ms_merge|WORMS:2'")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE cell_model AS SELECT ", cell_model_tile_sql("mc.cell_id"), " AS tile, ",
    "mo.mdl_id, mc.cell_id, mc.val FROM model_cell mc JOIN model mo USING (mdl_key)"))

  got <- species_for_cells(con, cells)            # via cell_model
  expect_equal(got$sp_scientific, want$sp_scientific)
  expect_equal(got$area_km2,      want$area_km2)
  expect_equal(got$avg_suit,      want$avg_suit)
  expect_equal(got$mdl_key,       want$mdl_key)
  expect_gt(nrow(got), 0)                          # guards a silently-empty join
})

test_that("model_cell is never touched when cell_model is available", {
  # REGRESSION (production): .sdm_cols() read model_cell's schema before
  # choosing a source. On the server that alone makes DuckDB LIST the S3 prefix
  # and fail —
  #   IO Error: SSL peer certificate ... HTTP GET .../serve/model_cell/
  # — so the clicked cell still broke even after cell_model existed. Here
  # model_cell is removed outright here, so any access at all errors.
  con <- fixture_db("v8"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cells <- data.frame(cell_id = c(1L, 2L), pct_covered = c(100, 50))

  DBI::dbExecute(con, "CREATE TABLE model AS SELECT 1 AS mdl_id, 'ms_merge|WORMS:1' AS mdl_key
                       UNION ALL SELECT 2, 'ms_merge|WORMS:2'")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE cell_model AS SELECT ", cell_model_tile_sql("mc.cell_id"), " AS tile, ",
    "mo.mdl_id, mc.cell_id, mc.val FROM model_cell mc JOIN model mo USING (mdl_key)"))

  # remove model_cell entirely: any reference to it — including reading its
  # schema — now errors, so this passes only if the code truly never touches it
  DBI::dbExecute(con, "DROP TABLE model_cell")
  expect_error(DBI::dbListFields(con, "model_cell"))   # confirms the trap

  d <- species_for_cells(con, cells)
  expect_equal(nrow(d), 1L)
  expect_equal(d$mdl_key, "ms_merge|WORMS:1")
})
