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
