# The `{ver}/app/` data contract: one schema for v1…v9, built from four very
# different databases. Every assertion here is a per-version adapter that does NOT
# have to be written again in TypeScript.

gens <- c("v9", "v7", "v7b", "v2")
BASE <- "https://example.invalid/marine-atlas"

build <- function(con, gen, ...) {
  m <- manifest_build(con, gen, base = BASE)
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "app")
  list(dir = d, manifest = m,
       out = app_bundle_build(con, gen, d, manifest = m, base = BASE, ...))
}

# ---- schema validation -------------------------------------------------------

test_that("every builder validates its own output against its schema", {
  for (gen in gens) with_synth(gen, function(con) {
    m <- manifest_build(con, gen, base = BASE)
    expect_silent(app_boot(con, gen, m, tables = list()))
    expect_silent(app_taxa(con, gen))
    expect_type(app_taxon_shards(con, gen), "list")
    expect_type(app_alias_shards(con, gen), "list")
  })
})

test_that("a deliberately malformed object FAILS schema validation", {
  with_synth("v9", function(con) {
    m <- manifest_build(con, "v9", base = BASE)
    good <- app_boot(con, "v9", m, tables = list())

    # 1. a required key removed
    bad <- good; bad$grid <- NULL
    expect_error(app_validate(bad, "boot"), "boot.schema.json")

    # 2. a key of the wrong TYPE (nc as a string is exactly what a JS build would do)
    bad <- good; bad$grid$nc <- "7200"
    expect_error(app_validate(bad, "boot"), "must be integer")

    # 3. an UNDECLARED key: a bundle must not grow keys nobody reads
    bad <- good; bad$surprise <- 1
    expect_error(app_validate(bad, "boot"), "additional propert")

    # 4. an out-of-range enum
    bad <- good; bad$release$access <- "semi-public"
    expect_error(app_validate(bad, "boot"), "boot.schema.json")

    # 5. a palette with the wrong number of stops: 10 stops silently rebins a legend
    bad <- good; bad$palettes$spectral_r <- as.list(head(unlist(good$palettes$spectral_r), 10))
    expect_error(app_validate(bad, "boot"), "boot.schema.json")

    # and the good one still passes, so the failures above are not vacuous
    expect_silent(app_validate(good, "boot"))
  })
})

test_that("every schema in inst/schema is loadable and named app_*", {
  expect_true(all(c("alias", "boot", "manifest", "table", "taxa", "taxon") %in%
                    app_schema_names()))
  for (n in app_schema_names()) expect_true(file.exists(app_schema_path(n)))
  expect_error(app_schema_path("nope"), "no schema")
})

# ---- the bundle, on each generation's shape ----------------------------------

test_that("the whole bundle builds on v2, v7, v7b and v9 alike", {
  for (gen in gens) with_synth(gen, function(con) {
    b <- build(con, gen)
    expect_true(file.exists(file.path(b$dir, "boot.json")))
    expect_true(file.exists(file.path(b$dir, "taxa.json")))
    expect_true(file.exists(file.path(b$dir, "taxon.parquet")))
    expect_equal(b$out$n_taxa, 3, info = gen)
    expect_gt(b$out$n_shards, 0)
    # every shard file is where boot.json says it is
    for (s in names(app_taxon_shards(con, gen)))
      expect_true(file.exists(file.path(b$dir, "taxon", paste0(s, ".json"))), info = gen)
  })
})

test_that("the normalized taxon table has ONE shape, v1/v2's missing ER columns included", {
  want <- c("key", "sci", "common", "sp_cat", "taxon_id", "taxon_authority",
            "rl", "er_code", "esa_source", "er_score", "is_mmpa", "is_mbta",
            "valid_usa", "valid_global")
  for (gen in gens) with_synth(gen, function(con) {
    d <- app_taxon_table(con)
    expect_setequal(names(d), want)
    expect_equal(nrow(d), 3, info = gen)
    if (gen == "v2") {
      # v1/v2 have no extrisk_code / er_score / MMPA / MBTA AT ALL: typed NULLs,
      # so the shape survives rather than the release failing outright
      expect_true(all(is.na(d$er_code)), info = gen)
      expect_true(all(is.na(d$er_score)), info = gen)
    } else {
      expect_false(any(is.na(d$er_score)), info = gen)
    }
    # v1-v7 have no is_valid_usa: it is TRUE by construction, never NA
    expect_false(any(is.na(d$valid_usa)), info = gen)
  })
})

test_that("taxa.json is the picker set, in parallel arrays with packed flags", {
  with_synth("v9", function(con) {
    t <- app_taxa(con, "v9")
    expect_equal(t$n, 3)
    expect_length(t$key, 3); expect_length(t$sci, 3); expect_length(t$flags, 3)
    expect_equal(length(t$cat_idx), t$n)
    # bit 1 = valid_usa, bit 2 = valid_global
    d <- app_taxon_table(con)
    expect_equal(unlist(t$flags),
                 as.integer(ifelse(d$valid_usa, 1L, 0L) + ifelse(d$valid_global, 2L, 0L)))
    expect_true(all(unlist(t$cat_idx) < length(t$cat)))
  })
})

test_that("reptiles and amphibians are never in the picker set", {
  with_synth("v9", function(con) {
    DBI::dbExecute(con, "INSERT INTO taxon (taxon_id, taxon_authority, ms_merge_key,
      scientific_name, common_name, sp_cat, is_marine, is_valid_usa, is_valid_global)
      VALUES ('999', 'worms', 'ms_merge|WORMS:999', 'Rhinella marina', 'Cane Toad',
              'amphibian', TRUE, TRUE, TRUE)")
    # the v8 run surfaced a cane toad as the first row of the study-area table
    expect_false("Rhinella marina" %in% app_taxon_table(con)$sci)
  })
})

# ---- shards ------------------------------------------------------------------

test_that("the union of taxon shard keys is exactly the taxa.json set", {
  for (gen in gens) with_synth(gen, function(con) {
    sh <- app_taxon_shards(con, gen)
    keys <- unlist(lapply(sh, function(s) names(s$taxa)), use.names = FALSE)
    expect_setequal(keys, unlist(app_taxa(con, gen)$key))
    expect_false(any(duplicated(keys)), info = gen)
  })
})

test_that("a shard name is the key's trailing integer mod 256, in hex", {
  expect_identical(.shard_of("ms_merge|WORMS:137162"), sprintf("%02x", 137162 %% 256))
  expect_identical(.shard_of("101"), sprintf("%02x", 101L))
  expect_identical(.shard_of("am|ITS-96148"), sprintf("%02x", 96148 %% 256))
  expect_identical(.shard_of("no-digits"), "00")     # the documented fallback
  expect_identical(.shard_of(c("x1", "x255", "x256")), c("01", "ff", "00"))
})

test_that("every alias resolves to a key that taxa.json offers", {
  for (gen in gens) with_synth(gen, function(con) {
    al <- app_alias_shards(con, gen)
    keys <- unlist(app_taxa(con, gen)$key)
    for (s in al) for (a in s$alias) expect_true(a[[1]] %in% keys, info = gen)
    # a legacy raw input key resolves to its merged taxon
    flat <- unlist(lapply(al, function(s) names(s$alias)), use.names = FALSE)
    expect_true(all(keys %in% flat), info = gen)   # a merged key is its own alias
  })
})

test_that("REGRESSION: the ms_merge self-edge is never an input", {
  # v1-v7 taxon_model INCLUDES an ms_merge edge and n_ds counts it; v8+ does not.
  # Left in, a v7 taxon reports one more input than the same taxon on v9.
  n_inputs <- function(gen) with_synth(gen, function(con) {
    sh <- app_taxon_shards(con, gen)
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    for (cd in cards) {
      expect_false(any(vapply(cd$inputs, function(i) i$ds_key, "") == "ms_merge"))
      expect_false(any(vapply(cd$inputs, function(i) i$mdl_key, "") == cd$key))
    }
    unname(sort(vapply(cards, function(cd) length(cd$inputs), 0L)))
  })
  # v7's self-edge removed, the two generations agree on the input count
  expect_identical(n_inputs("v7"), n_inputs("v9"))
})

test_that("a taxon with no published surface gets merged = null, not a broken URL", {
  with_synth("v2", function(con) {
    # v2 has no asset table at all: every card must say so rather than invent a URL
    sh <- app_taxon_shards(con, "v2")
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    expect_true(all(vapply(cards, function(cd) is.null(cd$merged), TRUE)))
  })
})

test_that("a merged bbox is in the lon_span_agg frame and null when it spans the globe", {
  with_synth("v9", function(con) {
    sh <- app_taxon_shards(con, "v9")
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    bb <- lapply(cards, function(cd) cd$merged$bbox)
    # taxon 3's merged asset is -180..180: a camera box that says "look at
    # everything" shows the viewer nothing, so it is null
    expect_true(any(vapply(bb, is.null, TRUE)))
    have <- Filter(Negate(is.null), bb)
    expect_true(length(have) >= 1)
    for (b in have) expect_length(b, 4)
  })
})

# ---- boot.json ---------------------------------------------------------------

test_that("boot.json zone metrics equal zone_metric exactly, keyed by metric_key", {
  for (gen in gens) with_synth(gen, function(con) {
    z  <- app_zones(con)$programarea
    vz <- sdm_val_col(con, "zone"); vm <- sdm_val_col(con, "zone_metric")
    pub <- DBI::dbGetQuery(con, glue::glue("
      SELECT z.{vz} AS zkey, m.metric_key, zm.{vm} AS val
        FROM zone z JOIN zone_metric zm USING (zone_seq) JOIN metric m USING (metric_seq)
       WHERE z.fld = 'programarea_key' AND m.metric_key NOT LIKE '%_coverage'"))
    for (zz in z) {
      p <- pub[pub$zkey == zz$key, , drop = FALSE]
      expect_setequal(names(zz$metrics), p$metric_key)
      for (k in p$metric_key)
        expect_equal(zz$metrics[[k]], p$val[p$metric_key == k], info = paste(gen, zz$key, k))
    }
  })
})

test_that("zone summaries carry n_cells and the coverage-weighted area", {
  with_synth("v9", function(con) {
    z <- app_zones(con)$programarea
    a <- Filter(function(x) x$key == "AAA", z)[[1]]
    expect_equal(a$n_cells, 4L)
    expect_equal(a$area_km2, 25 * (100 + 100 + 100 + 50) / 100)   # 87.5
  })
})

test_that("the v7.1 _coverage metrics ride along as `coverage`, optional by presence", {
  with_synth("v7b", function(con) {
    a <- Filter(function(x) x$key == "AAA", app_zones(con)$programarea)[[1]]
    expect_equal(sort(names(a$coverage)),
                 sort(c("extrisk_bird_ecoregion_rescaled",
                        "extrisk_turtle_ecoregion_rescaled",
                        "primprod_ecoregion_rescaled")))
    expect_equal(a$coverage[["extrisk_turtle_ecoregion_rescaled"]], 100 / 350 * 100)
    # and they are NOT mixed into `metrics`
    expect_false(any(grepl("_coverage$", names(a$metrics))))
    # a Program Area where the component has no scored cell has NO coverage row
    b <- Filter(function(x) x$key == "BBB", app_zones(con)$programarea)[[1]]
    expect_false("extrisk_turtle_ecoregion_rescaled" %in% names(b$coverage))
  })
  with_synth("v9", function(con) {
    # no other release has them, and their absence is not an error
    a <- Filter(function(x) x$key == "AAA", app_zones(con)$programarea)[[1]]
    expect_null(a$coverage)
  })
})

test_that("the v7.1 `methods` table rides along, optional by presence", {
  with_synth("v7b", function(con) {
    b <- app_boot(con, "v7b", manifest_build(con, "v7b", base = BASE))
    expect_length(b$methods, 4)
    expect_true(all(vapply(b$methods, function(m) nzchar(m$method_key), TRUE)))
  })
  with_synth("v9", function(con) {
    b <- app_boot(con, "v9", manifest_build(con, "v9", base = BASE))
    expect_null(b$methods)
  })
})

test_that("layers exclude every metric key with no cell rows", {
  for (gen in gens) with_synth(gen, function(con) {
    b <- app_boot(con, gen, manifest_build(con, gen, base = BASE))
    keys <- vapply(b$layers, function(l) l$metric_key, "")
    expect_false(any(grepl("_prepctareaweighting$|_ecoregion_(min|max)$|_coverage$", keys)),
                 info = gen)
    # and the ones kept really do have cell_metric rows
    have <- DBI::dbGetQuery(con, "SELECT DISTINCT metric_seq FROM cell_metric")$metric_seq
    n <- DBI::dbGetQuery(con, "SELECT count(*) n FROM metric WHERE metric_seq IN (
           SELECT DISTINCT metric_seq FROM cell_metric)")$n
    expect_equal(length(keys), n, info = gen)
  })
})

test_that("the grid block is the registry's, for both grids", {
  with_synth("v9", function(con) {
    g <- app_boot(con, "v9", manifest_build(con, "v9", base = BASE))$grid
    expect_equal(g$nc, 7200L); expect_equal(g$grid_id, "global05")
    expect_false(g$lon360); expect_equal(g$tile$size, 50L)
  })
  with_synth("v7", function(con) {
    g <- app_boot(con, "v7", manifest_build(con, "v7", base = BASE))$grid
    expect_equal(g$nc, 3103L); expect_equal(g$grid_id, "usa05")
    expect_true(g$lon360); expect_equal(g$xmin, 141.10)
  })
})

test_that("palettes are 11 stops of #RRGGBB and cover every colormap the release uses", {
  p <- app_palettes()
  expect_setequal(names(p), c("spectral_r", "viridis", "cividis", "magma"))
  for (nm in names(p)) {
    expect_length(p[[nm]], 11)
    expect_true(all(grepl("^#[0-9A-F]{6}$", p[[nm]])), info = nm)
  }
  expect_error(app_palettes("not_a_ramp"), "no ramp for colormap")
  with_synth("v9", function(con) {
    b <- app_boot(con, "v9", manifest_build(con, "v9", base = BASE))
    expect_true("spectral_r" %in% names(b$palettes))
  })
})

test_that("flower_default is versioned, per subregion, and never leaks across releases", {
  with_synth("v9", function(con) {
    # the synthetic release has no subregion zones: an empty OBJECT, not a shared file
    b <- app_boot(con, "v9", manifest_build(con, "v9", base = BASE))
    expect_true(grepl('"flower_default":\\{\\}', app_json(b)))
  })
})

# ---- units -------------------------------------------------------------------

test_that("a unit needs PMTiles, 2 scored zones and 2 geometry keys", {
  with_synth("v9", function(con) {
    m <- manifest_build(con, "v9", base = BASE,
                        zone_tiles = list(`programarea_2026-01` = "https://x/z.pmtiles"),
                        zone_sets = NULL)
    # (a) no composite `score_%` metric in zone_metric yet -> no unit at all
    expect_length(app_units(con, m), 0)

    # give both zones a composite score
    DBI::dbExecute(con, "INSERT INTO zone_metric SELECT zone_seq,
      (SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'), 10
      FROM zone")
    u <- app_units(con, m)
    expect_length(u, 1)
    expect_equal(u[[1]]$zone_type, "programarea")
    expect_equal(u[[1]]$source_layer, "programarea")   # the LAYER is the zone type
    expect_setequal(unlist(u[[1]]$keys), c("AAA", "BBB"))

    # (c) intersecting with the published geometry drops a rollup key WITHOUT
    # hardcoding its name; one key left is not drawable
    expect_length(app_units(con, m, geom_keys = list(programarea = c("AAA"))), 0)
    expect_length(app_units(con, m, geom_keys = list(programarea = c("AAA", "BBB"))), 1)

    # (b) no PMTiles in the manifest -> no unit, whatever is scored
    m2 <- m; m2$zones$pmtiles <- NA_character_
    expect_length(app_units(con, m2), 0)
  })
})

test_that("zone_tbl_for reads the zone table rather than guessing its name", {
  with_synth("v2", function(con) {
    # v1/v2 zone tables are UNSUFFIXED: glue('ply_programareas_2026_{ver}') matched
    # nothing, the cache was written empty and never healed
    expect_identical(zone_tbl_for(con, "programarea_key", "v2"), "ply_programareas_2026")
    expect_true(is.na(zone_tbl_for(con, "planarea_key")))
  })
  with_synth("v7", function(con) {
    expect_identical(zone_tbl_for(con, "programarea_key", "v7"), "ply_programareas_2026_v7")
  })
})

# ---- wide cell tiles ---------------------------------------------------------

test_that("the wide cell tiles carry one column per scored metric, named by key", {
  for (gen in gens) with_synth(gen, function(con) {
    d <- withr::local_tempdir()
    t <- app_cell_tiles(con, file.path(d, "cell"))
    cols <- DBI::dbGetQuery(con, glue::glue(
      "SELECT * FROM read_parquet('{d}/cell/**/*.parquet', hive_partitioning = true) LIMIT 0"))
    expect_true(all(t$metric_keys %in% names(cols)), info = gen)
    expect_true(all(c("cell_id", "area_km2", "in_usa", "in_pra", "tile") %in% names(cols)))
    # nothing joins on metric_seq: it is dropped and recreated every run
    expect_false("metric_seq" %in% names(cols), info = gen)
  })
})

test_that("GATE: per metric, the tiles' multiset digest equals cell_metric's", {
  for (gen in gens) with_synth(gen, function(con) {
    d <- withr::local_tempdir()
    app_cell_tiles(con, file.path(d, "cell"))
    dg <- app_cell_tile_digests(con, file.path(d, "cell"))
    expect_true(all(dg$ok), info = paste(gen, paste(dg$metric_key[!dg$ok], collapse = ",")))
  })
})

test_that("GATE: the tile-width check passes on the release's own nc and FAILS on the other", {
  # The historical silent-empty bug: a wrong ncol computes a DIFFERENT, VALID tile
  # id, so `tile IN (...)` prunes away the only partition holding the cells and the
  # query returns nothing at all rather than erroring.
  for (gen in gens) with_synth(gen, function(con) {
    d  <- withr::local_tempdir()
    t  <- app_cell_tiles(con, file.path(d, "cell"))
    ok <- c(v9 = 7200L, v7 = 3103L, v7b = 3103L, v2 = 3103L)[[gen]]
    expect_equal(t$ncol, ok, info = gen)
    expect_true(app_cell_tile_check(file.path(d, "cell"), ok))
    wrong <- if (ok == 7200L) 3103L else 7200L
    expect_error(app_cell_tile_check(file.path(d, "cell"), wrong),
                 "do not satisfy the tile key", info = gen)
  })
})

test_that("the tile universe is every cell of a tile that holds a metric or model row", {
  with_synth("v9", function(con) {
    d <- withr::local_tempdir()
    t <- app_cell_tiles(con, file.path(d, "cell"))
    want <- DBI::dbGetQuery(con, "SELECT count(DISTINCT cell_id) n FROM cell")$n
    expect_equal(t$rows, as.integer(want))
    # area_km2 comes from `cell`, bit for bit
    a <- DBI::dbGetQuery(con, glue::glue(
      "SELECT t.cell_id, t.area_km2 AS a, c.area_km2 AS b
         FROM read_parquet('{d}/cell/**/*.parquet', hive_partitioning = true) t
         JOIN cell c USING (cell_id)"))
    expect_identical(a$a, a$b)
  })
})

test_that("in_usa / in_pra are NULL on a release that has no such column", {
  with_synth("v7", function(con) {
    d <- withr::local_tempdir()
    app_cell_tiles(con, file.path(d, "cell"))
    x <- DBI::dbGetQuery(con, glue::glue(
      "SELECT in_usa, in_pra FROM read_parquet('{d}/cell/**/*.parquet',
         hive_partitioning = true)"))
    expect_true(all(is.na(x$in_usa)))
    expect_true(all(is.na(x$in_pra)))
  })
})

# ---- zone_taxon --------------------------------------------------------------

test_that("zone_taxon is normalized to ONE schema, er_score always a 0-1 fraction", {
  for (gen in gens) with_synth(gen, function(con) {
    d <- app_zone_taxon(con)
    expect_true(all(c("zone_fld", "zone_value", "sp_cat", "sp_common", "sp_scientific",
                      "taxon_id", "taxon_authority", "er_code", "er_score", "is_mmpa",
                      "is_mbta", "mdl_key", "area_km2", "avg_suit") %in% names(d)),
                info = gen)
    expect_true(all(is.na(d$er_score) | (d$er_score >= 0 & d$er_score <= 1)), info = gen)
    expect_equal(nrow(d), 3, info = gen)
  })
  # v3-v7 stored er_score on the RAW 1-100 scale; the normalizer divides
  with_synth("v7", function(con) expect_equal(sort(app_zone_taxon(con)$er_score),
                                              c(0.01, 0.05, 0.25)))
  with_synth("v9", function(con) expect_equal(sort(app_zone_taxon(con)$er_score),
                                              c(0.01, 0.05, 0.25)))
})

# ---- capabilities ------------------------------------------------------------

test_that("app capabilities are PROBED, never copied from manifest$capabilities", {
  with_synth("v7", function(con) {
    m <- manifest_build(con, "v7", base = BASE,
                        capabilities = list(cell_species_list = TRUE))
    expect_true(manifest_can(m, "cell_species_list"))

    # the bucket answers nothing for example.invalid: every capability is FALSE
    # even though the manifest advertises the server-side one
    p <- app_capabilities("v7", base = BASE, timeout = 2)
    expect_false(any(unlist(p$capabilities)))
    expect_setequal(names(p$capabilities),
                    c("cell", "cell_model", "taxonomy", "alias", "pmtiles_s3"))
    # the probed URL and status are kept so a FALSE can be explained
    expect_true(grepl("^https://", p$probed$cell$url))

    blk <- app_manifest_block("v7", base = BASE, capabilities = p)
    expect_false(blk$capabilities$cell_model)
    expect_identical(blk$boot, paste0(BASE, "/v7/app/boot.json"))
  })
})

test_that("boot.json tables carry an href, byte size and content digest", {
  with_synth("v9", function(con) {
    b <- build(con, "v9")
    tb <- b$out$boot$tables
    expect_true(all(c("taxon", "zone_taxon", "cell") %in% names(tb)))
    for (nm in names(tb)) {
      expect_true(grepl("^https://", tb[[nm]]$href), info = nm)
      expect_gt(tb[[nm]]$bytes, 0)
      expect_gt(nchar(tb[[nm]]$digest), 8)
    }
    # the digest is content-addressed: rebuilding unchanged data reproduces it
    b2 <- build(con, "v9")
    expect_identical(tb$taxon$digest, b2$out$boot$tables$taxon$digest)
  })
})
