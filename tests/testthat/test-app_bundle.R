# The `{ver}/app/` data contract: one schema for v1…v9, built from four very
# different databases. Every assertion here is a per-version adapter that does NOT
# have to be written again in TypeScript.

gens <- c("v9", "v7", "v7b", "v2")
BASE <- "https://example.invalid/marine-atlas"

build <- function(con, gen, ...) {
  m <- synth_manifest(con, gen, BASE)
  d <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "app")
  list(dir = d, manifest = m,
       out = app_bundle_build(con, gen, d, manifest = m, base = BASE, ...))
}

# ---- schema validation -------------------------------------------------------

test_that("every builder validates its own output against its schema", {
  for (gen in gens) with_synth(gen, function(con) {
    m <- synth_manifest(con, gen, BASE)
    expect_silent(app_boot(con, gen, m, tables = list()))
    expect_silent(app_taxa(con, gen))
    expect_type(app_taxon_shards(con, gen), "list")
    expect_type(app_alias_shards(con, gen), "list")
  })
})

test_that("a deliberately malformed object FAILS schema validation", {
  with_synth("v9", function(con) {
    m <- synth_manifest(con, "v9", BASE)
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
    z  <- app_zones(con, chosen = app_zone_tbl(con, synth_manifest(con, gen, BASE)))$programarea
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
    b <- app_boot(con, "v7b", synth_manifest(con, "v7b", BASE))
    expect_length(b$methods, 4)
    expect_true(all(vapply(b$methods, function(m) nzchar(m$method_key), TRUE)))
  })
  with_synth("v9", function(con) {
    b <- app_boot(con, "v9", synth_manifest(con, "v9", BASE))
    expect_null(b$methods)
  })
})

test_that("layers exclude every metric key with no cell rows", {
  for (gen in gens) with_synth(gen, function(con) {
    b <- app_boot(con, gen, synth_manifest(con, gen, BASE))
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
    g <- app_boot(con, "v9", synth_manifest(con, "v9", BASE))$grid
    expect_equal(g$nc, 7200L); expect_equal(g$grid_id, "global05")
    expect_false(g$lon360); expect_equal(g$tile$size, 50L)
  })
  with_synth("v7", function(con) {
    g <- app_boot(con, "v7", synth_manifest(con, "v7", BASE))$grid
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
    b <- app_boot(con, "v9", synth_manifest(con, "v9", BASE))
    expect_true("spectral_r" %in% names(b$palettes))
  })
})

test_that("flower_default is versioned, per subregion, and never leaks across releases", {
  with_synth("v9", function(con) {
    # the synthetic release has no subregion zones: an empty OBJECT, not a shared file
    b <- app_boot(con, "v9", synth_manifest(con, "v9", BASE))
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

test_that("GATE: exactly ONE data_0.parquet per tile, and `tiles` counts tiles", {
  # Anonymous LIST is denied on the bucket, so a static client cannot discover
  # parts: it constructs `tile={t}/data_0.parquet` and reads whatever is there.
  # DuckDB's PARTITION_BY wrote one part PER THREAD -- measured on v9: 422 tile
  # directories, 1,687 files, 290 of them multi-part -- so the browser would have
  # read about a sixth of a busy tile and been told nothing.
  for (gen in gens) with_synth(gen, function(con) {
    d <- withr::local_tempdir()
    t <- app_cell_tiles(con, file.path(d, "cell"))
    dirs <- list.dirs(file.path(d, "cell"), recursive = FALSE)
    expect_equal(t$tiles, length(dirs), info = gen)      # tiles, not files
    expect_equal(t$files, length(dirs), info = gen)
    for (p in dirs)
      expect_identical(list.files(p, "[.]parquet$"), "data_0.parquet", info = p)
    expect_equal(app_one_file_per_partition(file.path(d, "cell")), length(dirs))
  })
})

test_that("SEEDED: a tile split into two parts turns BOTH gates red", {
  with_synth("v9", function(con) {
    d  <- withr::local_tempdir()
    cd <- file.path(d, "cell")
    t  <- app_cell_tiles(con, cd)
    expect_true(all(app_cell_tile_digests(con, cd)$ok))       # green first

    # split one tile exactly as PARTITION_BY did: data_0 keeps half the rows
    td  <- file.path(cd, list.files(cd)[1])
    src <- file.path(td, "data_0.parquet")
    cp <- function(sel, out) DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM read_parquet('%s') WHERE %s) TO '%s' (FORMAT parquet)",
      src, sel, out))
    cp("cell_id % 2 = 0", file.path(td, "part.parquet"))
    cp("cell_id % 2 = 1", file.path(td, "data_1.parquet"))
    file.remove(src); file.rename(file.path(td, "part.parquet"), src)

    expect_error(app_one_file_per_partition(cd), "do not hold exactly one")
    # the DIGEST gate must fail too: it now reads the way the client does, so the
    # rows in data_1 are simply missing. Globbing `**/*.parquet` saw all of them.
    expect_false(all(app_cell_tile_digests(con, cd)$ok))
    # ...and the tile-WIDTH check still passes, because the ids are all valid --
    # which is why the two gates are separate rather than one
    expect_true(app_cell_tile_check(cd, t$ncol))
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
    d <- app_zone_taxon(con, app_zone_tbl(con, synth_manifest(con, gen, BASE)))
    expect_true(all(c("zone_fld", "zone_value", "sp_cat", "sp_common", "sp_scientific",
                      "taxon_id", "taxon_authority", "er_code", "er_score", "is_mmpa",
                      "is_mbta", "mdl_key", "area_km2", "avg_suit") %in% names(d)),
                info = gen)
    expect_true(all(is.na(d$er_score) | (d$er_score >= 0 & d$er_score <= 1)), info = gen)
    # 3 taxa per published zone: v2 also carries a subregion, so 6
    expect_equal(nrow(d), if (gen == "v2") 6 else 3, info = gen)
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

# ---- the manifest `app` block ------------------------------------------------
#
# Only v9's manifest comes from build_version_manifest.qmd; v1-v7b are written by
# backfill_versions.qmd through manifest_build(..., extra = list(...)), which is
# where `methods` went. So the `app` block has to be an argument of manifest_build()
# itself, not something a second notebook bolts on afterwards.

CAPS <- list(cell = TRUE, cell_model = FALSE, taxonomy = TRUE,
             alias = TRUE, pmtiles_s3 = FALSE)

test_that("a manifest with no `app` argument has NO `app` key at all", {
  for (gen in gens) with_synth(gen, function(con) {
    m <- synth_manifest(con, gen, BASE)
    expect_null(m$app, info = gen)
    expect_false("app" %in% names(m), info = gen)
    expect_silent(validate_manifest(m, ver = gen))
  })
})

test_that("a present `app` block is filled in from ver and base, and validates", {
  with_synth("v9", function(con) {
    m <- manifest_build(con, "v9", base = BASE,
                        app = list(capabilities = CAPS,
                                   built_at = "2026-09-21T00:00:00Z"))
    expect_identical(m$app$schema, 1L)
    expect_identical(m$app$base, paste0(BASE, "/v9/app"))
    expect_identical(m$app$boot, paste0(BASE, "/v9/app/boot.json"))
    expect_identical(m$app$built_at, "2026-09-21T00:00:00Z")
    expect_identical(m$app$capabilities, CAPS)
    expect_silent(validate_manifest(m, ver = "v9"))
    expect_silent(validate_manifest_app(m$app))

    # the probe evidence rides along when the notebook supplies it
    m2 <- manifest_build(con, "v9", base = BASE, app = list(
      capabilities = CAPS,
      probed = list(cell = list(url = paste0(BASE, "/v9/app/cell/tile=0/data_0.parquet"),
                                status = 200L))))
    expect_identical(m2$app$probed$cell$status, 200L)
    expect_silent(validate_manifest(m2, ver = "v9"))
  })
})

test_that("capabilities are INPUTS: msens never copies them from manifest$capabilities", {
  with_synth("v7", function(con) {
    # v7 advertises cell_species_list because the SERVER can read a cell_model that
    # never left the server; S3 holds only tables/ for it
    m <- manifest_build(con, "v7", base = BASE,
                        capabilities = list(cell_species_list = TRUE),
                        app = list(capabilities = CAPS))
    expect_true(manifest_can(m, "cell_species_list"))
    expect_false(m$app$capabilities$cell_model)     # what the BUCKET actually answers
    expect_false(identical(names(m$capabilities), names(m$app$capabilities)))
  })
})

test_that("a malformed `app` block is an error, per branch", {
  with_synth("v9", function(con) {
    bad <- function(app) manifest_build(con, "v9", base = BASE, app = app)
    expect_error(bad(list(capabilities = c(CAPS, list(nope = TRUE)))), "unknown key")
    expect_error(bad(list(capabilities = utils::modifyList(CAPS, list(cell = "yes")))),
                 "single non-NA logicals")
    expect_error(bad(list(capabilities = utils::modifyList(CAPS, list(cell = NA)))),
                 "single non-NA logicals")
    expect_error(bad(list(capabilities = utils::modifyList(CAPS, list(cell = c(TRUE, TRUE))))),
                 "single non-NA logicals")
    expect_error(bad(list(capabilities = CAPS["cell"])), "missing: cell_model")
    expect_error(bad(list(capabilities = list())), "NON-EMPTY NAMED list")
    expect_error(bad(list(capabilities = list(TRUE, FALSE, TRUE, TRUE, FALSE))),
                 "NON-EMPTY NAMED list")
    expect_error(bad(list()), "capabilities")
    expect_error(bad("nope"), "must be a list")
  })
})

test_that("`methods` and `app` ride together, each optional by presence", {
  with_synth("v7b", function(con) {
    md <- DBI::dbGetQuery(con, "SELECT * FROM release_method ORDER BY method_key")
    m <- manifest_build(con, "v7b", base = BASE,
                        extra = list(methods = md),          # backfill_versions.qmd
                        app   = list(capabilities = CAPS))   # the new argument
    expect_equal(nrow(m$methods), 4)
    expect_identical(m$app$capabilities$taxonomy, TRUE)
    expect_silent(validate_manifest(m, ver = "v7b"))
  })
  with_synth("v9", function(con) {
    m <- manifest_build(con, "v9", base = BASE, app = list(capabilities = CAPS))
    expect_null(m$methods)                                    # neither is implied
    expect_false(is.null(m$app))
  })
})

test_that("an old manifest with neither key still validates", {
  # every release published before the browser contract existed
  old <- list(ver = "v3", status = "retired", access = "public",
              grid_id = "usa05", id_field = "mdl_seq",
              capabilities = list(cell_species_list = FALSE),
              tables = list(cell = "https://example.invalid/v3/tables/cell.parquet"))
  expect_silent(validate_manifest(old, ver = "v3"))
  expect_null(old$app)

  # ...and a manifest carrying a MALFORMED app block does not
  bad <- old
  bad$app <- list(schema = 1L, base = "https://x/v3/app", boot = "https://x/v3/app/boot.json",
                  built_at = "2026-09-21T00:00:00Z",
                  capabilities = list(cell = TRUE))          # four keys missing
  expect_error(validate_manifest(bad, ver = "v3"))
})

# ---- what the real releases found, that the synthetic ones had not ------------

test_that("REGRESSION: taxon_id never carries a trailing .0, on any generation", {
  # v7 stores taxon.taxon_id as a DOUBLE, and DuckDB's CAST(22725044.0 AS VARCHAR)
  # is the string "22725044.0" -- so the published contract carried it on all
  # 16,153 v7 rows, every WoRMS link built from it 404s, and a join against another
  # release's integer-typed id matches nothing. The synthetic fixtures typed the
  # column as VARCHAR and could not see it.
  for (gen in gens) with_synth(gen, function(con) {
    d <- app_taxon_table(con)
    expect_true(all(is.na(d$taxon_id) | grepl("^[0-9]+$", d$taxon_id)),
                info = paste(gen, paste(d$taxon_id, collapse = ",")))
    expect_false(any(grepl("[.]", d$taxon_id, fixed = FALSE) & !is.na(d$taxon_id)),
                 info = gen)
  })
  # ...and the legacy fixtures really do store it as a DOUBLE, or this proves nothing
  with_synth("v7", function(con) {
    ty <- DBI::dbGetQuery(con,
      "SELECT column_type FROM (DESCRIBE SELECT * FROM taxon) WHERE column_name = 'taxon_id'")[[1]]
    expect_identical(ty, "DOUBLE")
  })
})

test_that("REGRESSION: a NA key never injects an all-NA edge row", {
  # v7's real shape: a taxon with NO merged model (mdl_seq IS NULL) that still has
  # taxon_model rows. `CAST(NULL AS VARCHAR)` is NA, so `d$mdl_key != d$key` is NA,
  # and `d[NA, ]` INJECTS an all-NA row rather than dropping it: 2,354 of 14,501
  # edges on the real release. card()'s own `e[e$key == key, ]` then matched every
  # one of them into EVERY taxon -- 38 M phantom inputs, and the shard step died.
  for (gen in c("v9", "v7", "v7b")) with_synth(gen, function(con) {
    e <- .app_edges(con)
    expect_equal(sum(!stats::complete.cases(e)), 0L, info = gen)
    expect_false(any(is.na(e$key)), info = gen)

    # the bare logical subset on the SAME data would have injected one
    k <- .app_taxon_cols(con)
    raw <- if ("ms_merge_key" %in% DBI::dbListFields(con, "taxon_model"))
      DBI::dbGetQuery(con, "SELECT CAST(ms_merge_key AS VARCHAR) AS key,
             CAST(mdl_key AS VARCHAR) AS mdl_key, ds_key FROM taxon_model")
    else
      DBI::dbGetQuery(con, sprintf(
        "SELECT CAST(t.%s AS VARCHAR) AS key, CAST(tm.mdl_seq AS VARCHAR) AS mdl_key,
                tm.ds_key FROM taxon_model tm JOIN taxon t
           ON CAST(t.taxon_id AS HUGEINT) = CAST(tm.taxon_id AS HUGEINT)", k$key))
    naive <- raw[raw$ds_key != "ms_merge" & raw$mdl_key != raw$key, , drop = FALSE]
    expect_equal(nrow(naive) - nrow(e), 1L, info = gen)
    expect_equal(sum(!stats::complete.cases(naive)), 1L, info = gen)
  })
})

test_that("REGRESSION: a taxon with no merged model gets no phantom inputs", {
  for (gen in c("v9", "v7")) with_synth(gen, function(con) {
    sh <- app_taxon_shards(con, gen)
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    for (cd in cards) {
      # every input belongs to THIS taxon, and none is an all-NA row
      expect_false(any(vapply(cd$inputs, function(i) is.null(i$mdl_key) ||
                                is.na(i$mdl_key), TRUE)), info = cd$key)
      expect_lte(length(cd$inputs), 2L, label = cd$key)
    }
    # the 4th taxon is not in the picker set at all (no merged key), so it cannot
    # drag its edges in
    expect_false("Orcinus orca" %in% vapply(cards, function(cd) cd$sci, ""))
  })
})

# ---- C: a fractional id is a data error, never a rounded neighbour -----------

test_that("REGRESSION: a non-integral taxon_id is never rounded into another id", {
  # `CAST(12.7 AS HUGEINT)` is 13. Published, that is a different and possibly
  # EXISTING id, and nothing downstream could tell it from a real one -- the WoRMS
  # link resolves, to the wrong animal. Cast through an integer type only where the
  # value is whole; keep the value's own text otherwise so it can be named.
  with_synth("v7", function(con) {
    expect_true(all(grepl("^[0-9]+$", app_taxon_table(con)$taxon_id)))

    DBI::dbExecute(con, "UPDATE taxon SET taxon_id = 12.7
                          WHERE scientific_name = 'Gadus morhua'")
    cast <- .app_id_cast(con, "taxon", "taxon_id", "taxon_id")
    ids  <- DBI::dbGetQuery(con, sprintf("SELECT %s AS id FROM taxon", cast))$id
    expect_false("13" %in% ids)           # the whole point
    expect_true("12.7" %in% ids)

    # ...and the BUILD stops, naming the row and counting them
    e <- tryCatch(app_taxon_table(con), error = function(e) conditionMessage(e))
    # 3, not 4: the taxon with no merged model is not in the picker set
    expect_match(e, "1 of 3 `taxon_id` values are not integral")
    expect_match(e, "12.7", fixed = TRUE)
    expect_match(e, "Gadus morhua", fixed = TRUE)
    expect_match(e, "12.7 -> 13", fixed = TRUE)
    expect_error(app_bundle_build(con, "v7", withr::local_tempdir(),
                                  manifest = synth_manifest(con, "v7", BASE),
                                  base = BASE), "not integral")
  })
})

test_that("whole DOUBLE ids, negatives and NULL still cast cleanly", {
  with_synth("v7", function(con) {
    one <- function() DBI::dbGetQuery(con, sprintf(
      "SELECT %s AS id FROM taxon WHERE scientific_name = 'Gadus morhua'",
      .app_id_cast(con, "taxon", "taxon_id", "taxon_id")))$id
    # a magnitude past 2^53: the double stores 9007199254740992 and the cast is
    # exact on THAT value -- the precision limit is the column's, not the cast's
    DBI::dbExecute(con, "UPDATE taxon SET taxon_id = 9007199254740992 WHERE scientific_name = 'Gadus morhua'")
    expect_identical(one(), "9007199254740992")
    DBI::dbExecute(con, "UPDATE taxon SET taxon_id = -42 WHERE scientific_name = 'Gadus morhua'")
    expect_identical(one(), "-42")
    DBI::dbExecute(con, "UPDATE taxon SET taxon_id = NULL WHERE scientific_name = 'Gadus morhua'")
    expect_true(is.na(one()))
    # a NULL id is allowed through the integrality check
    expect_silent(.app_assert_integral_ids(
      data.frame(taxon_id = c("1", NA_character_), stringsAsFactors = FALSE)))
  })
})

# ---- D: `methods` / `val`, which only v7b has --------------------------------

test_that("the methods block is read from `release_method`, the table v7b really has", {
  # v7b's sdm.duckdb holds `release_method`; `methods` is the MANIFEST's key for it.
  # Looking only for `methods` found nothing on the real release and emitted no
  # block at all, silently -- and v7b is the only release that has one.
  with_synth("v7b", function(con) {
    expect_true("release_method" %in% DBI::dbListTables(con))
    expect_false("methods" %in% DBI::dbListTables(con))
    b <- app_boot(con, "v7b", synth_manifest(con, "v7b", BASE))
    expect_length(b$methods, 4)
  })
})

test_that("boot$methods rows are method_key / val / description, never `value`", {
  # v7b is the ONLY release with a `methods` table, so nothing real exercised this
  # until now: the rename from `value` to `val` was asserted by the schema alone.
  with_synth("v7b", function(con) {
    b <- app_boot(con, "v7b", synth_manifest(con, "v7b", BASE))
    expect_length(b$methods, 4)
    for (mth in b$methods) {
      expect_setequal(names(mth), c("method_key", "val", "description"))
      expect_false("value" %in% names(mth))
      expect_true(nzchar(mth$method_key))
    }
    # ...and the published BYTES never contain the word
    j <- app_json(b)
    expect_false(grepl('"value"', j, fixed = TRUE))
    expect_true(grepl('"val"', j, fixed = TRUE))

    # SEEDED: rename back to `value` and both the schema and the grep go red
    bad <- b
    bad$methods <- lapply(b$methods, function(m)
      list(method_key = m$method_key, value = m$val, description = m$description))
    expect_error(app_validate(bad, "boot"), "required property 'val'")
    expect_true(grepl('"value"', app_json(bad), fixed = TRUE))   # the gate's grep
  })
})

test_that("a release with no `methods` table yields no `methods` key at all", {
  for (gen in c("v9", "v7", "v2")) with_synth(gen, function(con) {
    expect_false(any(c("release_method", "methods") %in% DBI::dbListTables(con)),
                 info = gen)
    b <- app_boot(con, gen, synth_manifest(con, gen, BASE))
    expect_null(b$methods, info = gen)
    expect_false("methods" %in% names(b), info = gen)
    # optional by presence means ABSENT, not an empty array an app would iterate
    expect_false(grepl('"methods"', app_json(b), fixed = TRUE), info = gen)
  })
})

test_that("the smoke gate's grep for \"value\" would catch a regression", {
  # the gate reads every published JSON; reproduce that read on a written bundle
  with_synth("v7b", function(con) {
    d <- withr::local_tempdir()
    app_bundle_build(con, "v7b", d, manifest = synth_manifest(con, "v7b", BASE),
                     base = BASE)
    jsons <- list.files(d, "[.]json$", recursive = TRUE, full.names = TRUE)
    expect_gt(length(jsons), 0)
    has_value <- function(fs) Filter(function(f)
      any(grepl('"value"', readLines(f, warn = FALSE), fixed = TRUE)), fs)
    expect_length(has_value(jsons), 0)

    # seed one: the grep must find it
    bad <- file.path(d, "boot.json")
    writeLines(sub('"val"', '"value"', readLines(bad, warn = FALSE), fixed = TRUE), bad)
    expect_length(has_value(jsons), 1)
  })
})

# ---- what the all-release sweep found ----------------------------------------

test_that("REGRESSION: a release whose `dataset` has no is_mask still builds", {
  # v1's dataset has no such column; `SELECT ds_key, is_mask FROM dataset` failed
  # with a Binder Error and took the whole shard stage with it. D11: a release that
  # cannot supply a capability simply does not advertise it.
  with_synth("v1", function(con) {
    expect_false("is_mask" %in% DBI::dbListFields(con, "dataset"))
    sh <- app_taxon_shards(con, "v1")
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    expect_gt(length(cards), 0)
    for (cd in cards) for (i in cd$inputs) expect_null(i$is_mask)   # NULL, not invented
    expect_silent(app_datasets(con))
  })
})

test_that("REGRESSION: cell_metric rows for cells absent from `cell` do not break the digest", {
  # 703 such cells on real v3. The tiles publish only cells that ARE in `cell`
  # (deliberately: the browser's JOIN drops the rest), so the source side of the
  # digest has to be restricted the same way or every metric reports a mismatch.
  with_synth("v1", function(con) {
    orph <- DBI::dbGetQuery(con, "SELECT count(DISTINCT cm.cell_id) n FROM cell_metric cm
      LEFT JOIN cell c USING (cell_id) WHERE c.cell_id IS NULL")$n
    expect_equal(orph, 1L)                     # the fixture really has one

    d <- withr::local_tempdir()
    app_cell_tiles(con, file.path(d, "cell"))
    dg <- app_cell_tile_digests(con, file.path(d, "cell"))
    expect_true(all(dg$ok))

    # the orphan is absent from the published tiles, as the contract says
    ids <- DBI::dbGetQuery(con, sprintf(
      "SELECT DISTINCT cell_id FROM read_parquet('%s/cell/*/data_0.parquet',
         hive_partitioning = true)", d))$cell_id
    have <- DBI::dbGetQuery(con, "SELECT cell_id FROM cell")$cell_id
    expect_true(all(ids %in% have))
  })
})

# ---- G: one id text on every published object --------------------------------

test_that("REGRESSION: zone_taxon publishes the same id text as taxon.parquet", {
  # `.app_id_cast()` reached app_taxon_table() and stopped there. zone_taxon is a
  # PRECOMPUTED table read with SELECT *, so v7's DOUBLE taxon_id came straight
  # through and this object said "22725044.0" beside the other's "22725044".
  for (gen in c("v9", "v7", "v7b", "v2")) with_synth(gen, function(con) {
    zt <- app_zone_taxon(con, app_zone_tbl(con, synth_manifest(con, gen, BASE)))
    tx <- app_taxon_table(con)
    expect_true(all(is.na(zt$taxon_id) | grepl("^[0-9]+$", zt$taxon_id)), info = gen)
    expect_false(any(grepl("[.]0$", zt$taxon_id)), info = gen)
    # and the two objects really do join
    expect_gt(length(intersect(zt$taxon_id, tx$taxon_id)), 0)
  })
})

test_that("app_id_chr normalises every id shape, and refuses a fractional one", {
  expect_identical(app_id_chr(c(137162, NA)), c("137162", NA))
  expect_identical(app_id_chr(c("22725044.0", "7")), c("22725044", "7"))
  expect_identical(app_id_chr(c(-42L, 0L)), c("-42", "0"))
  expect_identical(app_id_chr(9007199254740992), "9007199254740992")  # no sci notation
  expect_identical(app_id_chr(character(0)), character(0))
  expect_error(app_id_chr(12.7, "taxon_id"), "not integral")
  expect_error(app_id_chr("12.7", "taxon_id"), "not integral")
  # ".0" means whole and is stripped; ".7" is a data error, never rounded
  expect_false(identical(app_id_chr("12.0"), "12.7"))
})

# ---- B + F: taxonomy and the model mapping are IN the contract ---------------

test_that("boot$tables names exactly the Parquet objects under app/", {
  # taxonomy.parquet was written by the notebook OUTSIDE the bundle and the
  # mdl_id -> mdl_key mapping lived outside app/ entirely, so neither had a digest
  # and OPFS could never invalidate them.
  for (gen in c("v9", "v7")) with_synth(gen, function(con) {
    d <- withr::local_tempdir()
    b <- app_bundle_build(con, gen, d, manifest = synth_manifest(con, gen, BASE),
                          base = BASE)
    expect_silent(app_tables_match(d, b$boot))
    for (nm in names(b$boot$tables)) expect_gt(nchar(b$boot$tables[[nm]]$digest), 8)

    # seeded both ways: an object with no entry, and an entry with no object
    file.copy(file.path(d, "taxon.parquet"), file.path(d, "surprise.parquet"))
    expect_error(app_tables_match(d, b$boot), "written but NOT in boot")
    file.remove(file.path(d, "surprise.parquet"))
    bad <- b$boot; bad$tables$ghost <- list(href = "https://x", bytes = 1L, digest = "abcdefghij")
    expect_error(app_tables_match(d, bad), "would 404")
  })
})

test_that("app_model writes the mdl_id mapping on v8+ and nothing on v1-v7", {
  with_synth("v9", function(con) {
    d <- withr::local_tempdir()
    t <- app_model(con, "v9", d)
    expect_false(is.null(t))
    expect_setequal(t$columns, c("mdl_id", "mdl_key", "ds_key"))
    expect_true(file.exists(file.path(d, "model.parquet")))
    b <- app_bundle_build(con, "v9", withr::local_tempdir(),
                          manifest = synth_manifest(con, "v9", BASE), base = BASE)
    expect_true("model" %in% names(b$boot$tables))
  })
  for (gen in c("v7", "v2")) with_synth(gen, function(con) {
    # no mdl_id: cell_model joins on mdl_seq directly, so no mapping is needed --
    # and none is invented
    d <- withr::local_tempdir()
    expect_null(app_model(con, gen, d))
    expect_false(file.exists(file.path(d, "model.parquet")))
    b <- app_bundle_build(con, gen, withr::local_tempdir(),
                          manifest = synth_manifest(con, gen, BASE), base = BASE)
    expect_false("model" %in% names(b$boot$tables))
  })
})

test_that("app_taxonomy restricts the hierarchy to the release's taxa, or writes nothing", {
  with_synth("v9", function(con) {
    d <- withr::local_tempdir()
    expect_null(app_taxonomy(con, "v9", d, NULL))              # no CSV -> nothing
    expect_null(app_taxonomy(con, "v9", d, file.path(d, "absent.csv")))

    csv <- file.path(d, "hier.csv")
    tx  <- app_taxon_table(con)
    utils::write.csv(data.frame(
      species_id = c(tx$taxon_id, "999999"),                  # one taxon not in the release
      Kingdom = "Animalia", Phylum = "Chordata", stringsAsFactors = FALSE),
      csv, row.names = FALSE)
    t <- app_taxonomy(con, "v9", d, csv)
    expect_equal(t$rows, nrow(tx))                             # the stranger is dropped
    expect_true(file.exists(file.path(d, "taxonomy.parquet")))
    got <- DBI::dbGetQuery(con, sprintf(
      "SELECT taxon_id FROM read_parquet('%s')", file.path(d, "taxonomy.parquet")))
    expect_setequal(got$taxon_id, tx$taxon_id)
    expect_false(any(grepl("[.]", got$taxon_id)))              # same id text as everywhere
  })
})

# ---- the fault that stayed green: a TYPE, not a text -------------------------
#
# Deleting app_zone_taxon()'s id-normalising loop left every existing test green,
# because `as.character(137162)` is "137162" -- R drops the ".0" that DuckDB's
# CAST keeps, so a text assertion on the in-R frame cannot see a DOUBLE at all.
# What the real-data gate saw was the WRITTEN column: zone_taxon.parquet carried
# taxon_id as DOUBLE while taxon.parquet carried VARCHAR, and nothing joined.
# So these assert the TYPE and the written file, not the printed value.

test_that("REGRESSION: zone_taxon ids are text without .0 when the release stores them as DOUBLE", {
  for (gen in c("v7", "v7b", "v2")) with_synth(gen, function(con) {
    src <- DBI::dbGetQuery(con, "SELECT column_type FROM (DESCRIBE SELECT * FROM zone_taxon)
                                  WHERE column_name = 'taxon_id'")[[1]]
    expect_identical(src, "DOUBLE", info = gen)      # the fixture is the real shape

    zt <- app_zone_taxon(con, app_zone_tbl(con, synth_manifest(con, gen, BASE)))
    expect_type(zt$taxon_id, "character")            # <- the loop, not as.character()
    expect_setequal(zt$taxon_id, c("137162", "126436", "137206"))
    expect_false(any(grepl("[.]", zt$taxon_id)), info = gen)

    # and the WRITTEN object: a DOUBLE here is exactly what the smoke gate caught
    d <- withr::local_tempdir()
    write_atlas_parquet(zt, file.path(d, "zone_taxon.parquet"))
    ty <- DBI::dbGetQuery(con, sprintf(
      "SELECT column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))
        WHERE column_name = 'taxon_id'", file.path(d, "zone_taxon.parquet")))[[1]]
    expect_identical(ty, "VARCHAR", info = gen)

    # ...and the two published objects really do JOIN on it
    tx <- app_taxon_table(con)
    expect_type(tx$taxon_id, "character")
    common <- intersect(zt$taxon_id, tx$taxon_id)
    expect_equal(length(common), 3L, info = gen)   # intersect() is already unique
    expect_setequal(common, c("137162", "126436", "137206"))
  })
})

test_that("REGRESSION: model joins match on a DOUBLE mdl_seq", {
  # `taxon` holds mdl_seq as an INTEGER and `model_asset` as a DOUBLE, so casting
  # both to VARCHAR compared "101" with "101.0": the join matched NOTHING and every
  # taxon silently lost its native asset.
  for (gen in c("v7", "v7b")) with_synth(gen, function(con) {
    expect_identical(
      DBI::dbGetQuery(con, "SELECT column_type FROM (DESCRIBE SELECT * FROM model_asset)
                             WHERE column_name = 'mdl_seq'")[[1]], "DOUBLE", info = gen)

    a <- .app_assets(con)
    expect_gt(nrow(a), 0)                               # non-zero, exact
    expect_equal(nrow(a), 2L, info = gen)
    expect_setequal(a$mdl_key, c("101", "102"))
    expect_false(any(grepl("[.]", a$mdl_key)), info = gen)
    # the assets attach to the taxa they belong to, not to nobody
    expect_setequal(a$key, c("101", "102"))

    # the naive both-sides VARCHAR cast the fix replaced: zero matches
    naive <- DBI::dbGetQuery(con, "
      SELECT count(*) n FROM model_asset ma
        JOIN taxon t ON CAST(t.mdl_seq AS VARCHAR) = CAST(ma.mdl_seq AS VARCHAR)")$n
    expect_equal(naive, 0L, info = gen)

    # and the cards carry the asset through
    sh <- app_taxon_shards(con, gen)
    cards <- unlist(lapply(sh, function(s) unname(s$taxa)), recursive = FALSE)
    expect_gt(sum(vapply(cards, function(cd) length(cd$inputs), 0L)), 0L)
  })
})

# ---- one zone table per field ------------------------------------------------

test_that("REGRESSION: a field with two zone tables publishes exactly one", {
  # v2 carries ply_subregions_2025 (AK, AKL48, L48, USA) and ply_subregions_2026
  # (AK, GA, PA, USA) both under subregion_key. app_zone_taxon() dropped zone_tbl,
  # so 13,077 (zone_fld, zone_value, key) groups came out duplicated -- the same
  # taxon in the same subregion with two different area_km2 -- and boot$zones$
  # subregion had six rows for four subregions.
  with_synth("v2", function(con) {
    n_tbl <- DBI::dbGetQuery(con, "SELECT count(DISTINCT tbl) n FROM zone
                                    WHERE fld = 'subregion_key'")$n
    expect_equal(n_tbl, 2L)                    # the fixture is the real shape

    # the registry names ply_subregions_2025 as the source of a published zone set
    zs <- data.frame(zone_set_key = "subregion_2025-08", zone_type = "subregion",
                     source = "v1/ply_subregions_2025.gpkg", stringsAsFactors = FALSE)
    mf <- synth_manifest(con, "v2", BASE)
    ch <- app_zone_tbl(con, mf, zone_sets = zs)
    expect_identical(ch$tbl[ch$fld == "subregion_key"], "ply_subregions_2025")
    expect_match(ch$why[ch$fld == "subregion_key"], "named by manifest.json", fixed = TRUE)

    # geometry keys are a CHECK on that choice, not a second opinion -- but only
    # where a unit will be published, so the zones have to be scored first (D16)
    sq <- DBI::dbGetQuery(con,
      "SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'")$metric_seq[1]
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone_metric SELECT zone_seq, %d, 10 FROM zone
        WHERE tbl = 'ply_subregions_2025'", sq))
    ch2 <- app_zone_tbl(con, mf, geom_keys = list(subregion = c("SR1", "SR2")))
    expect_identical(ch2$tbl[ch2$fld == "subregion_key"], "ply_subregions_2025")
    expect_match(ch2$why[ch2$fld == "subregion_key"], "geometry keys agree", fixed = TRUE)

    # and with NO manifest there is no guess at all
    expect_error(app_zone_tbl(con), "manifest names none of them", fixed = TRUE)

    # the other table's rows are DROPPED, never merged
    zt <- app_zone_taxon(con, ch)
    expect_equal(nrow(zt), 6L)                 # 3 taxa x (1 programarea + 1 SR1), not 9
    expect_setequal(zt$zone_value, c("AAA", "SR1"))
    expect_false("ply_subregions_2026" %in% zt$zone_tbl)
    k <- paste(zt$zone_fld, zt$zone_value, zt$mdl_key)
    expect_equal(length(unique(k[duplicated(k)])), 0L)

    z <- app_zones(con, chosen = ch)
    expect_setequal(vapply(z$subregion, function(x) x$key, ""), c("SR1", "SR2"))
    expect_false(anyDuplicated(vapply(z$subregion, function(x) x$key, "")) > 0)
  })
})

test_that("the chosen zone_tbl is recorded in boot$units", {
  with_synth("v2", function(con) {
    DBI::dbExecute(con, "INSERT INTO zone_metric SELECT zone_seq,
      (SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'), 10
      FROM zone WHERE tbl = 'ply_subregions_2025'")
    m <- manifest_build(con, "v2", base = BASE,
                        zone_tiles = list(`subregion_2025-08` = "https://x/z.pmtiles"),
                        zone_sets = NULL)
    m$zones$zone_set_key <- "subregion_2025-08"
    m$zones$pmtiles <- "https://x/z.pmtiles"
    gk <- list(subregion = c("SR1", "SR2"))
    ch <- app_zone_tbl(con, synth_manifest(con, "v2", BASE), geom_keys = gk)
    u  <- app_units(con, m, geom_keys = gk, chosen = ch)
    sr <- Filter(function(x) x$zone_type == "subregion", u)
    expect_length(sr, 1)
    expect_identical(sr[[1]]$zone_tbl, "ply_subregions_2025")
    expect_setequal(unlist(sr[[1]]$keys), c("SR1", "SR2"))
  })
})

test_that("app_zones_unique is a hard stop on a duplicated zone key", {
  # hand-built duplicates, so the assertion is tested rather than assumed
  zt <- data.frame(zone_fld = "subregion_key", zone_value = c("AK", "AK", "GA"),
                   mdl_key = c("1", "1", "2"), stringsAsFactors = FALSE)
  boot_ok  <- list(zones = list(subregion = list(list(key = "AK"), list(key = "GA"))))
  boot_dup <- list(zones = list(subregion = list(list(key = "AK"), list(key = "AK"))))

  expect_error(app_zones_unique(zt, boot_ok), "duplicated (zone_fld, zone_value",
               fixed = TRUE)
  expect_error(app_zones_unique(zt, boot_ok), "subregion_key / AK / 1", fixed = TRUE)
  expect_error(app_zones_unique(zt[-2, ], boot_dup), "lists AK more than once")
  expect_true(app_zones_unique(zt[-2, ], boot_ok))
  expect_true(app_zones_unique(NULL, boot_ok))       # a release with no zone_taxon
})

test_that("every generation's bundle holds each zone exactly once", {
  for (gen in gens) with_synth(gen, function(con) {
    d <- withr::local_tempdir()
    b <- app_bundle_build(con, gen, d, manifest = synth_manifest(con, gen, BASE),
                          base = BASE)
    expect_silent(app_zones_unique(
      app_zone_taxon(con, app_zone_tbl(con, synth_manifest(con, gen, BASE))), b$boot))
  })
})

test_that("the builder STOPS when duplicates reach zone_taxon anyway", {
  # `zone_tbl` is how app_zone_taxon() drops the other table's rows. A release with
  # two tables for one field but NO zone_tbl column on zone_taxon cannot be filtered
  # -- and then the builder-level assertion is the only thing between that and a
  # bundle listing every taxon twice. So it is a hard stop, not a warning.
  with_synth("v2", function(con) {
    DBI::dbExecute(con, "CREATE OR REPLACE TABLE zone_taxon AS
                           SELECT * EXCLUDE (zone_tbl) FROM zone_taxon")
    expect_false("zone_tbl" %in% DBI::dbListFields(con, "zone_taxon"))

    zt <- app_zone_taxon(con, app_zone_tbl(con, synth_manifest(con, "v2", BASE)))
    k <- paste(zt$zone_fld, zt$zone_value, zt$mdl_key)
    expect_gt(length(unique(k[duplicated(k)])), 0L)

    expect_error(
      app_bundle_build(con, "v2", withr::local_tempdir(),
                       manifest = synth_manifest(con, "v2", BASE), base = BASE),
      "duplicated (zone_fld, zone_value", fixed = TRUE)
  })
})

# ---- the manifest decides, and nothing else gets a vote ----------------------

test_that("the manifest's zones[].tbl chooses the table, even against the newer one", {
  # v2's manifest.json says subregion_key -> ply_subregions_2025
  # (zone_set_key subregion_2025-06), the OLDER of its two tables. A date guess
  # picked ply_subregions_2026 and published AK, GA, PA, USA -- keys that match no
  # published geometry. Two defensible answers depending on the caller is the
  # ambiguity this rule exists to remove.
  with_synth("v2", function(con) {
    older <- "ply_subregions_2025"          # date_created 2025-08-06
    newer <- "ply_subregions_2026"          # date_created 2026-01-12
    mf <- list(zones = data.frame(
      fld = c("programarea_key", "subregion_key"),
      tbl = c("ply_programareas_2026", older),
      zone_set_key = c("programarea_2026-01", "subregion_2025-08"),
      stringsAsFactors = FALSE))

    ch <- app_zone_tbl(con, mf)
    expect_identical(ch$tbl[ch$fld == "subregion_key"], older)
    expect_match(ch$why[ch$fld == "subregion_key"], "named by manifest.json", fixed = TRUE)
    expect_match(ch$why[ch$fld == "subregion_key"], "subregion_2025-08", fixed = TRUE)
    expect_false(newer %in% ch$tbl)

    zt <- app_zone_taxon(con, ch)
    expect_equal(nrow(zt), 6L)                       # 3 taxa x (1 programarea + SR1)
    expect_false(newer %in% zt$zone_tbl)
    z <- app_zones(con, chosen = ch)
    expect_setequal(vapply(z$subregion, function(x) x$key, ""), c("SR1", "SR2"))
  })
})

test_that("geometry keys that DISAGREE with the manifest stop the build", {
  with_synth("v2", function(con) {
    mf <- list(zones = data.frame(fld = "subregion_key", tbl = "ply_subregions_2025",
                                  zone_set_key = "subregion_2025-08",
                                  stringsAsFactors = FALSE))
    # the check runs only where a unit is published, so score both subregions (D16)
    sq <- DBI::dbGetQuery(con,
      "SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'")$metric_seq[1]
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone_metric SELECT zone_seq, %d, 10 FROM zone
        WHERE tbl = 'ply_subregions_2025'", sq))
    # the keys of the OTHER table: the notebook was handed the wrong GeoPackage
    expect_error(app_zone_tbl(con, mf, geom_keys = list(subregion = c("SR1", "SR3"))),
                 "does not match the table the manifest names", fixed = TRUE)
    expect_error(app_zone_tbl(con, mf, geom_keys = list(subregion = c("SR1", "SR3"))),
                 "wrong GeoPackage", fixed = TRUE)
    # ...and the right ones are recorded as agreeing
    ch <- app_zone_tbl(con, mf, geom_keys = list(subregion = c("SR1", "SR2")))
    expect_match(ch$why[ch$fld == "subregion_key"], "geometry keys agree", fixed = TRUE)
  })
})

test_that("two tables and no manifest row is an ERROR, never a guess", {
  with_synth("v2", function(con) {
    expect_error(app_zone_tbl(con), "has 2 zone tables", fixed = TRUE)
    expect_error(app_zone_tbl(con), "subregion_key", fixed = TRUE)
    expect_error(app_zone_tbl(con), "manifest names none of them", fixed = TRUE)
    # a manifest naming a table the release does not have is also an error
    bad <- list(zones = data.frame(fld = "subregion_key", tbl = "ply_subregions_2030",
                                   stringsAsFactors = FALSE))
    expect_error(app_zone_tbl(con, bad), "holds only", fixed = TRUE)
    # a release with ONE table per field needs no manifest at all
    with_synth("v7", function(c7) expect_silent(app_zone_tbl(c7)))
  })
})

test_that("the zone-set registry is a cross-check, not a chooser", {
  with_synth("v2", function(con) {
    mf <- list(zones = data.frame(fld = "subregion_key", tbl = "ply_subregions_2025",
                                  stringsAsFactors = FALSE))
    zs <- data.frame(zone_set_key = "subregion_2025-08", zone_type = "subregion",
                     source = "v1/ply_subregions_2026.gpkg", stringsAsFactors = FALSE)
    # the registry points at the OTHER table: the manifest still wins, and the
    # disagreement is recorded rather than acted on
    ch <- app_zone_tbl(con, mf, zone_sets = zs)
    expect_identical(ch$tbl[ch$fld == "subregion_key"], "ply_subregions_2025")
    expect_match(ch$why[ch$fld == "subregion_key"], "registry lists no such source",
                 fixed = TRUE)
  })
})

test_that("REGRESSION: a manifest naming two tables for one field stops the build", {
  # `manifest_build()` on a release with two tables for a field emits BOTH rows, so
  # it names no single table -- it reproduces the ambiguity it is being asked to
  # settle. Taking the first row would make the answer depend on row order, which is
  # the same defect round 6 removed from the date guess.
  with_synth("v2", function(con) {
    two <- list(zones = data.frame(
      fld = c("programarea_key", "subregion_key", "subregion_key"),
      tbl = c("ply_programareas_2026", "ply_subregions_2025", "ply_subregions_2026"),
      stringsAsFactors = FALSE))

    expect_error(app_zone_tbl(con, two), "names no single table", fixed = TRUE)
    expect_error(app_zone_tbl(con, two), "the manifest lists 2 rows", fixed = TRUE)
    expect_error(app_zone_tbl(con, two), "subregion_key", fixed = TRUE)
    expect_error(app_zone_tbl(con, two),
                 "ply_subregions_2025, ply_subregions_2026", fixed = TRUE)
    # row order must not decide it either
    rev2 <- two; rev2$zones <- two$zones[c(1, 3, 2), ]
    expect_error(app_zone_tbl(con, rev2), "names no single table", fixed = TRUE)

    # and the whole build stops, not just the helper
    expect_error(
      app_bundle_build(con, "v2", withr::local_tempdir(), manifest = two, base = BASE),
      "names no single table", fixed = TRUE)

    # BENIGN: two rows for one field naming the SAME table (a manifest listing a
    # zone set twice) is not ambiguous -- it names one table, so it is chosen
    same <- list(zones = data.frame(
      fld = c("programarea_key", "subregion_key", "subregion_key"),
      tbl = c("ply_programareas_2026", "ply_subregions_2025", "ply_subregions_2025"),
      zone_set_key = c("programarea_2026-01", "subregion_2025-06", "subregion_2025-08"),
      stringsAsFactors = FALSE))
    ch <- app_zone_tbl(con, same)
    expect_identical(ch$tbl[ch$fld == "subregion_key"], "ply_subregions_2025")
    expect_match(ch$why[ch$fld == "subregion_key"], "named by manifest.json", fixed = TRUE)
  })
})

# ---- D16: the geometry is checked where a UNIT is published, and nowhere else --

# a release whose `subregion_key` has `n_scored` of its zones carrying a score_%
# metric, so the "is this a unit?" gate can be driven directly
synth_scored_subregions <- function(n_scored) {
  con <- synth_release("v9")
  DBI::dbExecute(con, "INSERT INTO zone (zone_seq, tbl, fld, val, zone_set_key) VALUES
    (3, 'ply_subregions_2026_v9', 'subregion_key', 'AK',  'subregion_2025-06'),
    (4, 'ply_subregions_2026_v9', 'subregion_key', 'GA',  'subregion_2025-06'),
    (5, 'ply_subregions_2026_v9', 'subregion_key', 'USA', 'subregion_2025-06')")
  # cells, or app_zones() lists nothing: it summarises zone JOIN zone_cell JOIN cell
  DBI::dbExecute(con, "INSERT INTO zone_cell
    SELECT z.zone_seq, c.cell_id, 100 FROM zone z, cell c WHERE z.zone_seq IN (3, 4, 5)")
  sq <- DBI::dbGetQuery(con,
    "SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'")$metric_seq[1]
  keys <- c("AK", "GA", "USA")[seq_len(n_scored)]
  for (k in keys)
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone_metric SELECT zone_seq, %d, 10 FROM zone
        WHERE fld = 'subregion_key' AND val = '%s'", sq, k))
  con
}

test_that("a field with fewer than 2 scored zones ignores its geometry, and says so", {
  # v1-v7b: the subregion zones exist but carry no score_% metric (v6 0 of 4,
  # v7/v7b only the FULL rollup). The notebook passes the published 2025-06 geometry
  # (AK, AT, GA, PA) to every release, and checking it against the full zone table
  # stopped v1 on "AT, GA, PA" and v4-v7b on "AT" -- over fields that would never be
  # a unit. The stop's purpose is right; its universe was wrong.
  for (n in 0:1) local({
    con <- synth_scored_subregions(n)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    mf <- synth_manifest(con, "v9", BASE)
    gk <- list(subregion = c("AK", "AT", "GA", "PA"))   # AT is in NO release's table

    ch <- expect_silent(app_zone_tbl(con, mf, geom_keys = gk))
    expect_match(ch$why[ch$fld == "subregion_key"],
                 sprintf("no unit: %d of 3 zones scored; geometry not checked", n),
                 fixed = TRUE, info = paste("n_scored", n))
    # ...and no unit is published for it
    m2 <- mf; m2$zones$pmtiles <- "https://x/z.pmtiles"
    u <- app_units(con, m2, geom_keys = gk, chosen = ch)
    expect_false("subregion" %in% vapply(u, function(x) x$zone_type, ""))
  })
})

test_that("REGRESSION: where a unit IS published, a geometry key that is not scored stops the build", {
  con <- synth_scored_subregions(2)          # AK, GA scored -> a unit
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mf <- synth_manifest(con, "v9", BASE)

  expect_error(
    app_zone_tbl(con, mf, geom_keys = list(subregion = c("AK", "GA", "AT"))),
    "in the geometry but NOT in that table: AT", fixed = TRUE)
  expect_error(
    app_zone_tbl(con, mf, geom_keys = list(subregion = c("AK", "GA", "AT"))),
    "wrong GeoPackage", fixed = TRUE)
  expect_error(
    app_bundle_build(con, "v9", withr::local_tempdir(), manifest = mf, base = BASE,
                     geom_keys = list(subregion = c("AK", "GA", "AT"))),
    "in the geometry but NOT in that table", fixed = TRUE)
})

test_that("a unit publishes exactly the keys that are both scored and drawn", {
  # USA is scored and has no polygon (the whole-study-area rollup): it stays out of
  # units[].keys without being an error, and stays IN boot$zones.
  con <- synth_scored_subregions(3)          # AK, GA, USA scored
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mf <- synth_manifest(con, "v9", BASE)
  mf$zones$pmtiles <- "https://x/z.pmtiles"
  gk <- list(subregion = c("AK", "GA"))      # the geometry draws no USA

  ch <- app_zone_tbl(con, mf, geom_keys = gk)
  expect_match(ch$why[ch$fld == "subregion_key"], "geometry keys agree", fixed = TRUE)

  u  <- app_units(con, mf, geom_keys = gk, chosen = ch)
  sr <- Filter(function(x) x$zone_type == "subregion", u)
  expect_length(sr, 1)
  expect_setequal(unlist(sr[[1]]$keys), c("AK", "GA"))     # scored AND drawn
  expect_false("USA" %in% unlist(sr[[1]]$keys))

  z <- app_zones(con, chosen = ch)
  expect_true("USA" %in% vapply(z$subregion, function(x) x$key, ""))   # kept here
})

test_that("a scored zone with NO zone_taxon rows publishes n_taxa = 0 and keeps its unit", {
  # v8 scores subregion `AT` and gives it 52,674 cells, but its zone_taxon has no
  # rows for it at all (v9 has 7,562). A gap in the RELEASE's table: never invent
  # rows, never drop the unit -- publish the count and let the app say so.
  con <- synth_scored_subregions(2)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(*) n FROM zone_taxon WHERE zone_fld = 'subregion_key'")$n, 0L)

  mf <- synth_manifest(con, "v9", BASE); mf$zones$pmtiles <- "https://x/z.pmtiles"
  ch <- app_zone_tbl(con, mf, geom_keys = list(subregion = c("AK", "GA")))
  z  <- app_zones(con, chosen = ch)

  sr <- z$subregion
  expect_gt(length(sr), 0)
  for (e in sr) {
    expect_true("n_taxa" %in% names(e))
    expect_identical(e$n_taxa, 0L)              # 0, not absent and not invented
    expect_gt(e$n_cells, 0)                     # the zone is real
  }
  # the programarea zones DO have zone_taxon rows, so the field is not always 0
  expect_true(any(vapply(z$programarea, function(e) e$n_taxa, 0L) > 0L))

  # ...and the unit is still published, because the score is real
  u  <- app_units(con, mf, geom_keys = list(subregion = c("AK", "GA")), chosen = ch)
  expect_true("subregion" %in% vapply(u, function(x) x$zone_type, ""))

  # the schema requires it
  b <- app_boot(con, "v9", mf, chosen = ch)
  expect_silent(app_validate(b, "boot"))
  bad <- b
  bad$zones$subregion <- lapply(b$zones$subregion, function(e) { e$n_taxa <- NULL; e })
  expect_error(app_validate(bad, "boot"), "n_taxa")
})

# ---- round 9: n_taxa is per ZONE, and `score_` really is the filter ------------

# three subregion zones with deliberately DIFFERENT taxon counts, so a per-field
# total cannot masquerade as a per-zone one
synth_taxa_counts <- function(n_by_key = c(AK = 0L, GA = 1L, PA = 3L),
                              metric_like = "score") {
  con <- synth_release("v9")
  # the programarea zones get a score_ metric so they publish a unit: without one,
  # "no subregion unit" would pass vacuously on a release with no units at all
  sq0 <- DBI::dbGetQuery(con,
    "SELECT metric_seq FROM metric WHERE metric_key LIKE 'score!_%' ESCAPE '!'")$metric_seq[1]
  DBI::dbExecute(con, sprintf(
    "INSERT INTO zone_metric SELECT zone_seq, %d, 10 FROM zone
      WHERE fld = 'programarea_key'", sq0))
  keys <- names(n_by_key)
  for (i in seq_along(keys))
    DBI::dbExecute(con, sprintf(
      "INSERT INTO zone (zone_seq, tbl, fld, val, zone_set_key)
         VALUES (%d, 'ply_subregions_2026_v9', 'subregion_key', '%s', 'subregion_2025-06')",
      2L + i, keys[i]))
  DBI::dbExecute(con, "INSERT INTO zone_cell
    SELECT z.zone_seq, c.cell_id, 100 FROM zone z, cell c WHERE z.zone_seq > 2")
  sq <- DBI::dbGetQuery(con, sprintf(
    "SELECT metric_seq FROM metric WHERE metric_key LIKE '%s%%'", metric_like))$metric_seq[1]
  DBI::dbExecute(con, sprintf(
    "INSERT INTO zone_metric SELECT zone_seq, %d, 10 FROM zone WHERE zone_seq > 2", sq))
  for (i in seq_along(keys)) {
    n <- n_by_key[[i]]
    if (n > 0) DBI::dbExecute(con, sprintf(
      "INSERT INTO zone_taxon SELECT 'subregion_key', '%s', sp_cat, sp_common,
         sp_scientific, taxon_id, taxon_authority, er_code, er_score, is_mmpa,
         is_mbta, mdl_key, area_km2, avg_suit
         FROM zone_taxon WHERE zone_fld = 'programarea_key' LIMIT %d", keys[i], n))
  }
  con
}

test_that("REGRESSION: n_taxa counts THAT zone's rows, not the whole field's", {
  # `sum(nt$n_taxa[nt$fld == fld])` gives every zone of the field the same total,
  # so a zone with no species table looks populated and one with few looks rich.
  con <- synth_taxa_counts(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  mf <- synth_manifest(con, "v9", BASE); mf$zones$pmtiles <- "https://x/z.pmtiles"
  ch <- app_zone_tbl(con, mf, geom_keys = list(subregion = c("AK", "GA", "PA")))
  z  <- app_zones(con, chosen = ch)$subregion

  got <- stats::setNames(vapply(z, function(e) e$n_taxa, 0L),
                         vapply(z, function(e) e$key, ""))
  expect_identical(got[c("AK", "GA", "PA")], c(AK = 0L, GA = 1L, PA = 3L))
  # the three must not be equal, or a per-field total would pass
  expect_equal(length(unique(got[c("AK", "GA", "PA")])), 3L)
  expect_false(any(got[c("AK", "GA", "PA")] == sum(c(0L, 1L, 3L))))

  # the zone with 0 is still published, with its cells
  ak <- Filter(function(e) e$key == "AK", z)[[1]]
  expect_identical(ak$n_taxa, 0L)
  expect_gt(ak$n_cells, 0)
})

test_that("REGRESSION: only a `score_` metric makes a zone count toward a unit", {
  # Dropping the LIKE 'score!_%' filter makes ANY zone_metric row count, so a field
  # whose zones carry only coverage/preweight metrics becomes a unit that has no
  # composite to draw a choropleth from.
  con <- synth_taxa_counts(metric_like = "extrisk_bird_ecoregion_rescaled")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # three subregion zones, each with a NON-score metric and none with a score_ one
  n_score <- DBI::dbGetQuery(con, "
    SELECT count(*) n FROM zone z JOIN zone_metric zm USING (zone_seq)
      JOIN metric m USING (metric_seq)
     WHERE z.fld = 'subregion_key' AND m.metric_key LIKE 'score!_%' ESCAPE '!'")$n
  expect_equal(n_score, 0L)
  expect_gte(DBI::dbGetQuery(con, "
    SELECT count(*) n FROM zone z JOIN zone_metric zm USING (zone_seq)
     WHERE z.fld = 'subregion_key'")$n, 3L)

  # .app_scored_keys(): the field has NO scored keys
  expect_null(.app_scored_keys(con)[["subregion_key"]])

  # so the geometry is ignored rather than checked -- even keys the table lacks
  mf <- synth_manifest(con, "v9", BASE); mf$zones$pmtiles <- "https://x/z.pmtiles"
  ch <- expect_silent(app_zone_tbl(con, mf, geom_keys = list(subregion = c("AK", "AT"))))
  expect_match(ch$why[ch$fld == "subregion_key"], "no unit: 0 of", fixed = TRUE)

  # ...and app_units() -- which runs its OWN copy of the query -- publishes none
  u <- app_units(con, mf, geom_keys = list(subregion = c("AK", "GA", "PA")), chosen = ch)
  expect_false("subregion" %in% vapply(u, function(x) x$zone_type, ""))
  # the programarea unit is unaffected, so this is not a vacuous "no units" pass
  expect_true("programarea" %in% vapply(u, function(x) x$zone_type, ""))
})
