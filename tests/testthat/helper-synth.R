# A synthetic release in each generation's shape: 3 taxa, 2 zones, 8 cells.
#
# One content, four schemas. Everything the adapters disagree about is varied here
# and nothing else is, so a test that passes on `v9` and fails on `v2` has found a
# schema assumption rather than an arithmetic one:
#
#   v2   usa05 · mdl_seq · `value` · `is_ok` · NO extinction-risk columns ·
#        unsuffixed zone table · zone_taxon with rl_code/rl_score · no asset table
#   v7   usa05 · mdl_seq · `value` · `is_ok` · er_score on the RAW 1-100 scale ·
#        model_asset.cog_url · taxon_model WITH the ms_merge self-edge
#   v7b  v7 plus the v7.1 patch: a `methods` table and `{component}_coverage`
#        zone metrics in percent, present only where the component has a cell
#   v9   global05 · mdl_key/mdl_id · `val` · is_valid_usa/is_valid_global +
#        is_marine · native_asset · cell_model with mdl_id · cell_grid
#
# The numbers are chosen so the coverage blend and the old present-cells-only mean
# disagree loudly: `turtle` covers ONE of zone A's four cells at value 80, so the
# blend says 22.86 and the old formula says 80.

synth_cells <- function(grid_id) {
  g <- msens::grid_spec_for(grid_id)
  # a 4 x 2 block of cells anchored at (-90, 27.1), the Gulf on either grid
  lon <- -90 + (seq_len(4) - 0.5) * 0.05
  lat <- c(27.075, 27.025)
  d <- expand.grid(lon = lon, lat = lat)
  d <- d[order(d$lat * -1, d$lon), ]
  data.frame(cell_id = as.integer(msens::cell_from_lonlat(d$lon, d$lat, g)),
             lon = d$lon, lat = d$lat,
             area_km2 = 25.0,
             # cell 4 (zone A's partial edge cell) is outside US waters: it is what
             # makes denominator = "study_area" differ from denominator = "all"
             in_usa = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
             in_pra = TRUE, stringsAsFactors = FALSE)
}

# the metric registry, in the order a release mints it (metric_seq is regenerated
# every run, which is why nothing downstream may join on it)
synth_metrics <- function(gen) {
  base <- c("extrisk_bird_ecoregion_rescaled",
            "extrisk_turtle_ecoregion_rescaled",
            "primprod_ecoregion_rescaled")
  keys <- c(base,
            paste0(base, "_prepctareaweighting"),
            "extrisk_bird_ecoregion_min", "extrisk_bird_ecoregion_max",
            "score_extriskspcat_primprod_ecoregionrescaled_equalweights")
  if (identical(gen, "v7b")) keys <- c(keys, paste0(base, "_coverage"))
  data.frame(metric_seq = seq_along(keys), metric_key = keys,
             description = paste("metric", keys), stringsAsFactors = FALSE)
}

synth_release <- function(gen = c("v9", "v7", "v7b", "v2")) {
  gen <- match.arg(gen)
  old <- gen %in% c("v2", "v7", "v7b")          # the v1-v7 shape
  grid_id <- if (old) "usa05" else "global05"
  vcol <- if (old) "value" else "val"
  sfx  <- if (identical(gen, "v2")) "" else paste0("_", gen)

  # its OWN database file per release. Connections to the default ":memory:" share
  # one instance, so shutting one down closes the others mid-test -- and the
  # ":memory:name" spelling is not honoured by the R client, which takes it as a
  # literal filename and leaves `:memory:synth_v9_...` files in the working directory.
  con <- DBI::dbConnect(duckdb::duckdb(
    dbdir = tempfile(sprintf("synth_%s_", gen), fileext = ".duckdb")))

  cl <- synth_cells(grid_id)
  cell <- if (old) cl[, c("cell_id", "area_km2")] else
    cl[, c("cell_id", "lon", "lat", "area_km2", "in_usa", "in_pra")]
  DBI::dbWriteTable(con, "cell", cell)
  id <- cl$cell_id

  met <- synth_metrics(gen)
  DBI::dbWriteTable(con, "metric", met)
  mseq <- stats::setNames(met$metric_seq, met$metric_key)

  # cell_metric: bird everywhere, primprod on two cells, turtle on ONE cell of A
  cm <- rbind(
    data.frame(cell_id = id,        metric_seq = mseq[["extrisk_bird_ecoregion_rescaled"]],
               v = c(50, 50, 50, 50, 10, 10, 10, 10)),
    data.frame(cell_id = id[1],     metric_seq = mseq[["extrisk_turtle_ecoregion_rescaled"]],
               v = 80),
    data.frame(cell_id = id[1:2],   metric_seq = mseq[["primprod_ecoregion_rescaled"]],
               v = 20))
  names(cm)[3] <- vcol
  DBI::dbWriteTable(con, "cell_metric", cm)

  # zones: A = the top row (cell 4 only half covered), B = the bottom row
  zone <- data.frame(
    zone_seq = 1:2,
    tbl = paste0("ply_programareas_2026", sfx),
    fld = "programarea_key", v = c("AAA", "BBB"), stringsAsFactors = FALSE)
  names(zone)[4] <- vcol
  if (!old) zone$zone_set_key <- "programarea_2026-01"
  DBI::dbWriteTable(con, "zone", zone)
  DBI::dbWriteTable(con, "zone_cell", data.frame(
    zone_seq    = rep(1:2, each = 4),
    cell_id     = id,
    pct_covered = c(100, 100, 100, 50, 100, 100, 100, 100)))

  # zone_metric by the PUBLISHED identity: sum(coalesce(val,0)*pct)/sum(pct) over
  # every zone cell. Written out rather than computed so the test asserts against a
  # number the test itself did not derive from the function under test.
  zm <- data.frame(
    zone_seq   = c(1, 1, 1, 2),
    metric_seq = mseq[c("extrisk_bird_ecoregion_rescaled",
                        "extrisk_turtle_ecoregion_rescaled",
                        "primprod_ecoregion_rescaled",
                        "extrisk_bird_ecoregion_rescaled")],
    v = c(17500 / 350, 8000 / 350, 4000 / 350, 10))
  # zone B's turtle is NOT reportable: no _ecoregion_rescaled row at all (never a
  # zero) -- but its _prepctareaweighting row stays, which is why reportability must
  # be read from the ABSENCE of the rescaled row
  zm <- rbind(zm, data.frame(
    zone_seq = 2, metric_seq = mseq[["extrisk_turtle_ecoregion_rescaled_prepctareaweighting"]],
    v = 0))
  if (identical(gen, "v7b"))
    # coverage in PERCENT, Program-Area zones only, absent where uncovered
    zm <- rbind(zm, data.frame(
      zone_seq   = c(1, 1, 1, 2),
      metric_seq = mseq[c("extrisk_bird_ecoregion_rescaled_coverage",
                          "extrisk_turtle_ecoregion_rescaled_coverage",
                          "primprod_ecoregion_rescaled_coverage",
                          "extrisk_bird_ecoregion_rescaled_coverage")],
      v = c(100, 100 / 350 * 100, 200 / 350 * 100, 100)))
  names(zm)[3] <- vcol
  DBI::dbWriteTable(con, "zone_metric", zm)

  # taxa / models ----------------------------------------------------------
  sci  <- c("Sterna paradisaea", "Chelonia mydas", "Gadus morhua")
  cmn  <- c("Arctic Tern", "Green Sea Turtle", "Atlantic Cod")
  cat_ <- c("bird", "turtle", "fish")
  tid  <- c("137162", "137206", "126436")
  mkey <- paste0("ms_merge|WORMS:", tid)
  mseqs <- c(101L, 102L, 103L)

  if (old) {
    taxon <- data.frame(
      taxon_id = tid, taxon_authority = "worms", scientific_name = sci,
      common_name = cmn, sp_cat = cat_, mdl_seq = mseqs, is_ok = TRUE,
      redlist_code = c("LC", "EN", "VU"), stringsAsFactors = FALSE)
    if (!identical(gen, "v2")) {                      # v1/v2 have NO ER columns
      taxon$extrisk_code <- c("LC", "EN", "VU")
      taxon$er_score     <- c(1, 25, 5)               # RAW 1-100 on v3-v7
      taxon$is_mmpa      <- c(FALSE, FALSE, FALSE)
      taxon$is_mbta      <- c(TRUE, FALSE, FALSE)
    }
    DBI::dbWriteTable(con, "taxon", taxon)
    DBI::dbWriteTable(con, "model", data.frame(
      mdl_seq = c(mseqs, 201L, 202L), ds_key = c(rep("ms_merge", 3), "am", "am"),
      taxa = c(sci, sci[1:2]), stringsAsFactors = FALSE))
    if (!identical(gen, "v2")) {
      # v1-v7 taxon_model INCLUDES the ms_merge self-edge; v8+ does not
      DBI::dbWriteTable(con, "taxon_model", data.frame(
        taxon_id = c(tid, tid[1:2]), ds_key = c(rep("ms_merge", 3), "am", "am"),
        mdl_seq  = c(mseqs, 201L, 202L), stringsAsFactors = FALSE))
      DBI::dbWriteTable(con, "model_asset", data.frame(
        mdl_key = paste0("am|", tid[1:2]), mdl_seq = c(101L, 102L), ds_key = "ms_merge",
        cog_url = paste0("https://example.invalid/cog/usa05/", tid[1:2], ".tif"),
        grid_id = "usa05", ver = gen, stringsAsFactors = FALSE))
    }
    zt <- data.frame(
      zone_tbl = zone$tbl[1], zone_fld = "programarea_key", zone_value = "AAA",
      mdl_seq = mseqs, sp_cat = cat_, sp_common = cmn, sp_scientific = sci,
      taxon_id = tid, taxon_authority = "worms", rl_code = c("LC", "EN", "VU"),
      area_km2 = c(75, 25, 50), avg_suit = c(0.5, 0.8, 0.3), stringsAsFactors = FALSE)
    # v1/v2 stored a 0-1 rl_score; v3-v7 a 1-100 er_score beside rl_code
    if (identical(gen, "v2")) zt$rl_score <- c(0.01, 0.25, 0.05)
    else                      zt$er_score <- c(1, 25, 5)
    DBI::dbWriteTable(con, "zone_taxon", zt)
  } else {
    DBI::dbWriteTable(con, "taxon", data.frame(
      taxon_id = tid, taxon_authority = "worms", ms_merge_key = mkey,
      scientific_name = sci, common_name = cmn, sp_cat = cat_,
      iucn_code = c("LC", "EN", "VU"), extrisk_code = c("LC", "EN", "VU"),
      er_score = c(1, 25, 5), is_mmpa = FALSE, is_mbta = c(TRUE, FALSE, FALSE),
      is_marine = TRUE, is_valid_usa = c(TRUE, TRUE, FALSE),
      is_valid_global = c(TRUE, FALSE, TRUE), rarity = "common",
      stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "model", data.frame(
      mdl_key = c(mkey, paste0("am|", tid[1:2])), mdl_id = c(1L, 2L, 3L, 4L, 5L),
      ds_key = c(rep("ms_merge", 3), "am", "am"), sp_id = c(tid, tid[1:2]),
      sci_name = c(sci, sci[1:2]), common_name = c(cmn, cmn[1:2]),
      er_score = c(1, 25, 5, 1, 25), sp_cat = c(cat_, cat_[1:2]),
      stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "taxon_model", data.frame(   # no self-edge on v8+
      mdl_key = paste0("am|", tid[1:2]), ds_key = "am",
      taxon_authority = "worms", taxon_id = tid[1:2], ms_merge_key = mkey[1:2],
      stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "native_asset", data.frame(
      ms_merge_key = c(mkey, mkey[1:2]),
      mdl_key      = c(mkey, paste0("am|", tid[1:2])),
      ds_key       = c(rep("ms_merge", 3), "am", "am"),
      asset_type   = "cog", representation = c(rep("model", 3), "native", "native"),
      asset_url    = c(paste0("https://example.invalid/merged/", tid, ".tif"),
                       paste0("https://example.invalid/native/", tid[1:2], ".tif")),
      rescale_min = 1, rescale_max = 100, colormap = "spectral_r",
      xmin = c(-90.2, -90.2, -180, -90.2, -90.2), xmax = c(-89.6, -89.6, 180, -89.6, -89.6),
      ymin = 26.9, ymax = 27.2, source_layer = NA_character_, stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "zone_taxon", data.frame(
      zone_fld = "programarea_key", zone_value = "AAA", sp_cat = cat_,
      sp_common = cmn, sp_scientific = sci, taxon_id = tid, taxon_authority = "worms",
      er_code = c("LC", "EN", "VU"), er_score = c(0.01, 0.25, 0.05),
      is_mmpa = FALSE, is_mbta = c(TRUE, FALSE, FALSE), mdl_key = mkey,
      area_km2 = c(75, 25, 50), avg_suit = c(0.5, 0.8, 0.3), stringsAsFactors = FALSE))
    DBI::dbWriteTable(con, "cell_model", data.frame(
      cell_id = rep(id[1:4], 2), mdl_id = rep(c(1L, 2L), each = 4),
      val = c(50, 40, 30, 20, 80, 10, 10, 10),
      tile = msens::cell_model_tiles(rep(id[1:4], 2), ncol = 7200L)[1]))
    msens::cell_grid_write(con, "global05")
  }

  DBI::dbWriteTable(con, "dataset", data.frame(
    ds_key = c("ms_merge", "am"), name_display = c("Merged", "AquaMaps"),
    value_info = c("suitability 0-100", "suitability 0-100"),
    is_mask = FALSE, sort_order = c(1L, 2L),
    citation = c("msens", "Kaschner et al."), stringsAsFactors = FALSE))

  if (identical(gen, "v7b"))
    DBI::dbWriteTable(con, "methods", data.frame(
      method_key = c("coverage_floor", "turtle_suit_min", "turtle_fill", "half_even"),
      value = c("0.05", "0.20", "NA", "TRUE"),
      description = paste("v7.1", c("coverage floor", "suitability floor",
                                    "range fill", "half-even rounding")),
      stringsAsFactors = FALSE))

  con
}

# the cell ids of the synthetic release, in zone order
synth_cell_ids <- function(grid_id) synth_cells(grid_id)$cell_id

# run `f(con)` against a synthetic release and close it afterwards.
#
# NOT `on.exit()` inside a loop: on.exit captures the EXPRESSION `con`, which is
# re-evaluated at exit against whatever `con` then holds -- so a four-generation
# loop closed the last connection four times and never closed the first three.
with_synth <- function(gen, f) {
  con <- synth_release(gen)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  f(con)
}
