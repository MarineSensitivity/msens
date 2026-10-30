# Extracted from test-share.R:425

# prequel ----------------------------------------------------------------------
skip_if_not_installed("duckdb")
NC <- 100L
LAND <- 290:299
AREA <- rep(1, 310)
AREA[61] <- 0.98
share_fixture_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())

  cell <- data.frame(cell_id = setdiff(1:310, LAND), in_usa = FALSE)
  cell$area_km2 <- AREA[cell$cell_id]; cell$in_usa <- cell$cell_id <= 100

  zone <- data.frame(
    zone_seq = 1:8,
    fld      = c(rep("programarea_key", 6), "subregion_key", "ecoregion_key"),
    val      = c("PA-A", "PA-B", "PA-C", "PA-K", "PA-L", "REM", "USA", "ECO-H"))
  zone_cell <- rbind(
    data.frame(zone_seq = 1L, cell_id =  1:10,  pct_covered = 100),
    data.frame(zone_seq = 2L, cell_id = 11:20,  pct_covered = 100),
    data.frame(zone_seq = 3L, cell_id = 21:40,  pct_covered = 100),
    data.frame(zone_seq = 4L, cell_id = 41:60,  pct_covered = 100),
    data.frame(zone_seq = 5L, cell_id = 61:80,  pct_covered = 100),
    data.frame(zone_seq = 6L, cell_id = 81:100, pct_covered = 100),
    data.frame(zone_seq = 7L, cell_id =  1:100, pct_covered = 100),
    data.frame(zone_seq = 8L, cell_id =  1:2,   pct_covered = 50))

  taxon <- data.frame(
    ms_merge_key = c("endemic", "wide", "wide2", "dup", "hi", "low", "exclu", "rept", "even", "uniform", "nat", "cov", "turtle", "dps", "dps20", "bird"),
    sp_cat       = c("A", "A", "A", "A", "A", "A", "A", "R", "B", "C", "D", "E", "turtle", "F", "F", "G"),
    er_score     = c(50, 100, 100, 100, 100, 100, 100, 100, 100, 20, 100, 100, 100, 100, 100, 10),
    er_mode      = c(rep("taxon", 12), "premultiplied", "cell", "cell", "taxon"),
    is_valid_usa = TRUE, is_marine = TRUE, stringsAsFactors = FALSE)
  taxon$is_valid_usa[taxon$ms_merge_key == "exclu"] <- FALSE

  mc <- function(key, cells, val) data.frame(mdl_key = key, cell_id = cells, val = val)
  # the US scoring surface (dup: two AquaMaps models -> its cell appears twice, as the merge keeps it)
  model_cell <- rbind(
    mc("endemic", 1:10, 100), mc("wide", 1, 100), mc("wide2", 11, 100), mc("dup", c(13, 13), 100),
    mc("hi", 50:51, 100), mc("low", 12, 1), mc("exclu", 1:10, 100), mc("rept", 1:10, 100),
    mc("even", c(1:10, 21:40), 50), mc("uniform", 1:100, 100), mc("nat", 1:10, 100),
    mc("cov", c(41, 61), 100), mc("turtle", 1:2, c(80, 1)), mc("dps", 1:2, 80), mc("dps20", 3:4, 80),
    mc("bird", 5, 100))
  # the whole-range global surfaces of the range-constrained taxa (am-only taxa have none)
  mc_global <- rbind(
    mc("endemic", 1:10, 100), mc("low", 12, 1), mc("exclu", 1:10, 100), mc("rept", 1:10, 100),
    mc("even", c(1:10, 21:40), 50), mc("uniform", 1:100, 100), mc("nat", 1:10, 100),
    mc("cov", c(41, 61), 100), mc("turtle", c(1, 2, 101), c(80, 1, 50)), mc("dps", c(1, 2, 101), 80),
    mc("dps20", 3:4, 80), mc("bird", c(5, LAND), 100))
  # the AquaMaps surfaces of the am-only taxa (wide, wide2, dup x2, hi), plus one for `endemic` to
  # prove the merged global surface takes precedence over AquaMaps
  mc_am <- rbind(
    mc("am|wide",  c(1, 101:199), 100), mc("am|wide2", c(11, 101:289, 300:309), 100),
    mc("am|dupa",  c(13, 101:198), 100), mc("am|dupb", 13, 100),
    mc("am|hi",    50:51, 100), mc("am|endemic", c(1:10, 101:110), 100))
  model_cell_er <- data.frame(mdl_key = c("dps", "dps", "dps20", "dps20"), cell_id = c(1, 2, 3, 4), er = c(100, 20, 20, 20))
  taxon_model <- data.frame(
    mdl_key = c("rng|endemic", "am|endemic", "am|wide", "am|wide2", "am|dupa", "am|dupb", "am|hi", "rng|low", "rng|exclu", "rng|rept",
                "rng|even", "rng|uniform", "rng_fws|nat", "rng|cov", "rng_turtle_swot_dps|turtle", "dps_nmfs|dps", "dps_nmfs|dps20", "bl|bird"),
    ms_merge_key = c("endemic", "endemic", "wide", "wide2", "dup", "dup", "hi", "low", "exclu", "rept",
                     "even", "uniform", "nat", "cov", "turtle", "dps", "dps20", "bird"),
    ds_key = c("rng_iucn", "am", "am", "am", "am", "am", "am", "rng_iucn", "rng_iucn", "rng_iucn",
               "rng_iucn", "rng_iucn", "rng_fws", "rng_iucn", "rng_turtle_swot_dps", "dps_nmfs", "dps_nmfs", "bl"),
    stringsAsFactors = FALSE)
  fallback <- data.frame(ms_merge_key = "nat", fraction = 0.25, method = "literature", stringsAsFactors = FALSE)

  # the denominators as the share tests expect them (denom_sql() is tested separately against these):
  # sum of v/100 * A over the species' whole range, land included
  gl <- function(key, strip = FALSE) {
    g <- mc_global[mc_global$mdl_key == key, ]
    if (nrow(g) == 0) return(NA_real_)
    v <- g$val; if (strip && key == "turtle") v[v <= 1] <- 0
    sum(v / 100 * AREA[g$cell_id])
  }
  taxon_denom <- data.frame(
    ms_merge_key       = taxon$ms_merge_key,
    total_global       = vapply(taxon$ms_merge_key, gl, 0),
    total_global_strip = vapply(taxon$ms_merge_key, gl, 0, strip = TRUE),
    source             = "merged_global", stringsAsFactors = FALSE)
  set_d <- function(key, tot, src) {
    i <- taxon_denom$ms_merge_key == key
    taxon_denom$total_global[i] <<- tot; taxon_denom$total_global_strip[i] <<- tot; taxon_denom$source[i] <<- src
  }
  set_d("wide",  100, "aquamaps"); set_d("wide2", 200, "aquamaps"); set_d("dup", 98 + 2, "aquamaps"); set_d("hi", 2, "aquamaps")
  set_d("nat", 10 / 0.25, "fallback:literature")

  for (nm in c("cell", "zone", "zone_cell", "taxon", "model_cell", "mc_global", "mc_am", "model_cell_er",
               "taxon_model", "fallback", "taxon_denom"))
    DBI::dbWriteTable(con, nm, get(nm))
  con
}
CATS <- c("A", "B", "C", "D", "E", "turtle", "F", "G")
A_USA <- 99.98
zrow <- function(r, zone_seq, cat) r$zone[r$zone$zone_seq == zone_seq & r$zone$sp_cat == cat, ]
ztax <- function(r, zone_seq, key) r$zone_taxon[r$zone_taxon$zone_seq == zone_seq & r$zone_taxon$ms_merge_key == key, ]
crow <- function(r, cat, cell) r$cell[r$cell$sp_cat == cat & r$cell$cell_id == cell, ]

# test -------------------------------------------------------------------------
con <- share_fixture_con()
on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
d <- DBI::dbGetQuery(con, denom_sql(nc = NC))
rownames(d) <- d$ms_merge_key
expect_equal(d["endemic", "total_global"], 10)
expect_equal(d["endemic", "source"], "merged_global")
expect_equal(d["bird",    "total_global"], 11)
expect_equal(d["bird", "n_cells_global"], 11L)
expect_equal(d["bird",    "n_cells_no_area"], 0L)
expect_equal(d["wide",    "total_global"], 99 + 1)
expect_equal(d["wide",  "source"], "aquamaps")
expect_equal(d["wide2",   "total_global"], 199 + 1)
expect_equal(d["wide2", "source"], "aquamaps")
expect_equal(d["dup",     "total_global"], 98 + 2)
expect_equal(d["dup", "total_usa"], 2)
expect_equal(d["hi",      "total_global"], 2)
expect_equal(d["hi", "source"], "aquamaps")
expect_equal(d["nat",     "total_global"], 10 / 0.25)
expect_equal(d["nat", "source"], "fallback:literature")
expect_equal(d["turtle",  "total_global"], 1.31)
expect_equal(d["turtle", "total_global_strip"], 1.30)
expect_equal(d["turtle",  "total_usa"], 0.81)
expect_equal(d["turtle", "total_usa_strip"], 0.80)
expect_true(d["dps", "source"] %in% c("merged_global", "aquamaps", "fallback:fraction"))
