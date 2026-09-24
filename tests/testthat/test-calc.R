# The three scoring inconsistencies (master-plan D7 / D7b), each with the name of
# the inconsistency it fixes. `atlas-refs/report pipeline spec.md` §3.6 recorded
# them; these tests are what stops them coming back.

gens <- c("v9", "v7", "v7b", "v2")

# zone A of the synthetic release: 4 cells at pct 100,100,100,50 (sum 350);
# bird on all 4 at 50, turtle on ONE at 80, primprod on two at 20
A_BLEND <- c(bird = 17500 / 350, primprod = 4000 / 350, turtle = 8000 / 350)
A_OLD   <- c(bird = 50,          primprod = 20,         turtle = 80)

test_that("REGRESSION 1: scores_for_cells blends absent values as zero (the sliver no longer reads high)", {
  # The bug: an INNER JOIN on cell_metric removed a cell without the metric from
  # the DENOMINATOR too, so a component covering 28.6 % of a place scored as if it
  # covered all of it. Measured on real v9: up to +49.1 on turtle, +6.28 composite.
  for (gen in gens) with_synth(gen, function(con) {
    cells <- cells_in_pra(con, "AAA")

    d <- scores_for_cells(con, cells, denominator = "all")
    s <- stats::setNames(d$score, d$component)
    expect_equal(s[names(A_BLEND)], A_BLEND, info = gen)

    old <- scores_for_cells(con, cells, blend = FALSE, denominator = "all")
    so  <- stats::setNames(old$score, old$component)
    expect_equal(so[names(A_OLD)], A_OLD, info = gen)

    # and the blend is exactly the published zone_metric, which is the whole claim
    pub <- scores_for_pra(con, "AAA")
    sp  <- stats::setNames(pub$score, pub$component)
    expect_equal(s[sort(names(sp))], sp[sort(names(sp))], info = gen)
    # the old formula was NOT the published number
    expect_false(isTRUE(all.equal(so[["turtle"]], sp[["turtle"]])), info = gen)
  })
})

test_that("REGRESSION 1b: coverage and mean_where_present factor the score", {
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- scores_for_cells(con, cells_in_pra(con, "AAA"), denominator = "all")
  r <- d[d$component == "turtle", ]
  expect_equal(r$coverage, 100 / 350)
  expect_equal(r$mean_where_present, 80)
  expect_equal(r$score, r$coverage * r$mean_where_present)
})

test_that("REGRESSION 1c: a component with no covered cell yields NO row, never a zero", {
  # zone B has no turtle cell. A zero would drag the composite down; absence is
  # what the published composite (a plain mean over present rows) assumes.
  for (gen in gens) with_synth(gen, function(con) {
    d <- scores_for_cells(con, cells_in_pra(con, "BBB"), denominator = "all")
    expect_false("turtle" %in% d$component, info = gen)
    # primprod has no cell in zone B either: also absent, also not a zero
    expect_identical(sort(d$component), "bird", info = gen)
  })
})

test_that("REGRESSION 1d: reportability is the ABSENCE of the rescaled row, not the prepctareaweighting row", {
  # v7.1 deletes an unreportable component's `_ecoregion_rescaled` row and leaves
  # its `_prepctareaweighting` row behind. Reading the wrong one reports a dropped
  # component as reportable with a stale number.
  con <- synth_release("v7b"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  pub <- scores_for_pra(con, "BBB")
  expect_false("turtle" %in% pub$component)

  stale <- DBI::dbGetQuery(con, "
    SELECT m.metric_key FROM zone zn
      JOIN zone_metric zm USING (zone_seq)
      JOIN metric m       USING (metric_seq)
     WHERE zn.value = 'BBB' AND m.metric_key LIKE '%turtle%'")
  expect_equal(stale$metric_key, "extrisk_turtle_ecoregion_rescaled_prepctareaweighting")
})

test_that("REGRESSION 2: cells_in_pra returns the real pct_covered, not a hardcoded 100", {
  # The bug: `mutate(pct_covered = 100L)` overwrote zone_cell's stored coverage
  # (74,938 of 2,241,876 rows are partial), so a report weighted its PRA scores by
  # coverage and its species table of the same area uniformly.
  for (gen in gens) with_synth(gen, function(con) {
    d <- cells_in_pra(con, "AAA")
    expect_equal(nrow(d), 4, info = gen)
    expect_equal(sort(d$pct_covered), c(50, 100, 100, 100), info = gen)
    expect_false(all(d$pct_covered == 100), info = gen)
    expect_equal(sum(d$pct_covered), 350, info = gen)
  })
})

test_that("REGRESSION 2b: the zone key column is resolved, not hardcoded to `value`", {
  # v8/v9 SOURCE databases spell it `val`; only v1-v7 and the served views have
  # `value`. Hardcoding it made cells_in_pra()/scores_for_pra() error on v9.
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_identical(sdm_val_col(con, "zone"), "val")
  expect_equal(nrow(cells_in_pra(con, "AAA")), 4)
  expect_gt(nrow(scores_for_pra(con, "AAA")), 0)
})

test_that("REGRESSION 3: ONE clipped cell set drives scores, species, area and N cells", {
  # The bug: in-report PRA species came from cells_in_pra(), the CSV download
  # re-derived a third set from the GeoPackage via cells_in_polygon(), and the
  # scores came from a fourth (zone_metric). Now one function produces the set and
  # everything consumes it.
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  raw <- cells_in_pra(con, "AAA")
  one <- cells_in_study_area(con, raw)

  # cell 4 is in_usa = FALSE: the clip removes it from the ONE set
  expect_equal(nrow(one), 3)
  expect_false(any(one$cell_id %in% raw$cell_id[raw$pct_covered == 50]))

  # scores computed on the clipped set agree with computing them from `one`
  auto <- scores_for_cells(con, raw)                       # clips internally
  hand <- scores_for_cells(con, one, denominator = "all")  # already clipped
  expect_equal(auto$score, hand$score)
  expect_equal(auto$metric_key, hand$metric_key)

  # the species table reads the SAME set, so N cells and area cannot disagree
  spp <- species_for_cells(con, one)
  expect_true(all(c("sp_scientific", "area_km2", "avg_suit") %in% names(spp)))
  expect_equal(sum(one$pct_covered), 300)
})

test_that("clipping to the study area is what stops land entering as a zero", {
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cells <- cells_in_pra(con, "AAA")
  all_   <- scores_for_cells(con, cells, denominator = "all")
  clip   <- scores_for_cells(con, cells, denominator = "study_area")
  gs <- function(d, k) d$score[d$component == k]

  expect_equal(gs(all_,  "turtle"), 8000 / 350)   # zone parity
  expect_equal(gs(clip,  "turtle"), 8000 / 300)   # the foreign cell's zero is gone
  expect_gt(gs(clip, "turtle"), gs(all_, "turtle"))
  expect_equal(gs(clip, "bird"), 50)              # unchanged: bird covers everything
})

test_that("the metric filter excludes _prepctareaweighting, _min and _max", {
  for (gen in gens) with_synth(gen, function(con) {
    d <- scores_for_cells(con, cells_in_pra(con, "AAA"), denominator = "all")
    expect_false(any(grepl("_prepctareaweighting$|_ecoregion_(min|max)$|_coverage$",
                           d$metric_key)), info = gen)
    expect_setequal(d$component, c("bird", "turtle", "primprod"))
  })
})

test_that("mean_score over the blended components is the published composite rule", {
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- scores_for_cells(con, cells_in_pra(con, "AAA"), denominator = "all")
  expect_equal(mean_score(d), mean(A_BLEND))
})

test_that("an empty cell set returns the right SHAPE, not an error", {
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- scores_for_cells(con, tibble::tibble(cell_id = integer(), pct_covered = numeric()))
  expect_equal(nrow(d), 0)
  expect_named(d, c("metric_key", "score", "component", "even",
                    "coverage", "mean_where_present"))
})

test_that("a drawn polygon tracing a zone reproduces that zone's published scores", {
  # the whole point of D7: the same ground, drawn or picked, gives the same numbers
  con <- synth_release("v9"); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # zone A is the top row of the block, minus the half-covered 4th cell's outer half
  ids <- synth_cell_ids("global05")[1:3]
  ll  <- cell_lonlat(ids, grid_spec_for("global05"))
  poly <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(min(ll$lon) - 0.025, max(ll$lon) + 0.025, max(ll$lon) + 0.025,
      min(ll$lon) - 0.025, min(ll$lon) - 0.025),
    c(rep(min(ll$lat) - 0.025, 2), rep(max(ll$lat) + 0.025, 2), min(ll$lat) - 0.025)))),
    crs = 4326)

  drawn <- cells_in_polygon(poly, con)
  expect_setequal(drawn$cell_id, ids)
  expect_true(all(drawn$pct_covered == 100))

  d <- scores_for_cells(con, drawn)
  s <- stats::setNames(d$score, d$component)
  # 3 cells at pct 100: bird 50, turtle 80/3, primprod 40/3
  expect_equal(unname(s[["bird"]]),     50)
  expect_equal(unname(s[["turtle"]]),   80 / 3)
  expect_equal(unname(s[["primprod"]]), 40 / 3)
})

# A GEO-shaped case: the coverage where the two formulas diverge most ------------
#
# REGRESSION 1 above pins turtle at 100/350 = 28.6 % coverage, which is a normal
# Program Area, not the pathological one. The case that actually broke reports is
# St George Basin (GEO), where turtle covers 1.41 % of the area: published 0.70,
# old formula 49.85. `inst/gates/pa_tracing.R` asserts that on the real release;
# this reproduces the SHAPE in milliseconds, so the rule is pinned even on a
# machine with no 29 GB database.

synth_sliver <- function(n_cells = 100L, n_with = 1L, val = 80) {
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = tempfile("synth_sliver_", fileext = ".duckdb"))
  id <- seq_len(n_cells)
  DBI::dbWriteTable(con, "cell", data.frame(
    cell_id = id, lon = 0, lat = 0, area_km2 = 25, in_usa = TRUE))
  DBI::dbWriteTable(con, "metric", data.frame(
    metric_seq = 1:2,
    metric_key = c("extrisk_turtle_ecoregion_rescaled",
                   "extrisk_bird_ecoregion_rescaled"),
    description = "m"))
  DBI::dbWriteTable(con, "cell_metric", rbind(
    data.frame(cell_id = id[seq_len(n_with)], metric_seq = 1L, val = val),
    data.frame(cell_id = id,                  metric_seq = 2L, val = 50)))
  DBI::dbWriteTable(con, "zone", data.frame(
    zone_seq = 1L, tbl = "z", fld = "programarea_key", val = "GEOISH"))
  DBI::dbWriteTable(con, "zone_cell", data.frame(
    zone_seq = 1L, cell_id = id, pct_covered = 100))
  # the PUBLISHED value, by the identity: sum(coalesce(val,0)*pct)/sum(pct)
  DBI::dbWriteTable(con, "zone_metric", data.frame(
    zone_seq = 1L, metric_seq = 1:2,
    val = c(val * n_with / n_cells, 50)))
  con
}

test_that("REGRESSION 1e: a ~1 % coverage component reads 99x high under the old formula", {
  con <- synth_sliver(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  cells <- cells_in_pra(con, "GEOISH")
  expect_equal(nrow(cells), 100)

  d <- scores_for_cells(con, cells, denominator = "all")
  t <- d[d$component == "turtle", ]
  expect_equal(t$coverage, 0.01)                 # the GEO shape: ~1 % covered
  expect_equal(t$mean_where_present, 80)
  expect_equal(t$score, 0.8)                     # blended: 80 * 0.01

  # ...and that IS the published number, to the last bit
  pub <- scores_for_pra(con, "GEOISH")
  expect_equal(t$score, pub$score[pub$component == "turtle"])

  # the old formula says 80 where the release published 0.8 — off by 79.2, which is
  # the synthetic twin of GEO's measured 49.14
  old <- scores_for_cells(con, cells, blend = FALSE, denominator = "all")
  o <- old$score[old$component == "turtle"]
  expect_equal(o, 80)
  expect_equal(o - t$score, 79.2)
  expect_gt(o - t$score, 0.5)                    # far outside any report tolerance

  # a fully covered component in the SAME area is untouched, so the blend is not
  # simply shrinking everything
  expect_equal(d$score[d$component == "bird"], 50)
  expect_equal(old$score[old$component == "bird"], 50)
})
