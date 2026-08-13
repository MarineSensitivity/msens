# registry_merge(): the native_asset registry may never silently lose an asset class.
#
# Regression origin: a publish_native run that skipped the PMTiles build rewrote
# native_asset from its own products alone, dropping all 2,234 vector-range rows while
# the tiles stayed published. The species app then offered AquaMaps as a taxon's only
# input, and the content-hash checkpoint recorded the loss as a clean run.

# one row per (ds_key, asset_type, representation) class, n times over
reg <- function(ds, type, rep, n, url = "u") data.frame(
  ds_key = rep(ds, n), asset_type = rep(type, n), representation = rep(rep, n),
  mdl_key = paste0(ds, "|", seq_len(n)), asset_url = url, stringsAsFactors = FALSE)

am_model  <- reg("am",       "cog",     "model",  3)
am_native <- reg("am",       "cog",     "native", 3)
merged    <- reg("ms_merge", "cog",     "model",  4)
iucn_pmt  <- reg("rng_iucn", "pmtiles", "native", 5)
bl_pmt    <- reg("bl",       "pmtiles", "native", 2)

test_that("a class the run did not build is carried forward, not deleted", {
  prior <- rbind(am_model, iucn_pmt)
  out   <- registry_merge(new = am_model, prior = prior)

  expect_equal(nrow(out), 8)                                   # 3 rebuilt + 5 preserved
  expect_equal(sum(out$ds_key == "rng_iucn"), 5)
  expect_equal(attr(out, "carried"), "rng_iucn | pmtiles | native")
  # carried rows are the prior rows verbatim
  expect_equal(out[out$ds_key == "rng_iucn", "mdl_key"], iucn_pmt$mdl_key)
})

test_that("EVERY skipped class is carried, not just the first", {
  prior <- rbind(am_model, iucn_pmt, bl_pmt, merged)
  out   <- registry_merge(new = am_model, prior = prior)

  expect_equal(nrow(out), 3 + 5 + 2 + 4)
  expect_setequal(attr(out, "carried"),
                  c("rng_iucn | pmtiles | native", "bl | pmtiles | native",
                    "ms_merge | cog | model"))
})

test_that("the exact regression: pmtiles skipped, am+merged rebuilt", {
  prior <- rbind(am_model, am_native, merged, iucn_pmt, bl_pmt)
  new   <- rbind(am_model, am_native, merged)                  # NATIVE_SKIP_PMTILES=1
  out   <- registry_merge(new = new, prior = prior)

  expect_equal(nrow(out), nrow(prior))
  expect_equal(sum(out$asset_type == "pmtiles"), 7)            # the rows that vanished
})

test_that("a class that came back SMALLER is refused, naming class and counts", {
  prior <- rbind(am_model, iucn_pmt)
  new   <- rbind(am_model, iucn_pmt[1:2, ])                    # partial rebuild

  expect_error(registry_merge(new, prior), "SHRINK")
  expect_error(registry_merge(new, prior), "rng_iucn \\| pmtiles \\| native: 5 -> 2")
  expect_error(registry_merge(new, prior), "NATIVE_REGISTRY_REBUILD=1")
})

test_that("shrink is reported for every affected class at once", {
  prior <- rbind(am_model, iucn_pmt, bl_pmt)
  new   <- rbind(am_model[1, ], iucn_pmt[1:2, ], bl_pmt)
  err   <- tryCatch(registry_merge(new, prior), error = conditionMessage)

  expect_match(err, "2 asset class\\(es\\)")
  expect_match(err, "am \\| cog \\| model: 3 -> 1")
  expect_match(err, "rng_iucn \\| pmtiles \\| native: 5 -> 2")
})

test_that("allow_shrink writes the run's products verbatim", {
  prior <- rbind(am_model, iucn_pmt)
  out   <- registry_merge(am_model, prior, allow_shrink = TRUE)

  expect_equal(nrow(out), 3)
  expect_equal(attr(out, "carried"), character(0))
  expect_false("rng_iucn" %in% out$ds_key)
})

test_that("growth and unchanged classes pass untouched", {
  prior <- rbind(am_model, iucn_pmt)
  new   <- rbind(reg("am", "cog", "model", 9), iucn_pmt)

  out <- registry_merge(new, prior)
  expect_equal(nrow(out), 14)
  expect_equal(attr(out, "carried"), character(0))
})

test_that("an absent or empty prior registry is a plain pass-through", {
  expect_equal(nrow(registry_merge(am_model, NULL)),          3)
  expect_equal(nrow(registry_merge(am_model, data.frame())),  3)
  # first-ever run: nothing built, nothing published
  expect_equal(nrow(registry_merge(data.frame(), NULL)),      0)
})

test_that("a run that builds NOTHING preserves the whole published registry", {
  prior <- rbind(am_model, iucn_pmt, merged)
  out   <- registry_merge(new = data.frame(), prior = prior)

  expect_equal(nrow(out), nrow(prior))
  expect_setequal(out$mdl_key, prior$mdl_key)
})

test_that("a carried class keeps its columns when the new build adds one", {
  prior <- rbind(am_model, iucn_pmt)                           # no bbox columns
  new   <- cbind(am_model, xmin = -10, xmax = 10)              # this run added bboxes

  out <- registry_merge(new, prior)
  expect_true(all(c("xmin", "xmax") %in% names(out)))
  expect_true(all(is.na(out$xmin[out$ds_key == "rng_iucn"])))  # carried rows have none
  expect_equal(out$xmin[out$ds_key == "am"], rep(-10, 3))
})

test_that("registry rows missing a key column are rejected, not silently classed", {
  bad <- am_model[setdiff(names(am_model), "representation")]
  expect_error(registry_merge(bad, am_model),  "missing registry key column")
  expect_error(registry_merge(am_model, bad),  "missing registry key column")
})
