# Reconstructing taxon_model for releases that predate it (v1/v2) --------------
#
# v1/v2 record taxon->model edges WIDE: one integer column per dataset holding
# that dataset's contributing mdl_seq, plus `mdl_seq` for the merged model and
# `n_ds` for how many datasets contributed. v3+ store the same information LONG,
# as taxon_model(taxon_id, ds_key, mdl_seq).
#
# The reconstruction lives in backfill_versions.qmd (it is a one-off shape
# migration against a legacy schema, not reusable package logic), so this asserts
# the RULE on a synthetic fixture shaped exactly like v1's taxon table.

unpivot_taxon_model <- function(taxon, ds_cols) {
  long <- do.call(rbind, lapply(ds_cols, function(k) {
    v <- taxon[[k]]
    keep <- !is.na(v)
    if (!any(keep)) return(NULL)
    data.frame(taxon_id = taxon$taxon_id[keep], ds_key = k,
               mdl_seq = v[keep], stringsAsFactors = FALSE)
  }))
  merged <- taxon[!is.na(taxon$mdl_seq), ]
  rbind(long, data.frame(taxon_id = merged$taxon_id, ds_key = "ms_merge",
                         mdl_seq = merged$mdl_seq, stringsAsFactors = FALSE))
}

# shaped after real v1 rows (taxon 127186 contributes from 4 datasets)
tx <- data.frame(
  taxon_id  = c(105848L, 126505L, 127186L, 137085L),
  n_ds      = c(2L, 2L, 4L, 2L),
  `am_0.05` = c(7288L, 881L, 7466L, NA_integer_),
  bl        = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
  ch_fws    = c(NA_integer_, NA_integer_, 18309L, 18314L),
  ch_nmfs   = c(18229L, NA_integer_, 18230L, NA_integer_),
  rng_fws   = c(NA_integer_, 18365L, 18401L, 18416L),
  mdl_seq   = c(18441L, 18442L, 18422L, 18444L),
  check.names = FALSE, stringsAsFactors = FALSE)
DS <- c("am_0.05", "bl", "ch_fws", "ch_nmfs", "rng_fws")

test_that("the unpivot reproduces the v3 taxon_model shape", {
  tm <- unpivot_taxon_model(tx, DS)
  expect_named(tm, c("taxon_id", "ds_key", "mdl_seq"))
  expect_type(tm$taxon_id, "integer")
  expect_type(tm$mdl_seq, "integer")
  expect_type(tm$ds_key, "character")
})

test_that("per-dataset edge count equals n_ds — the independent check", {
  tm <- unpivot_taxon_model(tx, DS)
  # n_ds is recorded separately from the wide columns, so agreeing with it means
  # the unpivot neither dropped a dataset nor invented one
  expect_equal(sum(tm$ds_key != "ms_merge"), sum(tx$n_ds))
  per_taxon <- table(tm$ds_key[tm$ds_key != "ms_merge"][
    order(tm$taxon_id[tm$ds_key != "ms_merge"])])
  expect_equal(as.integer(table(tm$taxon_id[tm$ds_key != "ms_merge"])[
    as.character(tx$taxon_id)]), tx$n_ds)
})

test_that("NA means 'this dataset did not contribute', not an edge to model NA", {
  tm <- unpivot_taxon_model(tx, DS)
  expect_false(any(is.na(tm$mdl_seq)))
  # `bl` is entirely NA in this fixture, so it must not appear at all
  expect_false("bl" %in% tm$ds_key)
  # taxon 137085 has no AquaMaps model
  expect_false(any(tm$taxon_id == 137085L & tm$ds_key == "am_0.05"))
})

test_that("every taxon with a merged model gets exactly one ms_merge edge", {
  tm <- unpivot_taxon_model(tx, DS)
  m <- tm[tm$ds_key == "ms_merge", ]
  expect_equal(nrow(m), nrow(tx))
  expect_equal(sort(m$mdl_seq), sort(tx$mdl_seq))
  expect_false(any(duplicated(m$taxon_id)))
})
