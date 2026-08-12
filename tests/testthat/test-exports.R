# The public surface, asserted -------------------------------------------------
#
# Inserting a function (with its own roxygen) BETWEEN an existing roxygen block
# and the function it documents silently reassigns that block -- the original
# function loses its @export and vanishes from NAMESPACE. Nothing fails at build
# or test time; it surfaces later as
#   'mdl_key_raw' is not an exported object from 'namespace:msens'
# from whichever notebook happens to call it first. That is exactly how a
# backfill render died mid-run after the package "built fine".
#
# Cheap insurance: name the functions other repos call across the package
# boundary, and fail here rather than in a two-hour pipeline.

test_that("functions the workflows/apps call are actually exported", {
  exported <- getNamespaceExports("msens")
  expect_true(all(c(
    # model identity
    "mdl_key_raw", "mdl_key_merged", "mdl_key_parse", "normalize_ds_key",
    "assign_mdl_id", "dataset_is_scored",
    # cross-version schema resolution (apps AND the versioned docs)
    "sdm_cols",
    # version + manifest registry
    "atlas_base_url", "atlas_latest", "atlas_versions", "atlas_resolve_ver",
    "atlas_manifest", "validate_manifest", "manifest_build", "manifest_can",
    # grid registry
    "grid_registry", "grid_for_ver", "grid_spec_for", "cell_lonlat",
    "cell_from_lonlat", "grid_cellid_url",
    # zone sets
    "zone_set_key", "zone_geom_hash", "zone_cells", "zone_set_resolve",
    "validate_zone_sets",
    # content-addressed store + publishing
    "content_hashes", "content_key", "content_url", "cog_store_index",
    "publish_cog", "publish_pmtiles",
    # serving helpers the apps use
    "cog_tile_url", "cog_point_value", "add_cell_tiles", "version_picker_html"
  ) %in% exported))
})

test_that("every .R file's roxygen documents the function directly beneath it", {
  # catches the orphaned-block bug at its source: an @export block must be
  # immediately followed by an assignment, not by another roxygen block
  for (f in list.files("../../R", pattern = "\\.R$", full.names = TRUE)) {
    ln <- readLines(f, warn = FALSE)
    ex <- grep("^#'\\s*@export\\s*$", ln)
    for (i in ex) {
      # scan to the first line that is neither roxygen nor blank -- NOT a fixed
      # window: an @examples block can run for dozens of lines, and a short window
      # reports every such function as orphaned (my first version did exactly that
      # for worms.R, which is perfectly fine)
      j <- i + 1
      while (j <= length(ln) && (grepl("^#'", ln[j]) || !nzchar(trimws(ln[j])))) j <- j + 1
      body_start <- if (j <= length(ln)) ln[j] else NA_character_
      expect_true(
        !is.na(body_start) && grepl("(<-|=)\\s*function|^`|^[A-Za-z._]+\\s*<-", body_start),
        info = sprintf("%s:%d — @export not followed by a definition (orphaned roxygen?)",
                       basename(f), i))
    }
  }
})
