# Changelog

## msens 0.5.0

The **v8 “Marine Atlas”** modeling + serving pass, part 2 — merge rules
extracted to the package (single source of truth) and the read/score API
made to compose off one connection.

- **Merge rules as a single source of truth.** New `merge.R`:
  [`merge_sql()`](http://marinesensitivity.org/msens/reference/merge_sql.md)
  and
  [`turtle_sql()`](http://marinesensitivity.org/msens/reference/turtle_sql.md)
  return the exact SQL for the two-surface merge (global viz
  `am ∪ range`; US-scoped, v7-faithful scoring surface with the AquaMaps
  no-EEZ constraint) and the multiplicative turtle merge. The
  `workflows` notebooks now *call* these and `test-merge.R` *asserts*
  them (one synthetic fixture per taxon category), so the notebook and
  the tests can never drift.
- **[`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
  now creates the table views** (via the new exported
  [`atlas_views()`](http://marinesensitivity.org/msens/reference/atlas_views.md)),
  mirroring the serving `serve.duckdb`, so the calc/score helpers that
  reference bare table names
  ([`scores_for_pra()`](http://marinesensitivity.org/msens/reference/scores_for_pra.md),
  [`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md),
  [`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md))
  compose directly — e.g.
  `scores_for_pra(attach_atlas(anon = TRUE), pra_key)` just works.
  `test-atlas.R` guards it.
- **Getting-started article + STAC alignment.** The `msens` article now
  walks attach → browse → search the STAC catalog → retrieve + map a
  whole-range COG → score a Program Area (flower plot) → species. The v8
  STAC catalog reads cleanly in R
  ([`rstac::read_stac()`](https://brazil-data-cube.github.io/rstac/reference/static_functions.html)
  for the static catalog — not
  [`stac()`](https://brazil-data-cube.github.io/rstac/reference/stac.html),
  which is for STAC *API* servers) and Python (`pystac`); `rstac` added
  to Suggests.
- [`cells_from_ranges()`](http://marinesensitivity.org/msens/reference/cells_from_ranges.md)
  uses terra touches-rasterize as the fast default (keeping the
  `exact_extract` coverage option) — big speed-up on large ranges.
- [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
  /
  [`cell_stats()`](http://marinesensitivity.org/msens/reference/cell_stats.md)
  default `base` → the v8 `titiler-v8` factory (accepts `?mdl_key=`).
  The legacy v7 `titilecache` Varnish takes `?sql=` and 422s on
  `mdl_key`, which had left default-base callers (e.g. the article’s
  map) with blank tiles.

## msens 0.4.0

The **v8 “Marine Atlas”** foundation: read the S3 Parquet release,
ingest source models onto the global 0.05° grid, publish native +
gridded representations, and emit a STAC catalog.

- **Read the release.**
  [`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
  — canonical DuckDB reader for the marine-atlas Parquet on S3
  (path-style, credential-chain), with
  [`atlas_path()`](http://marinesensitivity.org/msens/reference/atlas_path.md)
  /
  [`atlas_tbl()`](http://marinesensitivity.org/msens/reference/atlas_tbl.md)
  accessors.
- **Standardized Parquet + content-addressed change detection.**
  [`write_atlas_parquet()`](http://marinesensitivity.org/msens/reference/write_atlas_parquet.md)
  /
  [`copy_atlas_parquet()`](http://marinesensitivity.org/msens/reference/copy_atlas_parquet.md)
  (Parquet V2, zstd, ~80 MB byte-sized row groups) behind a
  [`require_duckdb()`](http://marinesensitivity.org/msens/reference/require_duckdb.md)
  version floor;
  [`hash_parquet()`](http://marinesensitivity.org/msens/reference/hash_parquet.md)
  /
  [`hash_query()`](http://marinesensitivity.org/msens/reference/hash_query.md)
  order-independent fingerprints +
  [`write_manifest()`](http://marinesensitivity.org/msens/reference/write_manifest.md)
  /
  [`force_target()`](http://marinesensitivity.org/msens/reference/force_target.md)
  for deterministic, timestamp-free manifests;
  [`report_table()`](http://marinesensitivity.org/msens/reference/report_table.md)
  /
  [`report_parquet_summary()`](http://marinesensitivity.org/msens/reference/report_parquet_summary.md)
  for the notebook `## Outputs` sections.
- **Stable model id.**
  [`mdl_key_raw()`](http://marinesensitivity.org/msens/reference/mdl_key_raw.md)
  /
  [`mdl_key_merged()`](http://marinesensitivity.org/msens/reference/mdl_key_merged.md)
  build the `{ds_key}|{sp_id}` key that replaces the volatile `mdl_seq`;
  renamed the model-cell field `value` → `val` (SQL reserved word).
- **Ingest helpers.**
  [`cells_from_ranges()`](http://marinesensitivity.org/msens/reference/cells_from_ranges.md)
  /
  [`cells_from_raster()`](http://marinesensitivity.org/msens/reference/cells_from_raster.md)
  /
  [`cells_pct_marine()`](http://marinesensitivity.org/msens/reference/cells_pct_marine.md)
  rasterize a source model onto the global grid capturing the **whole**
  range (no land mask; `pct_marine` derived), via `exactextractr`.
  [`clean_sci_name()`](http://marinesensitivity.org/msens/reference/clean_sci_name.md)
  for taxonomic matching.
- **Native + gridded publishing.**
  [`publish_cog()`](http://marinesensitivity.org/msens/reference/publish_cog.md)
  (COG with overviews),
  [`publish_pmtiles()`](http://marinesensitivity.org/msens/reference/publish_pmtiles.md)
  /
  [`publish_pmtiles_models()`](http://marinesensitivity.org/msens/reference/publish_pmtiles_models.md)
  (per-model PMTiles), and
  [`cog_tile_url()`](http://marinesensitivity.org/msens/reference/cog_tile_url.md)
  for titiler `/cog` tiles.
- **STAC v8** (`stac.R`):
  [`stac_build()`](http://marinesensitivity.org/msens/reference/stac_build.md)
  and the collection/item generators emit both `native` and `model`
  representations per dataset on `model_cell` Items, keyed on the stable
  `mdl_key`.
- **Pipeline generator.**
  [`build_targets_list()`](http://marinesensitivity.org/msens/reference/build_targets_list.md)
  parses the `msens:` front-matter of the workflow `*.qmd` into a
  `targets` list.
  [`pra_score_delta()`](http://marinesensitivity.org/msens/reference/pra_score_delta.md)
  is the version-equivalence gate.
- `hex.R` + `interp.R` are marked **DORMANT** — v8 rolled back from an
  H3 grid to the 0.05° cell grid.

## msens 0.3.4

- [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
  gains a `color` argument for single-color mask tiles: when set to a
  hex string (e.g. `"#222222"`), the URL uses the msens TiTiler
  factory’s `color=` query param, which renders every valid pixel in
  that flat RGBA color and ignores `colormap` / `rescale`. Used by the
  mapgl app’s “Cells outside Program Areas” overlay — replaces the old
  `msens::add_cells(r_outside_pra, colors = c("#222222","#222222"), ...)`
  pattern that shipped a terra raster as a base64 image source.

## msens 0.3.3

- Added
  [`add_cell_tiles()`](http://marinesensitivity.org/msens/reference/add_cell_tiles.md),
  [`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md),
  [`cell_stats()`](http://marinesensitivity.org/msens/reference/cell_stats.md)
  for TiTiler endpoint to support mapgl app.

## msens 0.3.2

- Added
  [`cells_in_pra()`](http://marinesensitivity.org/msens/reference/cells_in_pra.md)
  and
  [`scores_for_pra()`](http://marinesensitivity.org/msens/reference/scores_for_pra.md)
  — fast Program Area lookups that read from `zone` / `zone_cell` /
  `zone_metric` instead of rasterizing the polygon and aggregating
  across cells. Same output shape as
  [`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md)
  /
  [`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md)
  so they’re drop-in replacements when the area is a Program Area.

## msens 0.3.1

- Pin `mapgl (>= 0.4.5.9000)` and add `Remotes: walkerke/mapgl` so
  `install_github()` pulls the dev build that exports
  `add_pmtiles_source()` (needed by
  [`add_pmfill()`](http://marinesensitivity.org/msens/reference/add_pmfill.md)
  /
  [`add_pmline()`](http://marinesensitivity.org/msens/reference/add_pmline.md)).
  Fixes a silent install failure on fresh environments where the pinned
  CRAN snapshot still served `mapgl 0.1.3`.

## msens 0.3.0

- Added score-calculation helpers migrated out of the mapgl app so they
  can be reused by the report and API:
  [`cells_in_polygon()`](http://marinesensitivity.org/msens/reference/cells_in_polygon.md),
  [`scores_for_cells()`](http://marinesensitivity.org/msens/reference/scores_for_cells.md),
  [`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md),
  [`mean_score()`](http://marinesensitivity.org/msens/reference/mean_score.md)
  and
  [`cell_id_raster()`](http://marinesensitivity.org/msens/reference/cell_id_raster.md)
  (new `R/calc.R`).
- Added visualization helpers for multi-format (html / pdf / docx)
  reports:
  [`ggplot_flower()`](http://marinesensitivity.org/msens/reference/ggplot_flower.md)
  and
  [`ggmap_areas()`](http://marinesensitivity.org/msens/reference/ggmap_areas.md).
  [`plot_flower()`](http://marinesensitivity.org/msens/reference/plot_flower.md)
  and
  [`tbl_species()`](http://marinesensitivity.org/msens/reference/tbl_species.md)
  gained an `interactive=` argument so they can emit static output for
  non-html Quarto formats.
- [`sdm_db_path()`](http://marinesensitivity.org/msens/reference/sdm_db_path.md)
  /
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md)
  now default to the v6 species-distribution database.

## msens 0.2.1

- Added mapping functions for use in docs and map apps, including raster
  cells
  ([`add_cells()`](http://marinesensitivity.org/msens/reference/add_cells.md)),
  and using PMtiles as vectors sources to add fills
  ([`add_pmfill()`](http://marinesensitivity.org/msens/reference/add_pmfill.md),
  eg for Program Area scores), outlines
  ([`add_pmline()`](http://marinesensitivity.org/msens/reference/add_pmline.md),
  eg for Ecoregion outlines), and labels
  ([`add_pmlabel()`](http://marinesensitivity.org/msens/reference/add_pmlabel.md),
  eg for Ecoregion names and Program Area acronymns).

## msens 0.2.0

- Swapped polygons:
  - OLD: hierarchy `ply_shlfs` \> `ply_rgns` (and `*_s05`
    simplifications) that were clipped to US EEZ.
  - NEW: hierarchy `ply_boemrgns` \> `ply_ecorgns` \| `ply_planareas` \>
    `ply_ecoareas`, which are the intersection of `ply_ecorgns` and
    `ply_planareas`. Created `*_s05` simplifications of each. The new
    polygons conform to BOEM’s original nomenclature for “OCS Regions”,
    Planning Areas” and “Ecoregions”. These polygons are not clipped to
    the US EEZ.

## msens 0.1.2

- Added
  [`get_species_by_feature()`](http://marinesensitivity.org/msens/reference/get_species_by_feature.md)
  to read from API endpoint.

## msens 0.1.1

- Added simple
  [`ms_basemap()`](http://marinesensitivity.org/msens/reference/ms_basemap.md)
  to support map app.

## msens 0.1

- Added
  [`data`](http://marinesensitivity.org/msens/reference/index.html#data)
  basic Outer Continental Shelf (OCS) regions `ply_shlfs` and BOEM
  Planning Regions `ply_rgns` with simplified to 5% variants
  (`ply_shlfs_s05`, `ply_rgns_s05`).
