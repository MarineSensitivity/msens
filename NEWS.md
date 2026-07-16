# msens 0.5.0

The **v8 "Marine Atlas"** modeling + serving pass, part 2 — merge rules extracted to the package
(single source of truth) and the read/score API made to compose off one connection.

* **Merge rules as a single source of truth.** New `merge.R`: `merge_sql()` and `turtle_sql()`
  return the exact SQL for the two-surface merge (global viz `am ∪ range`; US-scoped, v7-faithful
  scoring surface with the AquaMaps no-EEZ constraint) and the multiplicative turtle merge. The
  `workflows` notebooks now *call* these and `test-merge.R` *asserts* them (one synthetic fixture per
  taxon category), so the notebook and the tests can never drift.
* **`attach_atlas()` now creates the table views** (via the new exported `atlas_views()`), mirroring
  the serving `serve.duckdb`, so the calc/score helpers that reference bare table names
  (`scores_for_pra()`, `species_for_cells()`, `scores_for_cells()`) compose directly — e.g.
  `scores_for_pra(attach_atlas(anon = TRUE), pra_key)` just works. `test-atlas.R` guards it.
* **Getting-started article + STAC alignment.** The `msens` article now walks attach → browse →
  search the STAC catalog → retrieve + map a whole-range COG → score a Program Area (flower plot) →
  species. The v8 STAC catalog reads cleanly in R (`rstac::read_stac()` for the static catalog — not
  `stac()`, which is for STAC *API* servers) and Python (`pystac`); `rstac` added to Suggests.
* `cells_from_ranges()` uses terra touches-rasterize as the fast default (keeping the `exact_extract`
  coverage option) — big speed-up on large ranges.
* `cell_tile_url()` / `cell_stats()` default `base` → the v8 `titiler-v8` factory (accepts
  `?mdl_key=`). The legacy v7 `titilecache` Varnish takes `?sql=` and 422s on `mdl_key`, which had
  left default-base callers (e.g. the article's map) with blank tiles.

# msens 0.4.0

The **v8 "Marine Atlas"** foundation: read the S3 Parquet release, ingest source models onto the
global 0.05° grid, publish native + gridded representations, and emit a STAC catalog.

* **Read the release.** `attach_atlas()` — canonical DuckDB reader for the marine-atlas Parquet on
  S3 (path-style, credential-chain), with `atlas_path()` / `atlas_tbl()` accessors.
* **Standardized Parquet + content-addressed change detection.** `write_atlas_parquet()` /
  `copy_atlas_parquet()` (Parquet V2, zstd, ~80 MB byte-sized row groups) behind a `require_duckdb()`
  version floor; `hash_parquet()` / `hash_query()` order-independent fingerprints +
  `write_manifest()` / `force_target()` for deterministic, timestamp-free manifests; `report_table()`
  / `report_parquet_summary()` for the notebook `## Outputs` sections.
* **Stable model id.** `mdl_key_raw()` / `mdl_key_merged()` build the `{ds_key}|{sp_id}` key that
  replaces the volatile `mdl_seq`; renamed the model-cell field `value` → `val` (SQL reserved word).
* **Ingest helpers.** `cells_from_ranges()` / `cells_from_raster()` / `cells_pct_marine()` rasterize
  a source model onto the global grid capturing the **whole** range (no land mask; `pct_marine`
  derived), via `exactextractr`. `clean_sci_name()` for taxonomic matching.
* **Native + gridded publishing.** `publish_cog()` (COG with overviews), `publish_pmtiles()` /
  `publish_pmtiles_models()` (per-model PMTiles), and `cog_tile_url()` for titiler `/cog` tiles.
* **STAC v8** (`stac.R`): `stac_build()` and the collection/item generators emit both `native` and
  `model` representations per dataset on `model_cell` Items, keyed on the stable `mdl_key`.
* **Pipeline generator.** `build_targets_list()` parses the `msens:` front-matter of the workflow
  `*.qmd` into a `targets` list. `pra_score_delta()` is the version-equivalence gate.
* `hex.R` + `interp.R` are marked **DORMANT** — v8 rolled back from an H3 grid to the 0.05° cell grid.

# msens 0.3.4

* `cell_tile_url()` gains a `color` argument for single-color mask tiles:
  when set to a hex string (e.g. `"#222222"`), the URL uses the msens
  TiTiler factory's `color=` query param, which renders every valid pixel
  in that flat RGBA color and ignores `colormap` / `rescale`. Used by the
  mapgl app's "Cells outside Program Areas" overlay — replaces the old
  `msens::add_cells(r_outside_pra, colors = c("#222222","#222222"), ...)`
  pattern that shipped a terra raster as a base64 image source.

# msens 0.3.3

* Added `add_cell_tiles()`, `cell_tile_url()`, `cell_stats()` for TiTiler 
  endpoint to support mapgl app.

# msens 0.3.2

* Added `cells_in_pra()` and `scores_for_pra()` — fast Program Area
  lookups that read from `zone` / `zone_cell` / `zone_metric` instead
  of rasterizing the polygon and aggregating across cells. Same
  output shape as `cells_in_polygon()` / `scores_for_cells()` so
  they're drop-in replacements when the area is a Program Area.

# msens 0.3.1

* Pin `mapgl (>= 0.4.5.9000)` and add `Remotes: walkerke/mapgl` so
  `install_github()` pulls the dev build that exports `add_pmtiles_source()`
  (needed by `add_pmfill()` / `add_pmline()`). Fixes a silent install
  failure on fresh environments where the pinned CRAN snapshot still
  served `mapgl 0.1.3`.

# msens 0.3.0

* Added score-calculation helpers migrated out of the mapgl app so they can be
  reused by the report and API: `cells_in_polygon()`, `scores_for_cells()`,
  `species_for_cells()`, `mean_score()` and `cell_id_raster()` (new `R/calc.R`).
* Added visualization helpers for multi-format (html / pdf / docx) reports:
  `ggplot_flower()` and `ggmap_areas()`. `plot_flower()` and `tbl_species()`
  gained an `interactive=` argument so they can emit static output for
  non-html Quarto formats.
* `sdm_db_path()` / `sdm_db_con()` now default to the v6 species-distribution
  database.

# msens 0.2.1

* Added mapping functions for use in docs and map apps, including raster cells 
  (`add_cells()`), and using PMtiles as vectors sources to add fills (`add_pmfill()`, 
  eg for Program Area scores), outlines (`add_pmline()`, eg for Ecoregion outlines),
  and labels (`add_pmlabel()`, eg for Ecoregion names and Program Area acronymns).

# msens 0.2.0

* Swapped polygons:
  - OLD: hierarchy `ply_shlfs` > `ply_rgns` (and `*_s05`
  simplifications) that were clipped to US EEZ.
  - NEW: hierarchy `ply_boemrgns` > `ply_ecorgns` | `ply_planareas` > 
  `ply_ecoareas`, which are the intersection of `ply_ecorgns` and `ply_planareas`.
  Created `*_s05` simplifications of each. The new polygons conform to BOEM's
  original nomenclature for "OCS Regions", Planning Areas" and "Ecoregions". 
  These polygons are not clipped to the US EEZ.

# msens 0.1.2

* Added `get_species_by_feature()` to read from API endpoint.

# msens 0.1.1

* Added simple `ms_basemap()` to support map app.

# msens 0.1

* Added [`data`](../reference/index.html#data) basic Outer Continental Shelf (OCS) regions `ply_shlfs` and BOEM Planning Regions `ply_rgns` with simplified to 5% variants (`ply_shlfs_s05`, `ply_rgns_s05`).
