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
