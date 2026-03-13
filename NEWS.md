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
