# Changelog

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
