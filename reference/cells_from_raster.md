# Resample a raster SDM onto the global 0.05° cell grid

For native raster SDMs on a *different* grid than the v8 0.05° (e.g.
NCCOS density COGs). Zero-fills absent cells within the source extent
then bilinear- resamples onto the cell grid (the same fade as AquaMaps)
and returns `(cell_id, value)` for cells at/above `min_value` — the
source's own values define coverage (no land mask; a marine SDM simply
has no land values). A source already on the v8 topology (AquaX,
Bio-Oracle) needs no resample — read it against `cellid_tif` directly.

## Usage

``` r
cells_from_raster(
  r,
  cellid_tif,
  method = "bilinear",
  min_value = 1,
  zero_fill = TRUE
)
```

## Arguments

- r:

  a `SpatRaster` (single layer) in EPSG:4326

- cellid_tif:

  path to the global cell-id COG

- method:

  resampling method (default "bilinear"; use "near" for categorical)

- min_value:

  drop resampled cells below this (default 1)

- zero_fill:

  zero-fill NA within the source extent before resampling so the surface
  fades to 0 at its edge (default TRUE)

## Value

a tibble `(cell_id integer, val double)`
