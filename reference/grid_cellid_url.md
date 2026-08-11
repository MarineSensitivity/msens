# Public URL of a grid's cell-id COG

The cell-id lookup published beside the release data, so a client can
resolve "which cell is at this point?" through the same titiler
`/cog/point` call it uses for the layer value – no local raster, and the
id comes from the same authority the tiles do. Written INT4U with **no
overviews**: cell ids are categorical, and a resampled pyramid would
average them into ids that do not exist.

## Usage

``` r
grid_cellid_url(grid_id, base = atlas_base_url())
```

## Arguments

- grid_id:

  `usa05` or `global05`

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

## Value

an https URL
