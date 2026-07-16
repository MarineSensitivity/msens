# Canonical global 0.5-degree cell index for lon/lat

A stable integer id for a point's cell in the global 0.5 degree grid
(`ext -180,180,-90,90`), so HCAF cells and per-species rasters on that
grid share one `src_i` key.

## Usage

``` r
cell_i_grid(lon, lat, res = 0.5)
```

## Arguments

- lon, lat:

  numeric vectors (EPSG:4326)

- res:

  grid resolution in degrees (default 0.5)

## Value

integer cell index (1-based, row-major from top-left)
