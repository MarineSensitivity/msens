# Cell id at a longitude/latitude (inverse of [`cell_lonlat()`](http://marinesensitivity.org/msens/reference/cell_lonlat.md))

Pure arithmetic on the grid definition – no raster read. The cell-id
COGs are lookup IMAGES whose band name and frame vary by grid, so
resolving a click by reading one is both slower and a source of bugs:
selecting the band by name fails (usa05 calls it `r_cellid`, global05
`depth_mean`), and shifting a click to 0-360 lands outside a raster
stored in -180..180. The grid registry already defines the mapping
exactly.

## Usage

``` r
cell_from_lonlat(lon, lat, grid)
```

## Arguments

- lon, lat:

  coordinates in degrees, `lon` in -180..180

- grid:

  grid spec from
  [`grid_spec_for()`](http://marinesensitivity.org/msens/reference/grid_spec_for.md)

## Value

cell id, or `NA` where the point falls outside the grid
