# Polygons of Ecoregions

BOEM Ecoregions, extended to the Planning Areas.

## Usage

``` r
ply_ecorgns
```

## Format

### `ply_ecorgns`

A spatial features sf data frame with 26 rows and 8 columns:

- ecorgn_key:

  key for Planning Area

- ecorgn_name:

  name for Planning Area

- geom:

  geometry of polygon as
  [`sf::st_geometry()`](https://r-spatial.github.io/sf/reference/st_geometry.html)

- ctr_lon:

  centroid longitude

- ctr_lat:

  centroid longitude

- area_km2:

  area in square kilometers
