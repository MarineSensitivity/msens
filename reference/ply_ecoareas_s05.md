# Polygons of EcoAreas

BOEM EcoAreas, which represent the intersection of Ecoregions and
Planning Areas. Simplified to 5% of original vertices for quickly
plotting.

## Usage

``` r
ply_ecoareas_s05
```

## Format

### `ply_ecoareas_s05`

A spatial features sf data frame with 26 rows and 8 columns:

- ecoarea_key:

  key for Planning Area

- ecoarea_name:

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
