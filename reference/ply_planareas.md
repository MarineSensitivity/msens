# Polygons of Planning Areas

BOEM Planning Areas from ([BOEM Offshore Oil and Gas Planning
Areas](https://www.arcgis.com/home/item.html?id=576ae15675d747baaec607594fed086e)).

## Usage

``` r
ply_planareas
```

## Format

### `ply_planareas`

A spatial features sf data frame with 26 rows and 8 columns:

- planarea_key:

  key for Planning Area

- planarea_name:

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

## Source

[BOEM Offshore Oil and Gas Planning
Areas](https://www.arcgis.com/home/item.html?id=576ae15675d747baaec607594fed086e)
