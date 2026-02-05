# Polygons of BOEM Regions, simplified to 5%

Polygons describing the BOEM Outer Continental Shelf (OCS) Regions [BOEM
Offshore Oil and Gas Planning
Areas](https://www.arcgis.com/home/item.html?id=576ae15675d747baaec607594fed086e)
Summarized to region name (key): Alaska (AK), Atlantic (ATL), Gulf of
Mexico (GOM), Pacific (PAC). Simplified to 5% of original vertices for
quickly plotting.

## Usage

``` r
ply_boemrgns_s05
```

## Format

### `ply_boemrgns_s05`

A spatial features sf data frame with 4 rows and 6 columns:

- boemrgn_key:

  key for shelf polygon containing region

- boemrgn_name:

  name for shelf polygon containing region

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
