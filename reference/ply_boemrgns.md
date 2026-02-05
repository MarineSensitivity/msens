# Polygons of BOEM Regions

Polygons describing the BOEM Outer Continental Shelf (OCS) Regions [BOEM
Offshore Oil and Gas Planning
Areas](https://www.arcgis.com/home/item.html?id=576ae15675d747baaec607594fed086e)
Summarized to region name (key): Alaska (AK), Atlantic (ATL), Gulf of
Mexico (GOM), Pacific (PAC).

## Usage

``` r
ply_boemrgns
```

## Format

### `ply_boemrgns`

A spatial features sf data frame with 5 rows and 6 columns:

- boemrgn_key:

  key for BOEM Region

- boemrgn_name:

  name for BOEM Region

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
