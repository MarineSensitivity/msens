# Basemap

Basemap with Esri Ocean Basemap

## Usage

``` r
ms_basemap(base_opacity = 0.5)
```

## Arguments

- base_opacity:

  numeric between 0 and 1 (default=0.5)

## Value

[leaflet](https://rstudio.github.io/leaflet/reference/leaflet.html) map
object with Esri.OceanBasemap

## Examples

``` r
ms_basemap()

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.OceanBasemap",null,null,{"errorTileUrl":"","noWrap":false,"opacity":0.5,"detectRetina":false,"variant":"Ocean/World_Ocean_Base"}]},{"method":"addProviderTiles","args":["Esri.OceanBasemap",null,null,{"errorTileUrl":"","noWrap":false,"opacity":0.5,"detectRetina":false,"variant":"Ocean/World_Ocean_Reference"}]}]},"evals":[],"jsHooks":[]}
```
