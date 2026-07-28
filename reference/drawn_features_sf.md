# Parse mapgl's drawn-features Shiny input into `sf`

`mapgl`'s draw control pushes its FeatureCollection to
`input$<map_id>_drawn_features`. **What lands there has changed shape
across mapgl versions**: older builds sent `JSON.stringify(fc)` (a
character string), current builds (`_mapglSyncDrawnFeatures`, mapgl \>=
0.5.0) send the object itself, which Shiny parses into a nested list. An
app that tests for only one of those forms silently sees "nothing drawn"
— which is exactly how the scores Report tab stopped recognizing drawn
polygons.

## Usage

``` r
drawn_features_sf(x)
```

## Arguments

- x:

  the raw value of `input$<map_id>_drawn_features`: a character GeoJSON
  string, a parsed list, or `NULL`

## Value

an `sf` data frame in EPSG:4326 with one row per drawn feature, or
`NULL` when nothing is drawn or the payload is unusable

## Details

This accepts BOTH forms so the apps cannot drift from mapgl again.

## Examples

``` r
drawn_features_sf(NULL)  # NULL — nothing drawn
#> NULL
```
