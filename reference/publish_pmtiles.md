# Publish vector features as PMTiles (tippecanoe)

Writes an sf (or an existing vector file) to PMTiles via tippecanoe,
keeping every feature (no dropping) so a source dataset's ranges can be
filtered client-side by an attribute (e.g. `mdl_key`). Reprojects to
EPSG:4326 first.

## Usage

``` r
publish_pmtiles(
  x,
  out_pmtiles,
  layer,
  minzoom = 0,
  maxzoom = 10,
  simplification = 10,
  keep_attrs = c("mdl_key", "ds_key"),
  tippecanoe = "tippecanoe",
  extra = character(0),
  quiet = TRUE
)
```

## Arguments

- x:

  an sf object, or a path to a vector file tippecanoe can read
  (FlatGeobuf / GeoJSON)

- out_pmtiles:

  output path (`.pmtiles`)

- layer:

  tile layer name (the `source_layer` the app references)

- minzoom, maxzoom:

  zoom range (default 0..10). Simplification is applied at the LOW zooms
  only, so maxzoom carries full source resolution and every view above
  it overzooms from something faithful.

- simplification:

  tippecanoe `--simplification` for the low zooms only

- keep_attrs:

  attributes to carry into the tiles (`-y`); the zone layers need their
  own key columns, not `mdl_key`/`ds_key`

- tippecanoe:

  path to the tippecanoe binary (default `"tippecanoe"`)

- extra:

  extra tippecanoe CLI args (character vector)

- quiet:

  suppress tippecanoe stderr (default `TRUE`)

## Value

`out_pmtiles` (invisibly)
