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
  maxzoom = 6,
  simplification = 20,
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

  zoom range (default 0..6; coarse expert ranges don't need street-level
  detail, and high zoom on huge global polygons is very slow)

- simplification:

  tippecanoe `--simplification` (default 10; aggressive, since ranges
  are coarse — keeps tiles small without dropping species)

- tippecanoe:

  path to the tippecanoe binary (default `"tippecanoe"`)

- extra:

  extra tippecanoe CLI args (character vector)

- quiet:

  suppress tippecanoe stderr (default `TRUE`)

## Value

`out_pmtiles` (invisibly)
