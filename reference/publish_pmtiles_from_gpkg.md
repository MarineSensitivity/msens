# Publish per-model PMTiles from an indexed GeoPackage

Like
[`publish_pmtiles_models()`](http://marinesensitivity.org/msens/reference/publish_pmtiles_models.md)
but reads each model's features **on demand** from an indexed GeoPackage
(`SELECT ... WHERE mdl_key = ...`) instead of a pre-loaded sf — so a
multi-GB source (full-resolution IUCN ranges are ~7 GB) never loads into
memory at once, and the per-model query is an index hit. Parallel
(`furrr`) + resumable.

## Usage

``` r
publish_pmtiles_from_gpkg(
  gpkg,
  layer,
  keys,
  dir_out,
  workers = NULL,
  redo = FALSE,
  minzoom = 0,
  maxzoom = 6,
  simplification = 10
)
```

## Arguments

- gpkg:

  path to a GeoPackage whose `layer` has a character `mdl_key` column
  (index it on `mdl_key` for speed:
  `CREATE INDEX ... ON layer(mdl_key)`)

- layer:

  gpkg layer name (also the tile `source_layer`)

- keys:

  character `mdl_key`s to publish (each -\> one file)

- dir_out:

  output directory (created); one `{slug(sp_id)}.pmtiles` per key

- workers, redo, minzoom, maxzoom, simplification:

  as in
  [`publish_pmtiles_models()`](http://marinesensitivity.org/msens/reference/publish_pmtiles_models.md)

## Value

a tibble `(mdl_key, sp_id, file, mb)`, one row per model built or found
