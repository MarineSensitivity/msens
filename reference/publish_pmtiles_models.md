# Publish per-model PMTiles — one file per `mdl_key`

Splits an sf carrying a `mdl_key` column into **one PMTiles per model**,
written to `{dir_out}/{slug(sp_id)}.pmtiles` (`sp_id` = `mdl_key` after
the first `|`). Each file holds a single model, so the species app needs
no client-side filter and — crucially — low-zoom tiles never collide
across species. Packing thousands of overlapping global ranges into one
per-dataset archive overflows the z0–z2 world tiles past maplibre's
per-tile budget, so it silently drops them (a selected range shows "no
polygon at all" at global zoom); a per-model file tiles to a few KB and
always renders. Mirrors the per-model AquaMaps COGs. Parallel
(`furrr`) + resumable (skips existing unless `redo`).

## Usage

``` r
publish_pmtiles_models(
  x,
  dir_out,
  layer,
  workers = NULL,
  redo = FALSE,
  minzoom = 0,
  maxzoom = 6,
  simplification = 10
)
```

## Arguments

- x:

  an sf with a character `mdl_key` column (features may repeat a key)

- dir_out:

  output directory (created); one `.pmtiles` per model

- layer:

  tile layer name (the app's `source_layer`)

- workers:

  parallel workers (default `max(1, detectCores() - 2)`)

- redo:

  rebuild files that already exist (default `FALSE`)

- minzoom, maxzoom, simplification:

  passed to
  [`publish_pmtiles()`](http://marinesensitivity.org/msens/reference/publish_pmtiles.md)

## Value

a tibble `(mdl_key, sp_id, file, mb)`, one row per model built or found
