# Model-surface Item for a (DuckDB-backed) dataset

Describes the dataset's per-taxon surfaces as a static GeoParquet asset
plus the live DuckDB-SQL TiTiler endpoint (as a web-map link + an
alternate asset), parameterized by `{mdl_seq}`. If the dataset has more
than one prediction interval, a datacube temporal/season dimension
enumerates them.

## Usage

``` r
stac_model_cell_item(ds, cfg, time_periods = NULL, mdl_key_ex = NA_character_)
```

## Arguments

- ds:

  one-row data.frame from `dataset`

- cfg:

  config from
  [`stac_cfg()`](http://marinesensitivity.org/msens/reference/stac_cfg.md)

- time_periods:

  character vector of distinct `model.time_period` for the dataset

## Value

STAC Item as a list
