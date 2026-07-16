# Compose a raw (per-dataset) model key

The stable id for a native, per-dataset model: `{dataset_key}|{sp_id}`
with an optional trailing `|{interval}` for time-resolved models
(monthly `gm`, seasonal `nc`). Vectorised over `sp_id` (and `interval`).

## Usage

``` r
mdl_key_raw(dataset_key, sp_id, interval = NULL)
```

## Arguments

- dataset_key:

  scalar dataset key, e.g. `"am"`, `"gm"`, `"nc"`, `"botw"`

- sp_id:

  dataset-native species/guild id(s) (character or coercible)

- interval:

  optional interval label(s), e.g. month `"01"` or season `"summer"`

## Value

character `mdl_key`(s)

## Examples

``` r
mdl_key_raw("am", "Fis-29291")   # "am|Fis-29291"
#> [1] "am|Fis-29291"
mdl_key_raw("gm", 1234, "01")    # "gm|1234|01"
#> [1] "gm|1234|01"
```
