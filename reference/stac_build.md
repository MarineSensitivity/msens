# Build the full static STAC catalog for a version

Connects to the version's SDM DuckDB, emits a Collection + model-surface
Item per dataset, optionally adds NCCOS seasonal Items from
`nc_models.csv`, and writes the tree (root Catalog -\> version
Collection -\> dataset Collections -\> Items) under `dir_out`.

## Usage

``` r
stac_build(
  version = "v7",
  dir_out = NULL,
  cfg = NULL,
  nc_csv = NULL,
  con = NULL
)
```

## Arguments

- version:

  data version (default "v7")

- dir_out:

  output directory for the catalog tree

- cfg:

  config from
  [`stac_cfg()`](http://marinesensitivity.org/msens/reference/stac_cfg.md)
  (defaults to `stac_cfg(version)`)

- nc_csv:

  optional path to `nc_models.csv` for seasonal Items

- con:

  optional open DBI connection (else opens read-only via
  [`sdm_db_con()`](http://marinesensitivity.org/msens/reference/sdm_db_con.md))

## Value

invisibly, the path to the root `catalog.json`
