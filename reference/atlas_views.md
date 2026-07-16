# Create named views over the released atlas tables

Mirrors the view set the serving `serve.duckdb` exposes, so the
calc/score helpers that reference bare table names (`tbl(con, "zone")`,
[`scores_for_pra()`](http://marinesensitivity.org/msens/reference/scores_for_pra.md),
[`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md),
…) compose directly with an
[`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
connection. Single-file tables are read via **path-style HTTPS**
(anonymous GET + HTTP range; the dotted bucket breaks virtual-hosted
TLS). `model_cell` is Hive-partitioned under `serve/` and its glob needs
S3 LIST, so it is created only when `anon = FALSE` (credentialed); it
joins `model` back so ad-hoc queries select by the stable `mdl_key`, not
the volatile `mdl_id`. The scoring tables store the metric in `val`; a
`val AS value` alias is exposed for back-compat.

## Usage

``` r
atlas_views(
  con,
  base = attr(con, "atlas_base"),
  region = "us-east-1",
  anon = FALSE
)
```

## Arguments

- con:

  connection from
  [`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)

- base:

  atlas base URL (defaults to `attr(con, "atlas_base")`)

- region:

  S3 region

- anon:

  if `TRUE`, skip the credentialed `model_cell` glob view

## Value

`con`, invisibly
