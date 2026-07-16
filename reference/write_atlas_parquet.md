# Write an atlas surface to Parquet (arrow path) with the standard v8 options

Parquet V2 (`version = "2.6"`), zstd compression, row groups of
`chunk_size` rows (arrow has no byte-based row-group control). Use in
per-model / per-species ingest loops where the data is an in-memory
tibble (and often inside `mclapply` / `furrr` forks). For
DuckDB-resident data use
[`copy_atlas_parquet()`](http://marinesensitivity.org/msens/reference/copy_atlas_parquet.md)
instead — both share the one option set in `.atlas_pq`.

## Usage

``` r
write_atlas_parquet(x, path, chunk_size = .atlas_pq$arrow_chunk)
```

## Arguments

- x:

  a data.frame / tibble (typically `mdl_key, cell_id, val`)

- path:

  output `.parquet` path

- chunk_size:

  rows per row group (default from `.atlas_pq`)

## Value

`path`, invisibly
