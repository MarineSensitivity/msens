# COPY a DuckDB relation/query to Parquet with the standard v8 options

Emits
`COPY (<sql>) TO '<path>' (FORMAT parquet, PARQUET_VERSION V2, COMPRESSION zstd, ...)`.
Two regimes:

- **unordered / partitioned** (default, or `partition_by`/`per_thread`):
  drops insertion-order preservation so `ROW_GROUP_SIZE_BYTES '80MB'`
  binds (true ~80 MB row groups). Restores the prior setting afterward.

- **ordered** (`order_by` given): keeps insertion order for the serving
  row-group zone-map pruning, so byte-sized groups can't be used —
  approximates ~80 MB with a row count instead.

## Usage

``` r
copy_atlas_parquet(
  con,
  sql,
  path,
  order_by = NULL,
  per_thread = FALSE,
  partition_by = NULL
)
```

## Arguments

- con:

  open DuckDB connection

- sql:

  a SELECT (no trailing `;`) OR a bare table name

- path:

  output file (or a directory when `partition_by`/`per_thread`)

- order_by:

  optional ORDER BY column expression, e.g. `"mdl_key"`

- per_thread:

  write one file per thread into `path` (a dir); default FALSE

- partition_by:

  optional Hive-partition column(s), e.g. `"mdl_id"`

## Value

`path`, invisibly
