# Write a deterministic, content-addressed target manifest

The tracked `output:` for a pipeline notebook. Bytes are a pure function
of `target`, `content_hash` and the (content-derived) `stats`, keys in
fixed order, NO wall-clock — so `targets` `format = "file"` re-hashes to
the SAME value when the data is unchanged and downstream targets do not
re-run. Idempotent: if the file already holds identical bytes it is left
untouched (mtime preserved) unless `force`. Keep `stats` deterministic
(counts, ranges, versions) and free of machine-specific paths so the
manifest is host-independent.

## Usage

``` r
write_manifest(path, target, content_hash, stats = list(), force = FALSE)
```

## Arguments

- path:

  manifest path (the notebook's `output:`)

- target:

  target name

- content_hash:

  from
  [`hash_parquet()`](http://marinesensitivity.org/msens/reference/hash_parquet.md)
  /
  [`hash_query()`](http://marinesensitivity.org/msens/reference/hash_query.md)

- stats:

  named list of deterministic summary stats

- force:

  rewrite even if unchanged (default `FALSE`; see
  [`force_target()`](http://marinesensitivity.org/msens/reference/force_target.md))

## Value

`content_hash`, invisibly
