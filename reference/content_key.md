# Object key for a content-addressed COG

`grid_id` is part of the key because `cell_id` is meaningless without
it: the same id names a different place on `usa05` than on `global05`,
so two surfaces could hash alike yet be entirely different maps.

## Usage

``` r
content_key(grid_id, content_hash, ext = "tif")
```

## Arguments

- grid_id:

  grid the cells belong to (see
  [`grid_registry()`](http://marinesensitivity.org/msens/reference/grid_registry.md))

- content_hash:

  from
  [`content_hashes()`](http://marinesensitivity.org/msens/reference/content_hashes.md)

- ext:

  file extension (default `"tif"`)

## Value

relative object key(s), e.g. `"cog/usa05/9f3c….tif"`
