# Generate index pages for a bucket tree

Generate index pages for a bucket tree

## Usage

``` r
build_storage_index(
  objs,
  site_url = "https://storage.marinesensitivity.org",
  obj_url = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public",
  max_depth = 3L,
  skip = "^marine-atlas/(cog|v[0-9]+[a-z]?/(serve|dist_merged|dist))/"
)
```

## Arguments

- objs:

  data frame from
  [`s3_list_all()`](http://marinesensitivity.org/msens/reference/s3_list_all.md)

- site_url:

  public base of the browse host

- obj_url:

  public base objects are fetched from

- max_depth:

  deepest directory level to generate a page for

- skip:

  regex of key prefixes to summarize rather than walk (machine-only
  trees such as Hive partitions, which would otherwise produce tens of
  thousands of pages nobody reads)

## Value

a data frame of `key` (the index.html object key) and `html`
