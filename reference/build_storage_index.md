# Generate index pages for a bucket tree

A page is generated for EVERY directory, however many files it holds:
the cost of this index is the number of PAGES, not the number of
objects, so a directory of 20,000 files is still one page. Only a
directory whose CHILDREN are themselves numerous directories is
collapsed - `serve/model_cell/` is ~17,765 Hive partition directories,
which would be ~17,765 pages nobody reads. Those children are still
listed with counts and labelled, so the reason is visible rather than
mysterious.

## Usage

``` r
build_storage_index(
  objs,
  site_url = "https://storage.marinesensitivity.org",
  obj_url = "https://s3.us-east-1.amazonaws.com/oceanmetrics.io-public",
  max_child_dirs = 500L,
  max_rows = 2000L
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

- max_child_dirs:

  a directory with more than this many SUBDIRECTORIES has its children
  listed but not expanded into pages of their own

- max_rows:

  most rows rendered on one page, so a huge directory does not emit a
  multi-megabyte document

## Value

a data frame of `key` (the index.html object key) and `html`
