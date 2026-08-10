# HTML for the version picker

Lists every published version newest-first, marks the one on screen, and
links to the others via `?ver=`. Retired and pre-release versions are
labelled rather than hidden: a reader following a citation to v4 needs
to find it, and needs to be told it is superseded.

## Usage

``` r
version_picker_html(
  current,
  versions = NULL,
  href = function(v) sprintf("?ver=%s", v)
)
```

## Arguments

- current:

  the version being displayed

- versions:

  data frame from
  [`atlas_versions()`](http://marinesensitivity.org/msens/reference/atlas_versions.md);
  fetched if `NULL`

- href:

  a function of the version label returning its link (default
  `?ver={v}`)

## Value

an
[`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html)
