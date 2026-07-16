# Build the targets list from `msens:` frontmatter

Reads every pipeline notebook's `msens:` block (via
[`parse_qmd_frontmatter()`](http://marinesensitivity.org/msens/reference/parse_qmd_frontmatter.md))
and returns a [`list()`](https://rdrr.io/r/base/list.html) of
[`targets::tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html)
objects for use as the body of `_targets.R`. Each target's body is a
[`{}`](https://rdrr.io/r/base/Paren.html) block whose leading statements
are bare references to its upstream target names (so `targets` draws the
DAG edges), followed by a
[`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
of the notebook and the `output` path (tracked with `format = "file"`
for hash-based invalidation).

## Usage

``` r
build_targets_list(
  workflows_dir = here::here(),
  exclude = NULL,
  verbose = TRUE
)
```

## Arguments

- workflows_dir:

  directory holding the pipeline `.qmd`s (default
  [`here::here()`](https://here.r-lib.org/reference/here.html))

- exclude:

  character vector of target names (or `.qmd` filenames) to drop from
  the pipeline; excluded targets are also stripped from other targets'
  dependency lists

- verbose:

  print the parsed workflow table (default `TRUE`)

## Value

a [`list()`](https://rdrr.io/r/base/list.html) of
[`tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html)
objects for `_targets.R`

## Details

`dependency: [auto]` (typically the `release` caboose) resolves to every
`grid` + `ingest` target.

## Examples

``` r
if (FALSE) { # \dontrun{
# in _targets.R:
library(targets)
library(msens)          # or devtools::load_all("../msens")
build_targets_list()
} # }
```
