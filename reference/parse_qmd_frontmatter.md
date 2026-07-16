# Parse `msens:` frontmatter from pipeline notebooks

Globs the `.qmd` files in `workflows_dir`, reads the YAML frontmatter of
each (the block between the first two `---` fences), and keeps only
those carrying a top-level `msens:` key. Returns one row per pipeline
notebook with the fields needed to wire the `targets` DAG.

## Usage

``` r
parse_qmd_frontmatter(workflows_dir = here::here(), pattern = "*.qmd")
```

## Arguments

- workflows_dir:

  directory holding the pipeline `.qmd`s (default
  [`here::here()`](https://here.r-lib.org/reference/here.html))

- pattern:

  glob for notebooks (default `"*.qmd"`)

## Value

a tibble with columns `qmd_file`, `target_name`, `workflow_type`,
`dependency` (list column), `output`

## Details

The `msens:` block vocabulary:

- target_name:

  the `targets` node name (a legal R symbol)

- workflow_type:

  one of `grid`, `ingest`, `merge`, `score`, `publish`, `release`,
  `test`

- dependency:

  list of upstream `target_name`s, or `[auto]` to depend on every
  `grid` + `ingest` target

- output:

  the file/dir the notebook produces (tracked via `format = "file"`; may
  contain a `*` glob)

## Examples

``` r
if (FALSE) { # \dontrun{
wf <- parse_qmd_frontmatter("~/Github/MarineSensitivity/workflows")
dplyr::filter(wf, workflow_type == "ingest")
} # }
```
