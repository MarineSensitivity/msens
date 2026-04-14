# Species table (interactive DT or static gt)

Renders a species tibble — as returned by
[`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md)
— as either an interactive
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) (html
output) or a static [`gt::gt`](https://gt.rstudio.com/reference/gt.html)
table (pdf/docx output). Taxon and model columns become clickable links
in the interactive version.

## Usage

``` r
tbl_species(d_spp, interactive = TRUE)
```

## Arguments

- d_spp:

  tibble with columns from
  [`species_for_cells()`](http://marinesensitivity.org/msens/reference/species_for_cells.md):
  `mdl_seq`, `sp_cat`, `sp_common`, `sp_scientific`, `taxon_id`,
  `taxon_authority`, `er_code`, `er_score`, `is_mmpa`, `is_mbta`,
  `area_km2`, `avg_suit`, `pct_cat`

- interactive:

  logical; if TRUE (default) return a DT datatable, otherwise return a
  gt table

## Value

a DT htmlwidget or a gt_tbl
