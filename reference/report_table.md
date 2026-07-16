# House-style table for pipeline reports

Thin wrapper over
[`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) so notebook
`## Outputs` tables share one style and a single call site to restyle
later. Returns `x` unchanged if `knitr` is unavailable.

## Usage

``` r
report_table(x, caption = NULL)
```

## Arguments

- x:

  a data.frame / tibble

- caption:

  optional table caption

## Value

a `knitr_kable` object (or `x`)
