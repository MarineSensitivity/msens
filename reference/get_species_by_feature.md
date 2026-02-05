# Get species by feature

Output a table of species present from one or more spatial feature(s).

## Usage

``` r
get_species_by_feature(schema.table, where)
```

## Arguments

- schema.table:

  The SCHEMA.TABLE containing the feature(s).

- where:

  The WHERE clause selecting the feature(s) as the area of interest.

## Value

data.frame of species present in the feature(s)

## Details

Use the `species_by_feature` endpoint of the API at
[api.marinesensitivity.org](https://api.marinesensitivity.org/__docs__/#/default/get_species_by_feature).

## Examples

``` r
get_species_by_feature(
  schema.table = "raw.mr_eez",
  where        = "mrgid = 8442")
#> Warning: The `file` argument of `vroom()` must use `I()` for literal data as of vroom
#> 1.5.0.
#>   
#>   # Bad:
#>   vroom("X,Y\n1.5,2.3\n")
#>   
#>   # Good:
#>   vroom(I("X,Y\n1.5,2.3\n"))
#> ℹ The deprecated feature was likely used in the readr package.
#>   Please report the issue at <https://github.com/tidyverse/readr/issues>.
#> # A tibble: 4,797 × 12
#>    sp_key n_cells avg_pct_cell area_km2 avg_suit   amt phylum class order family
#>    <chr>    <dbl>        <dbl>    <dbl>    <dbl> <dbl> <chr>  <chr> <chr> <chr> 
#>  1 Chn-0…       9        0.750   20765.    0.703 4.75  Arthr… Mala… Deca… Galat…
#>  2 Chn-2…       1        0.999    3073.    1     0.999 Arthr… Mala… Deca… Scyll…
#>  3 Chn-4…       1        0.999    3073.    1     0.999 Arthr… Mala… Deca… Palae…
#>  4 Chn-4…       1        0.999    3073.    0.99  0.989 Arthr… Maxi… Sess… Pyrgo…
#>  5 Chn-5…       1        0.999    3073.    0.98  0.979 Arthr… Mala… Deca… Palae…
#>  6 Chn-a…       1        0.999    3073.    0.97  0.969 Arthr… Mala… Deca… Coeno…
#>  7 Chn-e…       1        0.999    3073.    1     0.999 Arthr… Mala… Deca… Albun…
#>  8 Chn-f…       1        0.999    3073.    0.87  0.869 Arthr… Mala… Deca… Munid…
#>  9 Fis-1…       1        0.999    3073.    1     0.999 Chord… Acti… Mugi… Mugil…
#> 10 Fis-1…       1        0.999    3073.    1     0.999 Chord… Acti… Perc… Labri…
#> # ℹ 4,787 more rows
#> # ℹ 2 more variables: genus <chr>, species <chr>
```
