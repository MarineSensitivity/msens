# msens

``` r
library(msens)
```

``` r
d <- get_species_by_feature(
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
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
d
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
