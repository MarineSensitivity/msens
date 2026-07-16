# Parse `mdl_key`(s) into components

Inverse of
[`mdl_key_raw()`](http://marinesensitivity.org/msens/reference/mdl_key_raw.md)
/
[`mdl_key_merged()`](http://marinesensitivity.org/msens/reference/mdl_key_merged.md):
splits on `|` into `dataset_key`, `sp_id`, `interval`; for `ms_merge`
keys the `sp_id` (`AUTHORITY:id`) is further split into
`taxon_authority` + `taxon_id`.

## Usage

``` r
mdl_key_parse(mdl_key)
```

## Arguments

- mdl_key:

  character `mdl_key`(s)

## Value

a tibble with columns `mdl_key`, `dataset_key`, `sp_id`, `interval`,
`taxon_authority`, `taxon_id` (`NA` where not applicable)

## Examples

``` r
mdl_key_parse(c("am|Fis-29291", "gm|1234|01", "ms_merge|WORMS:137209"))
#> # A tibble: 3 × 6
#>   mdl_key               dataset_key sp_id      interval taxon_authority taxon_id
#>   <chr>                 <chr>       <chr>      <chr>    <chr>           <chr>   
#> 1 am|Fis-29291          am          Fis-29291  NA       NA              NA      
#> 2 gm|1234|01            gm          1234       01       NA              NA      
#> 3 ms_merge|WORMS:137209 ms_merge    WORMS:137… NA       WORMS           137209  
```
