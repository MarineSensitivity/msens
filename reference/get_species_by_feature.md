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
#> Error in httr2::req_perform(httr2::req_url_query(httr2::req_url_path_append(httr2::request("https://api.marinesensitivity.org"),     "species_by_feature"), schema.table = schema.table, where = where)): HTTP 502 Bad Gateway.
```
