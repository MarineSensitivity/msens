# Parse NOAA protected_status field

Extract ESA status and MMPA flag from the semicolon-delimited
`protected_status` field in the NOAA species directory.

## Usage

``` r
parse_noaa_status(status_str)
```

## Arguments

- status_str:

  character vector of semicolon-delimited statuses

## Value

tibble with columns: esa_status (character), is_mmpa (logical)
