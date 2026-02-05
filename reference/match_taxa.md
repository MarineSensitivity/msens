# Match taxa to spp.duckdb via cascade

Match species records to the canonical taxonomy in spp.duckdb using a
three-step cascade:

## Usage

``` r
match_taxa(d, con_spp)
```

## Arguments

- d:

  data.frame with `scientific_name` and optionally `itis_id` columns

- con_spp:

  DBI connection to spp.duckdb (read-only)

## Value

d with added `worms_id` and `botw_id` columns

## Details

1.  ITIS TSN crosswalk -\> worms_id

2.  Exact scientific_name match in worms table

3.  WoRMS REST API for unmatched (via
    [`msens::wm_rest()`](http://marinesensitivity.org/msens/reference/wm_rest.md))
