# Build a tracking-event payload

The pure payload constructor shared by
[`ms_track()`](http://marinesensitivity.org/msens/reference/ms_track.md)
and the tests. Normalises the event name to GA4's rules (lowercase,
`[a-z0-9_]`, leading letter, truncated to 40 characters) and drops
`NULL`/`NA`/empty parameters so the Sheet's `params` column stays
readable.

## Usage

``` r
ms_event(event, ...)
```

## Arguments

- event:

  event name, e.g. `"select_species"`; coerced to GA4-safe form

- ...:

  named event parameters (scalars); `NULL`/`NA`/`""` are dropped

## Value

list with `event` (character scalar) and `params` (named list)

## Details

Parameter *values* are NOT truncated here — the Sheet leg wants the full
string (a long search term, a full scientific name). The client
truncates to 100 characters for the gtag leg only.

## Examples

``` r
ms_event("Select Species", scientific_name = "Dermochelys coriacea", n = 3)
#> $event
#> [1] "select_species"
#> 
#> $params
#> $params$scientific_name
#> [1] "Dermochelys coriacea"
#> 
#> $params$n
#> [1] "3"
#> 
#> 
#> $metrics
#> named list()
#> 
ms_event("download", file = NULL, format = "csv")   # NULL dropped
#> $event
#> [1] "download"
#> 
#> $params
#> $params$format
#> [1] "csv"
#> 
#> 
#> $metrics
#> named list()
#> 
```
