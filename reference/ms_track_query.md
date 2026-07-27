# Time a query, log its shape, and re-raise any error

Wraps an expression so the Sheet records how long it took, how many rows
it returned, and whether it failed — in the dedicated `ms` / `n_rows` /
`status` / `error` columns, which stay numeric and chartable rather than
being buried in the `params` JSON.

## Usage

``` r
ms_track_query(session, event, params = list(), expr)
```

## Arguments

- session:

  the Shiny `session` object

- event:

  event name, passed to
  [`ms_event()`](http://marinesensitivity.org/msens/reference/ms_event.md)

- params:

  named list of event parameters

- expr:

  the expression to time

## Value

whatever `expr` returns

## Details

The result passes through untouched (including a lazy `dbplyr` table,
whose row count is deliberately NOT forced), and an error is re-raised
after being logged, so wrapping a call never changes behaviour.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- ms_track_query(session, "download_species_csv", list(area = hdr),
                    species_for_zone(con, "programarea_key", key))
} # }
```
