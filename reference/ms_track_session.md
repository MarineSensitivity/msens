# Hand the browser the session facts only the server knows

The Shiny session token cannot be read in JavaScript, so the server
pushes it once at session start; the client then stamps it on every
queued row (the `session` column of
[`ms_log_header()`](http://marinesensitivity.org/msens/reference/ms_log_header.md)).
Call it at the top of the `server` function, before any
[`ms_track()`](http://marinesensitivity.org/msens/reference/ms_track.md)
call, so no event is written without it.

## Usage

``` r
ms_track_session(session)
```

## Arguments

- session:

  the Shiny `session` object

## Value

the sent list, invisibly

## Details

The token is authoritative, the IP is only a **fallback**: behind
shiny-server a session sees `127.0.0.1`, so an `ip` already baked into
the page by
[`ga_js()`](http://marinesensitivity.org/msens/reference/ga_js.md) wins
and this one is ignored. Without that rule the websocket's useless
address would silently overwrite the good one a moment after the page
supplied it.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  msens::ms_track_session(session)
  ...
}
} # }
```
