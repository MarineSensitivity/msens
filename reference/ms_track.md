# Send a tracking event from the Shiny server to the browser

Pushes an event over the session's existing websocket; the client-side
handler installed by
[`ga_js()`](http://marinesensitivity.org/msens/reference/ga_js.md)
forwards it to GA4 and queues it for the Sheet log. **Non-blocking** —
no HTTP request is made on the R side, so a slow or unreachable log
endpoint can never stall a reactive.

## Usage

``` r
ms_track(session, event, ...)
```

## Arguments

- session:

  the Shiny `session` object

- event:

  event name, passed to
  [`ms_event()`](http://marinesensitivity.org/msens/reference/ms_event.md)

- ...:

  named event parameters, passed to
  [`ms_event()`](http://marinesensitivity.org/msens/reference/ms_event.md)

## Value

the payload, invisibly

## Details

Use it for facts only the server knows (the scientific name behind a
picker value, a report's parameters, a row count, an error); pure UI
interactions are better tracked client-side by
[`ga_js()`](http://marinesensitivity.org/msens/reference/ga_js.md)'s
delegated handlers.

## Examples

``` r
if (FALSE) { # \dontrun{
ms_track(session, "select_species",
         mdl_key = input$sel_sp, scientific_name = sp$scientific_name)
} # }
```
