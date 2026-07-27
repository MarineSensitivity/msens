# Analytics `<head>` snippet (GA4 + batched Sheet beacon)

Generates the self-contained HTML/JS installed once per page: the GA4
gtag loader, the client-side event queue that beacons to the usage-log
Sheet, a `Shiny.addCustomMessageHandler("msTrack", ...)` handler so
[`ms_track()`](http://marinesensitivity.org/msens/reference/ms_track.md)
works, and a `window.msTrack(event, params)` helper for page JS.

## Usage

``` r
ga_js(
  app,
  content_group = app,
  app_version = "",
  ip = "",
  measurement_id = .MS_GA_ID,
  log_url = Sys.getenv("MSENS_LOG_URL", "")
)
```

## Arguments

- app:

  short app/product id recorded on every event, e.g. `"scores"`

- content_group:

  GA4 content group for reporting; defaults to `app`

- app_version:

  version string recorded on every event — the deployed git commit, so a
  Sheet row ties back to the exact code that produced it

- ip:

  client IP to stamp on every logged row, from `ms_client_ip(req)` in a
  `ui = function(req)`. Behind shiny-server this is the ONLY place a
  real address exists — see
  [`ms_client_ip()`](http://marinesensitivity.org/msens/reference/ms_client_ip.md).

- measurement_id:

  GA4 measurement ID; defaults to the project property

- log_url:

  Apps Script `/exec` endpoint for the Sheet log. Defaults to the
  `MSENS_LOG_URL` environment variable; empty means the Sheet leg is a
  silent no-op (GA4 still receives events).

## Value

character scalar of raw HTML (`<script>` tags)

## Details

The same snippet serves every product; `content_group` is what separates
them in GA4 reporting, so no product needs its own measurement ID.

## Examples

``` r
substr(ga_js(app = "scores", app_version = "v8"), 1, 60)
#> <!-- Google tag (gtag.js) + MarineSensitivity usage log -->
#> 
```
