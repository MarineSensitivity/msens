# Best-effort client IP from a Shiny request

Reads `X-Forwarded-For` (set by the Caddy reverse proxy the apps sit
behind) and falls back to the direct `REMOTE_ADDR`. Never errors.

## Usage

``` r
ms_client_ip(x)
```

## Arguments

- x:

  a Shiny `session`, or the `req` environment handed to a `ui` function
  (anything carrying the request fields directly)

## Value

character scalar, or `NA_character_` if unavailable

## Details

**Pass the `req` of a `ui` function, not a `session`, when you can.**
shiny-server does not proxy the websocket upgrade — it opens a fresh
localhost connection to the R worker — so a session's `request` carries
no `X-Forwarded-For` and its `REMOTE_ADDR` is always `127.0.0.1`, no
matter how correctly Caddy is configured (verified on msens1: the page
GET shows the real address while the websocket handshake shows
`HTTP_HOST 127.0.0.1:<worker port>` and no forwarded header at all). The
page's HTTP request, which `ui = function(req)` receives, is the only
place the real client IP survives. See the `ip` argument of
[`ga_js()`](http://marinesensitivity.org/msens/reference/ga_js.md).

## Examples

``` r
ms_client_ip(list(request = list(HTTP_X_FORWARDED_FOR = "203.0.113.7, 10.0.0.1")))
#> [1] "203.0.113.7"
ms_client_ip(list(HTTP_X_FORWARDED_FOR = "203.0.113.7"))   # a ui(req)
#> [1] "203.0.113.7"
```
