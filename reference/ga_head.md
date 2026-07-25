# Analytics `<head>` snippet as a Shiny tag

[`ga_js()`](http://marinesensitivity.org/msens/reference/ga_js.md)
wrapped in
[`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html)
for use inside `tags$head()`.

## Usage

``` r
ga_head(
  app,
  content_group = app,
  app_version = "",
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

  version string recorded on every event, e.g. `"v8"`

- measurement_id:

  GA4 measurement ID; defaults to the project property

- log_url:

  Apps Script `/exec` endpoint for the Sheet log. Defaults to the
  `MSENS_LOG_URL` environment variable; empty means the Sheet leg is a
  silent no-op (GA4 still receives events).

## Value

an `html` object

## Examples

``` r
if (FALSE) { # \dontrun{
ui <- bslib::page_sidebar(
  tags$head(msens::ga_head("scores", app_version = "v8")), ...)
} # }
```
