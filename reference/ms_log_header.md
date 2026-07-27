# Column header for the usage-log Sheet

The exact first row the Google Sheet must carry for
[`ms_apps_script()`](http://marinesensitivity.org/msens/reference/ms_apps_script.md)
to append into. Kept here so the Sheet, the Apps Script, and the client
payload cannot drift.

## Usage

``` r
ms_log_header()
```

## Value

character vector of column names, in order

## Examples

``` r
ms_log_header()
#>  [1] "timestamp"   "ip"          "session"     "event"       "params"     
#>  [6] "n_rows"      "ms"          "status"      "error"       "app_version"
#> [11] "app"         "client_id"   "session_id"  "page"        "referrer"   
#> [16] "user_agent" 
```
