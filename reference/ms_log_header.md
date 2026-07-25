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
#>  [1] "timestamp"   "app"         "app_version" "client_id"   "session_id" 
#>  [6] "event"       "params"      "page"        "referrer"    "user_agent" 
```
