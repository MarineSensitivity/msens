# Query WoRMS REST API with multiple requests

When trying to perform batch requests the WoRMS REST API unfortunately
does not return the requested field so it is not obvious which taxa
requested matches which response. Even when fetching records in batch by
`aphia_id` functions in the `worrms` R package do not page requests
based on the record limits of the WoRMS REST API. Finally, it is much
preferred to use the multiplexing capabilities of the latest `httr2`
library to send multiple requests in parallel, versus each one
sequentially.

## Usage

``` r
wm_rest(
  df,
  fld,
  operation = "AphiaRecordsByMatchNames",
  server = "https://www.marinespecies.org/rest",
  ...
)
```

## Arguments

- df:

  data frame to match

- fld:

  field in data frame to use with operation

- operation:

  operation name of WoRMS REST API; One of operations listed at
  [marinespecies.org/rest](https://www.marinespecies.org/rest), like
  "AphiaRecordsByMatchNames" (non-paging), "AphiaRecordsByVernacular"
  (paging), "AphiaRecordsByNames" (paging), or "AphiaRecordsByAphiaIDs"
  (paging); default: "AphiaRecordsByMatchNames"

- server:

  URL of server REST endpoint; default:
  "https://www.marinespecies.org/rest"

- ...:

  other query parameters to pass to operation

## Value

data frame of results from WoRMS API prepended with unique values from
`fld`

## Examples

``` r
if (FALSE) { # \dontrun{
tmp_test <- tibble::tribble(
           ~common,                     ~scientific, aphia_id_0,
     "Minke whale",     "Balaenoptera acutorostrata", 137087,
      "Blue whale",          "Balaenoptera musculus", 137090,
"Bonaparte's Gull",   "Chroicocephalus philadelphia", 882954)
# 882954 invalid non-marine -> valid marine 159076

wm_exact <- wm_rest(tmp_test, scientific, "AphiaRecordsByMatchNames")
wm_fuzzy <- wm_rest(tmp_test, scientific, "AphiaRecordsByNames")
wm_byid  <- wm_rest(tmp_test, scientific, "AphiaRecordsByNames", marine_only=F)
} # }
```
