# Compute extinction risk score from extrisk_code and flags

US-listed species get the max score across ESA status (EN=100, TN=50,
LC=1), MMPA (20), and MBTA (10). Non-US species fall back to IUCN Red
List scale (CR=50, EN=25, VU=5, NT=2, other=1).

## Usage

``` r
compute_er_score(extrisk_code, is_mmpa = FALSE, is_mbta = FALSE)
```

## Arguments

- extrisk_code:

  character, e.g. "NMFS:EN", "FWS:TN", "IUCN:CR"

- is_mmpa:

  logical; species protected under MMPA (default: FALSE)

- is_mbta:

  logical; species protected under MBTA (default: FALSE)

## Value

integer score 0-100
