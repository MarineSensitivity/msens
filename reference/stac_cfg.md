# Default configuration (base URLs) for STAC generation

Mirrors the `is_prod`/`pmtiles_base_url` pattern: defaults point at the
public marinesensitivity.org hosts. Override for local or BOEM-internal
deployments.

## Usage

``` r
stac_cfg(version = "v7")
```

## Arguments

- version:

  data version (e.g. "v7")

## Value

named list of base URLs + the study-area bbox
