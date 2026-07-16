# Seasonal per-species Item from the NCCOS season-COG metadata

One Item per (ds_key, sp_code); one COG Asset per season (the public
season-named GeoTIFFs), each tagged with `sdm:season` and
`raster:bands`.

## Usage

``` r
stac_season_cog_item(d_sp, cfg, taxon = NULL)
```

## Arguments

- d_sp:

  rows of `nc_models.csv` for one (ds_key, sp_code)

- cfg:

  config from
  [`stac_cfg()`](http://marinesensitivity.org/msens/reference/stac_cfg.md)

- taxon:

  optional list(scientific_name=, common_name=, group=, authorities=)

## Value

STAC Item as a list
