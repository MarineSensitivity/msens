# Value of a COG at a point, via titiler

Asks the tile server what the layer shows at a coordinate, instead of
the app opening a raster itself. The pixels the user is looking at and
the number in the popup then come from the SAME source, so they cannot
disagree – and the app needs no local copy of the surface, which is the
point of publishing COGs.

## Usage

``` r
cog_point_value(
  cog_url,
  lon,
  lat,
  base = "https://titiler-v8.marinesensitivity.org",
  timeout = 8
)
```

## Arguments

- cog_url:

  the COG the layer is drawn from

- lon, lat:

  coordinates in degrees (-180..180)

- base:

  titiler base URL

- timeout:

  seconds before giving up

## Value

numeric value, or `NA` if the point is nodata/outside or the request
fails – a popup is not worth an error dialog
