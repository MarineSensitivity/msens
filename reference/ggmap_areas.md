# Static ggplot map of labeled areas

Returns a
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
map of labeled sf polygons overlaid on an
[`rnaturalearth::ne_countries`](https://docs.ropensci.org/rnaturalearth/reference/ne_countries.html)
basemap, for embedding in pdf/docx Quarto output where interactive
[`mapgl::mapgl`](https://walker-data.com/mapgl/reference/mapgl-package.html)
maps do not render.

## Usage

``` r
ggmap_areas(areas_sf, fill_color = "#3388ff", fill_alpha = 0.4)
```

## Arguments

- areas_sf:

  an sf object with a `label` column

- fill_color:

  fill color for area polygons (default "#3388ff")

- fill_alpha:

  fill alpha (default 0.4)

## Value

a ggplot
