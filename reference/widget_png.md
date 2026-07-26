# Render an htmlwidget to a static PNG

Saves `widget` to a temporary self-contained-free HTML, screenshots it
with headless Chrome via `webshot2`, and returns the image path — for
use as `knitr::include_graphics(widget_png(m, "figures/map.png"))` in a
notebook chunk, in place of printing the widget.

## Usage

``` r
widget_png(widget, file, width = 1200, height = 800, delay = 8, zoom = 2)
```

## Arguments

- widget:

  an htmlwidget (e.g. from
  [`mapgl::maplibre()`](https://walker-data.com/mapgl/reference/maplibre.html),
  `mapview`, `leaflet`)

- file:

  output PNG path, relative to the notebook (parent dirs created)

- width, height:

  viewport size in CSS pixels

- delay:

  seconds to wait before the screenshot — map tiles and GeoJSON layers
  load asynchronously, so too short a delay yields a blank basemap

- zoom:

  device pixel ratio; 2 gives a retina-quality PNG

## Value

`file`, invisibly-safe for
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html)

## Details

Use it for maps whose payload is large and whose interactivity is not
the point. Keep printing the widget where panning/zooming genuinely
matters.

## Examples

``` r
if (FALSE) { # \dontrun{
knitr::include_graphics(widget_png(m_gap, "figures/gap_turtles.png"))
} # }
```
