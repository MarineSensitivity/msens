# Build a flower-plot ggplot for component scores

Polar bar chart of component scores with a center-label weighted score.
Returns a plain ggplot object (no interactive wrapping), so it is usable
in static outputs (pdf/docx). For an interactive version, use
[`plot_flower()`](http://marinesensitivity.org/msens/reference/plot_flower.md)
with `interactive = TRUE` (the default).

## Usage

``` r
ggplot_flower(
  data,
  fld_category,
  fld_height,
  fld_width,
  tooltip_expr = NULL,
  score = NULL,
  title = NULL
)
```

## Arguments

- data:

  a tibble with one row per component

- fld_category:

  bare column name for the category (fill)

- fld_height:

  bare column name for the bar height (score)

- fld_width:

  bare column name for the bar width

- tooltip_expr:

  optional glue string for tooltip text

- score:

  optional pre-computed weighted-mean score for the center

- title:

  optional plot title

## Value

a ggplot
