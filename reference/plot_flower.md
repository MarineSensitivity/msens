# Flower plot of component scores (interactive or static)

Builds a
[`ggplot_flower()`](http://marinesensitivity.org/msens/reference/ggplot_flower.md)
and optionally wraps it as an interactive
[`ggiraph::girafe`](https://davidgohel.github.io/ggiraph/reference/girafe.html)
for HTML output. Pass `interactive = FALSE` for pdf/docx.

## Usage

``` r
plot_flower(
  data,
  fld_category,
  fld_height,
  fld_width,
  tooltip_expr = NULL,
  score = NULL,
  title = NULL,
  interactive = TRUE
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

- interactive:

  logical; if TRUE (default) return a girafe htmlwidget, otherwise
  return the plain ggplot

## Value

a girafe htmlwidget or a ggplot
