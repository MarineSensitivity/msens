# Add symbol label layers from sf point data

Auto-detects text_anchor, text_justify, text_offset_right,
text_offset_down columns in the source sf for per-feature label
placement.

## Usage

``` r
add_pmlabel(m, labels)
```

## Arguments

- m:

  map or map_proxy

- labels:

  list of label specs, each with: source (sf), text_field, and
  optionally id, text_color, text_size, text_font, text_halo_color,
  text_halo_width, text_halo_blur, text_line_height, text_allow_overlap,
  filter, text_anchor, text_justify, text_offset

## Value

map (pipeable)
