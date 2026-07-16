# Flag hexes inside a polygon (H3 polyfill membership)

Adds a BOOLEAN column `col` to a DuckDB hex table (with a `hex_id`
BIGINT column), TRUE where the hex is inside `poly`. Rather than a
point-in-polygon test over every hex (which scales with the grid —
prohibitive at ~79M global hexes), this **polyfills** the polygon at
`res` to enumerate the in-polygon hexes directly (center containment),
then semi-joins. The multipolygon is exploded with `ST_Dump` because
`h3_polygon_wkb_to_cells` takes single polygons. Cost is polygon-driven
(fixed) rather than grid-driven, so it is the same whether the grid is a
region or the whole globe.

## Usage

``` r
hex_add_membership(con, hex_tbl, poly, col, res = HEX_RES, buffer = TRUE)
```

## Arguments

- con:

  a DuckDB connection (h3 + spatial loaded on demand)

- hex_tbl:

  name of the DuckDB hex table (with `hex_id` BIGINT)

- poly:

  path to a polygon file readable by DuckDB `ST_Read` (e.g. `.gpkg`)

- col:

  name of the boolean membership column to add

- res:

  H3 resolution the hex grid was built at (default
  [HEX_RES](http://marinesensitivity.org/msens/reference/HEX_RES.md))

- buffer:

  if `TRUE` (default) buffer the polygon outward by one hex circumradius
  before polyfilling, so hexes that *overlap* the boundary (not only
  those centred inside) are captured — polyfill is centroid-inclusion,
  so without the buffer up to a half-hex-wide fringe along the boundary
  is missed

## Value

invisibly, `hex_tbl`
