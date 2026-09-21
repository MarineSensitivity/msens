# place.R — a PLACE is a polygon plus the cells it covers, on ONE grid
#
# Three implementations of "which cells does this polygon touch?" used to coexist:
# the v8 `cell`-table bbox select, the v1-v7 `terra::extract(exact = TRUE)` read of
# the 0-360 cell-id raster, and the zone builder's `exactextractr` coverage. They
# agreed to within a percentage point and never exactly, so the same drawn polygon
# reported different N cells, different area and different scores depending on which
# release was open -- and the browser would have made a fourth.
#
# `cells_in_polygon_grid()` is the ONE rule: pure arithmetic on a grid spec. No
# `cell` table, no raster, no GDAL extension. Given the grid registry's numbers it
# is reproducible in any language, which is the point -- `inst/fixtures/places/*.json`
# carries a polygon, the grid it was computed on and the expected `(cell_id, pct)`
# for each rule, so the TypeScript twin asserts the same bytes.

# Resolve whatever the caller passed into a grid spec list.
#
# A bare `grid_id` is the common case; a full spec (from `grid_spec_for()`) is
# accepted so a caller can pin a grid read from a cell-id COG. Anything missing a
# field is an error rather than a default: silently assuming 7200 columns is how a
# v7 polygon lands in the Arctic.
.as_grid <- function(grid) {
  if (is.character(grid) && length(grid) == 1L) return(grid_spec_for(grid))
  if (!is.list(grid))
    stop("`grid` must be a grid_id or a grid spec list from grid_spec_for()", call. = FALSE)
  need <- c("nc", "nr", "xmin", "ymax", "resx", "resy")
  if (length(miss <- setdiff(need, names(grid))))
    stop(sprintf("grid spec is missing: %s", paste(miss, collapse = ", ")), call. = FALSE)
  grid$nc <- as.integer(grid$nc); grid$nr <- as.integer(grid$nr)
  grid$lon360 <- isTRUE(grid$lon360)
  grid
}

# Does the grid close on itself in longitude? `global05` does (7200 x 0.05 = 360),
# `usa05` does not (3103 x 0.05 = 155.15 deg, a window from 141.10 E to 296.25 E).
# A wrapping grid may take a column index outside 1..nc and fold it; a windowed one
# must DROP it, or a polygon just west of 141.10 E would fold onto the US east coast.
.grid_wraps <- function(grid) isTRUE(abs(grid$nc * grid$resx - 360) < 1e-9)

# Put one ring's longitudes into the grid's own frame and keep every EDGE short.
#
# A polygon crossing the antimeridian arrives with vertices at 179 and -179. Read
# planar, that edge runs 358 degrees the wrong way around the world and the polygon
# becomes its own complement -- the classic silent antimeridian bug. The fix is
# local, not global: normalise the FIRST vertex into [xmin, xmin+360), then walk the
# ring adding or subtracting 360 whenever a step would exceed 180 degrees. The ring
# may end up outside [xmin, xmin+360); that is intended, and the column arithmetic
# below folds it back.
.ring_unwrap <- function(m, xmin) {
  lon <- xmin + ((m[, 1] - xmin) %% 360)
  n   <- length(lon)
  if (n > 1L) for (i in 2:n) {
    d <- lon[i] - lon[i - 1L]
    if (d >  180) lon[i] <- lon[i] - 360
    if (d < -180) lon[i] <- lon[i] + 360
  }
  m[, 1] <- lon
  m
}

# apply a ring function through POLYGON / MULTIPOLYGON structure
.map_rings <- function(g, f) {
  if (inherits(g, "MULTIPOLYGON"))
    sf::st_multipolygon(lapply(unclass(g), function(p) lapply(p, f)))
  else if (inherits(g, "POLYGON"))
    sf::st_polygon(lapply(unclass(g), f))
  else
    stop("place geometry must be a POLYGON or MULTIPOLYGON (got ",
         paste(class(g), collapse = "/"), ")", call. = FALSE)
}

# The percent rule, in one place so the fixtures pin it, and agreed with the
# TypeScript twin: SNAP the percent to 9 decimals, then round HALF-TO-EVEN, then
# keep only pct > 0.
#
# `round()` is already half-to-even in R, which is what `.cells_in_polygon_db()` has
# always used and therefore what every published number was computed with.
# JavaScript's `Math.round` is half-UP, so a cell covered 2.5 % rounds to 2 here and
# 3 there -- `inst/fixtures/places/half_cell_even.json` and the shared
# `half-even-2p5-global05.json` make that disagreement a test failure rather than a
# silent one-cell drift.
#
# The 9-decimal snap comes FIRST because a drawn rectangle produces exact halves
# that arrive from polygon clipping as 2.4999999999 or 2.5000000001. Without it, a
# knife-edge cell is a float coin toss and the two languages disagree at random --
# which is worse than either rule, since it cannot be reproduced. 9 decimals is far
# below any real geometry (1e-9 % of a 0.0025 deg2 cell) and far above the clipping
# noise. NOTE the place CODEC deliberately does not snap: there both sides round the
# same IEEE double and a snap would only add a second place to disagree.
.pct_round <- function(x) round(round(x, 9))

#' Cells a polygon covers, from the grid definition alone
#'
#' Returns `cell_id` + `pct_covered` (0-100) by pure arithmetic on a grid spec: the
#' polygon is intersected with the 0.05-degree squares the grid defines, planar in
#' degrees, and `pct_covered` is `round(area / (resx * resy) * 100)` with cells
#' rounding to 0 dropped. No `cell` table is read and no raster is opened, so the
#' answer depends on nothing but the polygon and the six numbers in the grid
#' registry -- which is what lets a browser reproduce it.
#'
#' **Antimeridian.** Each ring is brought into the grid's own longitude frame and
#' then unwrapped so no edge spans more than 180 degrees, which is the only way a
#' polygon straddling 180 stays itself rather than becoming its complement. A grid
#' that closes in longitude (`global05`: 7200 x 0.05 = 360) folds out-of-range
#' columns back with a modulo; a windowed grid (`usa05`, 141.10 E to 296.25 E) drops
#' them, because there is no cell there to fold onto.
#'
#' **This is the definition, not an approximation of one.** [cells_in_polygon()]
#' delegates to it for every database connection, so the drawn-polygon cell set, the
#' Program-Area cell set and the browser's cell set are one set.
#'
#' @param poly an `sf`, `sfc` or `sfg` polygon / multipolygon (EPSG:4326, or
#'   transformable to it)
#' @param grid a `grid_id` (`"usa05"`, `"global05"`) or a spec list from
#'   [grid_spec_for()]
#' @return a tibble with `cell_id` (integer) and `pct_covered` (numeric 1-100),
#'   ordered by `cell_id`
#' @examples
#' # a rectangle snapped to cell edges: whole cells only
#' p <- sf::st_sfc(sf::st_polygon(list(cbind(
#'   c(-90, -89.9, -89.9, -90, -90), c(27, 27, 27.1, 27.1, 27)))), crs = 4326)
#' cells_in_polygon_grid(p, "global05")
#' @importFrom sf st_geometry st_transform st_union st_sfc st_polygon st_multipolygon
#'   st_bbox st_set_crs st_intersects st_covered_by st_intersection st_area st_is_empty
#'   st_cast st_crs
#' @importFrom tibble tibble
#' @export
#' @concept place
cells_in_polygon_grid <- function(poly, grid) {
  grid  <- .as_grid(grid)
  empty <- tibble::tibble(cell_id = integer(), pct_covered = numeric())

  g <- sf::st_geometry(poly)
  if (!length(g)) return(empty)
  if (!is.na(sf::st_crs(g)) && sf::st_crs(g) != sf::st_crs(4326))
    g <- sf::st_transform(g, 4326)
  # drop the CRS before any topology: everything here is planar in degrees by
  # definition, and leaving EPSG:4326 on would route st_union/st_intersection
  # through s2, which rejects rings a rounded coastline legitimately contains
  # ("Loop 0 is not valid: edge N is degenerate") and measures area on a sphere
  g <- sf::st_set_crs(g, NA_character_)
  # Cast to parts, THEN union: overlapping rings of one place count once rather
  # than twice, and the result is a valid geometry whatever arrived.
  #
  # A MULTIPOLYGON whose parts overlap is not a valid MULTIPOLYGON -- but it is
  # exactly what a hand-drawn two-stroke place is. `st_union()` on the multipolygon
  # as a single feature is a no-op that leaves the invalidity in place, and GEOS
  # then throws from the INTERSECTION a hundred lines later ("TopologyException:
  # side location conflict"), which reads like a bug in the cell arithmetic. Casting
  # first makes the union do real work and repairs the overlap where it happens.
  g <- sf::st_union(suppressWarnings(sf::st_cast(g, "POLYGON")))
  g <- g[!sf::st_is_empty(g)]
  if (!length(g)) return(empty)

  gu <- sf::st_sfc(.map_rings(g[[1]], function(m) .ring_unwrap(m, grid$xmin)))
  bb <- sf::st_bbox(gu)

  nc <- grid$nc; nr <- grid$nr
  # top-left origin, 1-based, row-major -- the same arithmetic as cell_lonlat()
  cols <- seq(floor((bb[["xmin"]] - grid$xmin) / grid$resx) + 1,
              floor((bb[["xmax"]] - grid$xmin) / grid$resx) + 1)
  rows <- seq(floor((grid$ymax - bb[["ymax"]]) / grid$resy) + 1,
              floor((grid$ymax - bb[["ymin"]]) / grid$resy) + 1)
  rows <- rows[rows >= 1L & rows <= nr]
  if (.grid_wraps(grid)) {
    # a polygon wider than the world would revisit columns; cap at one full turn
    if (length(cols) > nc) cols <- cols[seq_len(nc)]
  } else {
    cols <- cols[cols >= 1L & cols <= nc]
  }
  if (!length(cols) || !length(rows)) return(empty)

  gr    <- expand.grid(col = cols, row = rows)
  lon_c <- grid$xmin + (gr$col - 0.5) * grid$resx     # at the UNWRAPPED position
  lat_c <- grid$ymax - (gr$row - 0.5) * grid$resy
  hx    <- grid$resx / 2; hy <- grid$resy / 2

  gp <- gu                                            # already CRS-free (planar)
  bx <- sf::st_sfc(lapply(seq_along(lon_c), function(i) sf::st_polygon(list(cbind(
    lon_c[i] + c(-hx, hx, hx, -hx, -hx),
    lat_c[i] + c(-hy, -hy, hy, hy, -hy))))))

  hit <- sf::st_intersects(bx, gp, sparse = FALSE)[, 1]
  if (!any(hit)) return(empty)

  idx <- which(hit)
  pct <- rep(NA_real_, length(idx))
  # a box entirely inside the polygon is exactly 100 %: take it without an area
  # computation, so a whole-cell answer never drifts to 99 on floating-point noise
  full <- sf::st_covered_by(bx[idx], gp, sparse = FALSE)[, 1]
  pct[full] <- 100
  if (any(!full)) {
    inter    <- suppressWarnings(sf::st_intersection(bx[idx][!full], gp))
    pct[!full] <- .pct_round(as.numeric(sf::st_area(inter)) / (grid$resx * grid$resy) * 100)
  }

  col_w   <- if (.grid_wraps(grid)) ((gr$col[idx] - 1L) %% nc) + 1L else gr$col[idx]
  cell_id <- as.integer((gr$row[idx] - 1L) * nc + col_w)

  keep <- !is.na(pct) & pct > 0
  if (!any(keep)) return(empty)
  # an unwrapped ring can revisit a column band on a wrapping grid; sum, cap at 100
  d <- stats::aggregate(list(pct_covered = pct[keep]),
                        by = list(cell_id = cell_id[keep]), FUN = sum)
  d <- d[order(d$cell_id), , drop = FALSE]
  tibble::tibble(cell_id = as.integer(d$cell_id),
                 pct_covered = pmin(as.numeric(d$pct_covered), 100))
}

#' The grid a release's `cell_id` values index, read from the connection
#'
#' Resolution order: the `cell_grid` table written beside `cell_model`, then the
#' presence of `lon`/`lat` on `cell` (v8+ carry them, v1-v7 do not). Never guessed
#' from row counts -- a wrong grid does not error, it relocates the polygon.
#'
#' @param con a DBI connection to a release database
#' @return a grid spec list from [grid_spec_for()]
#' @importFrom DBI dbListTables dbGetQuery
#' @export
#' @concept place
grid_for_con <- function(con) {
  gid <- tryCatch({
    if (!"cell_grid" %in% DBI::dbListTables(con)) NULL else
      DBI::dbGetQuery(con, "SELECT grid_id FROM cell_grid LIMIT 1")$grid_id[1]
  }, error = function(e) NULL)
  if (!is.null(gid) && !is.na(gid) && gid %in% grid_registry()$grid_id)
    return(grid_spec_for(gid))
  grid_spec_for(if (.cell_has_lonlat(con)) "global05" else "usa05")
}

#' Restrict a cell set to the release's study area
#'
#' D7b: a custom place is clipped to the cells that actually exist in the release's
#' `cell` table with `coalesce(in_usa, TRUE)`. Everything downstream -- scores,
#' species, area, N cells -- then runs on ONE cell set, and land and foreign waters
#' never enter a coverage-blended score as zeros.
#'
#' `coalesce(in_usa, TRUE)`, not `in_usa`: v1-v7 have no such column, and a release
#' whose `cell` table is already the study area must not be emptied by its absence.
#'
#' @param con a DBI connection to a release database
#' @param cells a tibble with `cell_id` and `pct_covered`
#' @return the same tibble, keeping only in-study-area cells (column order and
#'   `pct_covered` preserved), ordered by `cell_id`
#' @importFrom DBI dbGetQuery dbListFields
#' @importFrom tibble as_tibble
#' @export
#' @concept place
cells_in_study_area <- function(con, cells) {
  stopifnot(all(c("cell_id", "pct_covered") %in% names(cells)))
  if (!nrow(cells)) return(tibble::as_tibble(cells))
  in_usa <- if ("in_usa" %in% DBI::dbListFields(con, "cell")) "COALESCE(c.in_usa, TRUE)" else "TRUE"
  ids <- DBI::dbGetQuery(con, sprintf(
    "SELECT c.cell_id FROM cell c WHERE %s AND c.cell_id IN (%s)",
    in_usa, paste(as.integer(cells$cell_id), collapse = ", ")))$cell_id
  out <- cells[cells$cell_id %in% as.integer(ids), , drop = FALSE]
  tibble::as_tibble(out[order(out$cell_id), , drop = FALSE])
}

#' Read a place fixture
#'
#' The fixtures in `inst/fixtures/places/` are the cross-language contract for
#' [cells_in_polygon_grid()]: each carries a polygon, the grid spec it was computed
#' on, and the expected `(cell_id, pct)`. R and TypeScript read the same bytes, so
#' neither side can quietly adopt its own rounding or antimeridian rule.
#'
#' @param id fixture id (file basename without `.json`), or a path to one
#' @return a list with `id`, `rule`, `grid`, `geometry` (an `sfc`) and `expected`
#'   (a tibble of `cell_id`, `pct`)
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom sf st_read
#' @importFrom tibble tibble
#' @export
#' @concept place
place_fixture <- function(id) {
  path <- if (file.exists(id)) id else
    system.file("fixtures", "places", paste0(id, ".json"), package = "msens")
  if (!nzchar(path) || !file.exists(path))
    stop(sprintf("no place fixture '%s'", id), call. = FALSE)
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # the shared shape, tolerant on the three points the two loaders differ: the
  # geometry may be `geometry` or `polygon`; `grid` may name its own `grid_id` or
  # carry it at the top level; `expected` may be [[cell_id, pct], ...] pairs or
  # [{cell_id, pct}, ...] objects. Tolerance here costs one function; a fixture
  # each side can only read half of costs the whole point of sharing them.
  g <- x$grid
  g <- lapply(g, function(v) if (is.list(v)) unlist(v) else v)
  g$grid_id <- g$grid_id %||% x$grid_id
  e <- x$expected
  pair <- function(r) if (!is.null(r$cell_id)) c(r$cell_id, r$pct) else
    as.numeric(unlist(r))[1:2]
  m <- if (!length(e)) matrix(numeric(), 0, 2) else
    do.call(rbind, lapply(e, pair))
  list(id       = x$id,
       rule     = x$rule %||% x$note,
       grid     = g,
       geometry = geojson_sfc(x$geometry %||% x$polygon),
       expected = tibble::tibble(cell_id = as.integer(m[, 1]),
                                 pct     = as.numeric(m[, 2])))
}

#' Build an `sfc` from parsed GeoJSON coordinate arrays
#'
#' Deliberately NOT `sf::st_read()` on a GeoJSON string: that goes through GDAL and
#' then s2, which rejects a ring carrying a duplicate vertex — something a
#' coordinate-rounded coastline produces routinely, and something a planar cell
#' intersection does not care about at all. Reading the arrays directly also means
#' the fixture's `geometry` is exactly the numbers a TypeScript twin parses, with no
#' library in between to normalise them differently.
#'
#' @param x a parsed GeoJSON geometry (`jsonlite::fromJSON(simplifyVector = FALSE)`),
#'   of type `Polygon` or `MultiPolygon`
#' @return an `sfc` in EPSG:4326
#' @importFrom sf st_sfc st_polygon st_multipolygon
#' @export
#' @concept place
geojson_sfc <- function(x) {
  ring <- function(r) do.call(rbind, lapply(r, function(p) as.numeric(unlist(p))[1:2]))
  g <- switch(
    x$type,
    Polygon      = sf::st_polygon(lapply(x$coordinates, ring)),
    MultiPolygon = sf::st_multipolygon(
      lapply(x$coordinates, function(p) lapply(p, ring))),
    stop(sprintf("place geometry must be Polygon or MultiPolygon (got '%s')", x$type),
         call. = FALSE))
  sf::st_sfc(g, crs = 4326)
}

#' Every place fixture id
#'
#' @return character vector of fixture ids
#' @export
#' @concept place
place_fixture_ids <- function() {
  d <- system.file("fixtures", "places", package = "msens")
  if (!nzchar(d)) return(character())
  sort(sub("\\.json$", "", basename(list.files(d, pattern = "\\.json$"))))
}
