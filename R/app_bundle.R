# app_bundle.R — normalize at publish, not in the browser (master-plan D5)
#
# Both Shiny apps carry a per-version adapter: two grids, `mdl_seq` vs `mdl_key`,
# `value` vs `val` vs both, `is_ok` vs `is_valid_usa`, unsuffixed v1/v2 zone tables,
# a `taxon_model` self-edge on one side only, `model_asset` vs `native_asset`. Every
# branch has a production bug recorded behind it, and every one of them already has
# a resolver and a test HERE. Re-implementing them in TypeScript doubles the surface
# and halves the tests.
#
# So the release writes ONE small, version-independent contract under `{ver}/app/`
# and the quirks stay where the fixtures are. The browser reads `app/` and never
# learns which generation it is looking at — with one deliberate exception, the
# `cell_model` id field, which the manifest already states.

#' The zone types that may become a drawable unit, best first
#'
#' Master-plan **D17** (Ben, 2026-09-22): a release publishes **at most one**
#' drawable unit — Program Areas, or Planning Areas on v1, which has no Program
#' Areas. Never subregions, never ecoregions, whatever the data would support: those
#' are camera presets and scoring context, not places a user draws a report for.
#' Their scores stay in `boot$zones` untouched.
#'
#' First match wins, so a later decision is one line here rather than a rewrite of
#' [app_units()].
#'
#' @return a character vector of zone types, in preference order
#' @examples
#' APP_UNIT_TYPES
#' @export
#' @concept app
APP_UNIT_TYPES <- c("programarea", "planarea")

.APP_SCHEMA    <- 1L
.APP_TILE_SIDE <- 50L

# ---- schema validation -------------------------------------------------------

#' Path to a published `app/` JSON Schema
#'
#' @param name one of `boot`, `taxa`, `taxon`, `alias`, `table`, `manifest`
#' @return a file path
#' @export
#' @concept app
app_schema_path <- function(name) {
  p <- system.file("schema", sprintf("app_%s.schema.json", name), package = "msens")
  if (!nzchar(p))
    stop(sprintf("no schema 'app_%s.schema.json'; known: %s", name,
                 paste(app_schema_names(), collapse = ", ")), call. = FALSE)
  p
}

#' @rdname app_schema_path
#' @return for `app_schema_names()`, the available schema names
#' @export
#' @concept app
app_schema_names <- function() {
  d <- system.file("schema", package = "msens")
  if (!nzchar(d)) return(character())
  # gsub, not sub: sub() replaces the FIRST alternative only, so "app_boot.schema.json"
  # came back as "boot.schema.json" and every path built from it doubled the suffix
  sort(gsub("^app_|\\.schema\\.json$", "", basename(list.files(d, "^app_.*\\.schema\\.json$"))))
}

#' Validate an object against its published `app/` schema
#'
#' Every builder validates its own output before it is written, so a shape change
#' fails in the notebook that made it rather than in a browser three steps later.
#' The schemas are `additionalProperties: false` at the top level on purpose: a key
#' the contract does not name is a key the app will not read, and shipping it is how
#' a bundle quietly grows.
#'
#' @param x the object (a list, as [jsonlite::toJSON()] would see it)
#' @param name schema name, see [app_schema_path()]
#' @param what label used in the error message
#' @return `x`, invisibly; errors with the failing JSON pointers
#' @importFrom jsonlite toJSON
#' @export
#' @concept app
app_validate <- function(x, name, what = name) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE))
    stop("package 'jsonvalidate' is required to validate an app bundle", call. = FALSE)
  json <- app_json(x)
  ok <- jsonvalidate::json_validate(json, app_schema_path(name), engine = "ajv",
                                    verbose = TRUE, greedy = TRUE)
  if (!isTRUE(ok)) {
    e <- attr(ok, "errors")
    msg <- if (is.data.frame(e) && nrow(e))
      paste(sprintf("  %s %s", e$instancePath, e$message), collapse = "\n") else
        "(no detail)"
    stop(sprintf("%s does not satisfy app_%s.schema.json:\n%s", what, name, msg),
         call. = FALSE)
  }
  invisible(x)
}

# An empty R list serialises to `[]`, but the schemas (and the app) expect `{}`
# wherever a JSON OBJECT is declared. `setNames(list(), character())` is the only
# form jsonlite writes as an empty object, so every object-valued field goes
# through this rather than relying on there happening to be at least one key.
.obj <- function(x = list()) {
  if (!length(x)) return(stats::setNames(list(), character()))
  if (is.null(names(x))) stop("an object-valued field needs names", call. = FALSE)
  x
}

#' Serialize an `app/` object exactly as it is published
#'
#' One serializer so the bytes that were validated are the bytes that are written:
#' `auto_unbox` (a length-1 vector is a scalar, as the schemas declare),
#' `digits = NA` (full double precision — a rounded `rescale` changes a colour ramp),
#' `null = "null"` and `na = "null"` (absent is `null`, never the string `"NA"`).
#'
#' @param x an object
#' @param pretty pretty-print (default `FALSE`: these are machine objects)
#' @return a single JSON string
#' @importFrom jsonlite toJSON
#' @export
#' @concept app
app_json <- function(x, pretty = FALSE)
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null",
                                na = "null", pretty = pretty))

# ---- small shared rules ------------------------------------------------------

# the shard of a key: the trailing integer mod 256, hex. No digits -> "00".
.shard_of <- function(key) {
  m <- regmatches(key, regexpr("[0-9]+$", key))
  n <- rep(0, length(key))
  hit <- regexpr("[0-9]+$", key) > 0
  # as.numeric, not as.integer: a WoRMS aphia id fits, but a future 11-digit id
  # would silently become NA as an integer and collapse every such taxon onto "00"
  n[hit] <- as.numeric(m) %% 256
  sprintf("%02x", as.integer(n))
}

# the metric keys that actually have cell rows. Derived, never a hardcoded list:
# `_ecoregion_min`, `_ecoregion_max` and `_prepctareaweighting` have zero
# `cell_metric` rows, and a UI iterating `metric` shows them as empty layers.
.app_cell_metric_keys <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT m.metric_key
      FROM metric m
     WHERE m.metric_seq IN (SELECT DISTINCT metric_seq FROM cell_metric)
     ORDER BY m.metric_seq")$metric_key
}

# the zone table serving a `fld`, preferring the version-suffixed name.
#' Zone table name for a spatial unit
#'
#' `SELECT DISTINCT tbl FROM zone WHERE fld = ...`, preferring a name ending in the
#' release's own suffix. Never `glue("ply_subregions_2026_{ver}")`: v1 and v2 zone
#' tables are UNSUFFIXED, so the guessed name matched nothing, the subregion cache
#' was written empty and never healed, and the choropleth came up blank with an
#' `InfM/-InfM` legend from `range(numeric(0))`.
#'
#' @param con a DBI connection to a release database
#' @param fld zone field, e.g. `"programarea_key"`
#' @param ver version label, used only to prefer a suffixed name
#' @return a table name, or `NA_character_` when the release has no such unit
#' @importFrom DBI dbGetQuery dbQuoteString
#' @export
#' @concept app
zone_tbl_for <- function(con, fld, ver = NULL) {
  tb <- DBI::dbGetQuery(con, sprintf(
    "SELECT DISTINCT tbl FROM zone WHERE fld = %s ORDER BY tbl",
    DBI::dbQuoteString(con, fld)))$tbl
  if (!length(tb)) return(NA_character_)
  if (!is.null(ver) && any(hit <- grepl(paste0("_", ver, "$"), tb))) return(tb[hit][1])
  tb[length(tb)]
}

# the keys of a field that carry a `score_%` metric -- the same set app_units()
# gates a drawable unit on. A choropleth is drawn from that metric, so a zone
# without one has nothing to draw.
.app_scored_keys <- function(con) {
  vz <- sdm_val_col(con, "zone")
  d <- DBI::dbGetQuery(con, glue::glue("
    SELECT DISTINCT z.fld, CAST(z.{vz} AS VARCHAR) AS zkey
      FROM zone z JOIN zone_metric zm USING (zone_seq)
      JOIN metric m USING (metric_seq)
     WHERE m.metric_key LIKE 'score!_%' ESCAPE '!'"))
  split(d$zkey, d$fld)
}

#' The ONE zone table each spatial unit publishes
#'
#' A release may carry more than one `zone.tbl` for a single `fld`. v2 has two for
#' `subregion_key` — `ply_subregions_2025` (AK, AKL48, L48, USA) and
#' `ply_subregions_2026` (AK, GA, PA, USA), two keys in common — and because
#' `app_zone_taxon()` dropped `zone_tbl`, `(zone_fld, zone_value, key)` came out
#' duplicated **13,077 times**: the same taxon in the same subregion with two
#' different `area_km2`. The app would have listed every taxon twice.
#'
#' @section The manifest decides:
#' **The release's own `manifest.json` names the table.** Its `zones[]` rows carry
#' `fld` *and* `tbl`, and v2's says `subregion_key -> ply_subregions_2025`
#' (`zone_set_key: subregion_2025-06`). That is the first and normally the only
#' evidence, and it is already an input of [app_bundle_build()].
#'
#' Everything else is a **check**, not a second opinion:
#' \itemize{
#'   \item `geom_keys` — the keys in the published geometry — must AGREE with the
#'     manifest. A disagreement means the notebook was handed the wrong GeoPackage,
#'     which is a defect to stop on, not a tie to break.
#'   \item `zone_sets` — the registry's `source` basename — is a cross-check,
#'     recorded in `why`, never a chooser.
#' }
#'
#' There is no fallback guess. A field with two tables and no manifest row is an
#' **error**: "most recent `date_created`" looked reasonable and picked
#' `ply_subregions_2026` for v2, whose keys match no published geometry — two
#' defensible answers depending on who called, which is the ambiguity this rule
#' exists to remove.
#'
#' The winner is recorded in `boot$units[].zone_tbl` so the notebook's geometry
#' check can assert its GeoPackage holds the same keys.
#'
#' @param con a DBI connection to a release database
#' @param manifest the release's **published** manifest ([atlas_manifest()], or the
#'   `manifest.json` beside the database); its `zones[]` rows name the table per
#'   field. A manifest rebuilt by [manifest_build()] is **not** acceptable here — it
#'   has already collapsed the rows this needs — and one whose `zones` lack
#'   `zone_set_key` is refused.
#' @param geom_keys named list `zone_type -> keys present in the published geometry`,
#'   checked against the manifest's choice
#' @param zone_sets the zone-set registry (`data/zone_sets.csv`), cross-check only
#' @return a data frame `fld`, `tbl`, `why`, `n_tables`, one row per field
#' @importFrom DBI dbGetQuery dbListFields
#' @export
#' @concept app
app_zone_tbl <- function(con, manifest = NULL, geom_keys = list(), zone_sets = NULL) {
  vz <- sdm_val_col(con, "zone")
  scored <- tryCatch(.app_scored_keys(con), error = function(e) list())
  d <- DBI::dbGetQuery(con, glue::glue(
    "SELECT fld, tbl, count(*) AS n,
            string_agg(DISTINCT CAST({vz} AS VARCHAR), ',') AS keys
       FROM zone GROUP BY 1, 2 ORDER BY 1, 2"))
  if (!nrow(d)) return(data.frame(fld = character(), tbl = character(),
                                  why = character(), n_tables = integer()))

  mz <- manifest$zones
  have_mz <- !is.null(mz) && is.data.frame(mz) && nrow(mz) &&
             all(c("fld", "tbl") %in% names(mz))
  # A PUBLISHED manifest carries `zone_set_key` on every zones row; one rebuilt from
  # the database by manifest_build() may not, and a rebuilt one is not an acceptable
  # input for the app path: it has already collapsed the very rows being asked about.
  # Refuse it rather than reading a choice it did not really make.
  if (have_mz && (!"zone_set_key" %in% names(mz) || any(is.na(mz$zone_set_key))))
    stop(paste0(
      "the manifest's `zones` rows are missing `zone_set_key`, so this is a ",
      "manifest rebuilt from the database rather than the release's published one.\n",
      "  Pass the PUBLISHED manifest.json: a rebuilt one has already collapsed the ",
      "rows this needs, and did so without the evidence to choose."), call. = FALSE)

  pick <- function(g) {
    fld  <- g$fld[1]
    type <- sub("_key$", "", fld)

    if (nrow(g) == 1L) {
      tbl <- g$tbl[1]; why <- "only table for this field"
    } else {
      row <- if (have_mz) mz[!is.na(mz$fld) & mz$fld == fld, , drop = FALSE] else NULL
      if (is.null(row) || !nrow(row) || is.na(row$tbl[1]))
        stop(sprintf(paste0(
          "`%s` has %d zone tables (%s) and the release manifest names none of ",
          "them.\n  Pass the manifest (its `zones[].tbl` is the deciding evidence). ",
          "There is no fallback: guessing by date picked ply_subregions_2026 for v2, ",
          "whose keys match no published geometry."),
          fld, nrow(g), paste(g$tbl, collapse = ", ")), call. = FALSE)
      if (length(unique(row$tbl)) > 1L)
        stop(sprintf(paste0(
          "the manifest lists %d rows for `%s` (%s), so it names no single table.\n",
          "  A published manifest carries ONE zones[] row per field; one rebuilt ",
          "from the database reproduces the ambiguity it is being asked to settle."),
          nrow(row), fld, paste(unique(row$tbl), collapse = ", ")), call. = FALSE)
      tbl <- row$tbl[1]
      if (!tbl %in% g$tbl)
        stop(sprintf(paste0(
          "the manifest names `%s` for `%s`, but the release's zone table holds ",
          "only: %s"), tbl, fld, paste(g$tbl, collapse = ", ")), call. = FALSE)
      why <- sprintf("named by manifest.json%s",
                     if ("zone_set_key" %in% names(row) && !is.na(row$zone_set_key[1]))
                       sprintf(" (zone_set_key %s)", row$zone_set_key[1]) else "")
    }

    # geometry keys must AGREE; a mismatch is the wrong GeoPackage, not a tie
    gk <- geom_keys[[type]]
    # D17: only the chosen unit's geometry decides anything. A caller may pass
    # geometries for every zone type (the notebook does); the extras belong to
    # types that will never be a unit, so checking them would stop a build over a
    # GeoPackage the app never opens.
    if (!type %in% APP_UNIT_TYPES && !is.null(gk) && length(gk)) {
      why <- paste0(why, "; not a drawable unit type: geometry ignored")
      gk <- NULL
    }
    if (!is.null(gk) && length(gk)) {
      # Against the SCORED keys, not the table's full key set, and only where a unit
      # will actually be published (master-plan D16). A field becomes a drawable unit
      # only when >= 2 of its zones carry a `score_%` metric; the geometry matters to
      # a unit and to nothing else. Measured: the subregion zones are scored on v8/v9
      # only (5 of 5), v6 0 of 4, v7/v7b only `FULL` (1 of 5). Checking the full table
      # stopped v1 ("in the geometry but NOT in that table: AT, GA, PA") and v4-v7b
      # ("AT") over fields that would never be a unit -- the right stop, the wrong
      # universe.
      sk <- sort(unique(as.character(scored[[fld]] %||% character())))
      if (length(sk) < 2L) {
        why <- paste0(why, sprintf("; no unit: %d of %d zones scored; geometry not checked",
                                   length(sk), g$n[match(tbl, g$tbl)]))
      } else {
        # SUBSET, not setequal: a key scored but not drawn -- the whole-study-area
        # rollups `USA` (v8) and `FULL` (v7) have no polygon -- is normal. A key in
        # the GEOMETRY that is not scored on this release is the other thing
        # entirely: the wrong GeoPackage.
        extra <- setdiff(gk, sk)
        if (length(extra))
          stop(sprintf(paste0(
            "the geometry given for `%s` does not match the table the manifest names.\n",
            "  manifest: %s -> %s\n  in the geometry but NOT in that table: %s\n",
            "  A mismatch means the wrong GeoPackage was handed in; it is not a tie to break."),
            fld, fld, tbl, paste(extra, collapse = ", ")), call. = FALSE)
        why <- paste0(why, "; geometry keys agree")
      }
    }

    # the registry is a cross-check, recorded, never a chooser
    if (!is.null(zone_sets) && nrow(zone_sets) && "source" %in% names(zone_sets)) {
      zt  <- zone_sets[zone_sets$zone_type %in% type, , drop = FALSE]
      src <- sub("[.][^.]*$", "", basename(as.character(zt$source)))
      if (length(src)) why <- paste0(why, if (tbl %in% src)
        "; zone-set registry agrees" else "; NOTE the zone-set registry lists no such source")
    }
    list(tbl = tbl, why = why)
  }

  out <- do.call(rbind, lapply(split(d, d$fld), function(g) {
    p <- pick(g)
    data.frame(fld = g$fld[1], tbl = p$tbl, why = p$why,
               n_tables = nrow(g), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# a WHERE fragment keeping only the chosen table of each field
.app_zone_where <- function(chosen, alias = "z") {
  if (is.null(chosen) || !nrow(chosen)) return("TRUE")
  paste(sprintf("(%s.fld = %s AND %s.tbl = %s)", alias, .sql_str(chosen$fld),
                alias, .sql_str(chosen$tbl)), collapse = " OR ")
}

# ---- boot.json ---------------------------------------------------------------

#' The 11-stop palettes the app must draw with
#'
#' The legend, the popup swatch and the choropleth bins have to equal what R drew
#' and what titiler rendered, so the ramps are shipped rather than re-derived by a
#' JavaScript colour library interpolating in a different space.
#'
#' `spectral_r` is ColorBrewer Spectral reversed, interpolated in **Lab**
#' (`colorRampPalette(space = "Lab")`), which is what `msens::viz` uses. The
#' perceptual ramps come from `viridisLite`, whose values are matplotlib's own — the
#' same table titiler renders from. `grDevices::hcl.colors()` is deliberately not
#' used: it re-derives them in HCL and, on some builds, errors outright on `Cividis`.
#'
#' Unknown names are an error, never a silent substitution: a layer rendered in the
#' wrong ramp is a legend that lies.
#'
#' @param names colormap names to build (default: the four the contract lists)
#' @return a named list of 11 upper-case `#RRGGBB` strings each
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @concept app
app_palettes <- function(names = c("spectral_r", "viridis", "cividis", "magma")) {
  vl <- c(viridis = "D", magma = "A", inferno = "B", plasma = "C",
          cividis = "E", rocket = "F", mako = "G", turbo = "H")
  up <- function(x) toupper(substr(x, 1, 7))
  one <- function(nm) {
    if (nm %in% c("spectral_r", "spectral")) {
      p <- RColorBrewer::brewer.pal(11, "Spectral")
      if (nm == "spectral_r") p <- rev(p)
      return(up(grDevices::colorRampPalette(p, space = "Lab")(11)))
    }
    if (nm %in% base::names(vl)) {
      if (!requireNamespace("viridisLite", quietly = TRUE))
        stop(sprintf("package 'viridisLite' is required for the '%s' ramp", nm),
             call. = FALSE)
      return(up(viridisLite::viridis(11, option = vl[[nm]])))
    }
    stop(sprintf("no ramp for colormap '%s'; known: %s", nm,
                 paste(c("spectral", "spectral_r", base::names(vl)), collapse = ", ")),
         call. = FALSE)
  }
  stats::setNames(lapply(names, one), names)
}

# every colormap the release actually references, so the bundle ships the ramps it
# needs and no others (and fails loudly on one msens cannot reproduce)
.app_colormaps <- function(con, manifest) {
  cm <- character()
  if ("native_asset" %in% DBI::dbListTables(con))
    cm <- c(cm, DBI::dbGetQuery(con, "SELECT DISTINCT colormap FROM native_asset")$colormap)
  if (!is.null(manifest$metrics) && "colormap" %in% base::names(manifest$metrics))
    cm <- c(cm, manifest$metrics$colormap)
  sort(unique(c("spectral_r", cm[!is.na(cm) & nzchar(cm)])))
}

#' The drawable scored spatial units of a release
#'
#' A unit is offered iff (a) the manifest gives it PMTiles, (b) at least two of its
#' zones carry a composite `score_%` metric, and (c) at least two of those keys exist
#' in the published geometry. (c) is what excludes the whole-study-area rollups —
#' `USA` on v8, `FULL` on v7 — **without hardcoding either name**; pass
#' `geom_keys` to apply it, and it is skipped (with the reason recorded) when the
#' geometry is not available to the builder.
#'
#' @param con a DBI connection to a release database
#' @param manifest a manifest from [manifest_build()] / [atlas_manifest()], whose
#'   `zones` rows carry `fld` and `pmtiles`
#' @param geom_keys optional named list `zone_type -> character keys present in the
#'   published geometry`
#' @return a list of unit objects for `boot.json`
#' @importFrom DBI dbGetQuery
#' @export
#' @concept app
app_units <- function(con, manifest, geom_keys = list(), chosen = NULL) {
  z <- manifest$zones
  if (is.null(z) || !nrow(z) || !"pmtiles" %in% names(z)) return(list())
  vz <- sdm_val_col(con, "zone")
  if (is.null(chosen)) chosen <- app_zone_tbl(con, manifest, geom_keys)
  w <- .app_zone_where(chosen, "z")
  scored <- DBI::dbGetQuery(con, glue::glue("
    SELECT DISTINCT z.{vz} AS zkey, z.fld
      FROM zone z JOIN zone_metric zm USING (zone_seq)
      JOIN metric m USING (metric_seq)
     WHERE ({w}) AND m.metric_key LIKE 'score!_%' ESCAPE '!'"))
  lab <- c(programarea = "Program areas", planarea = "Planning areas",
           ecoregion   = "Ecoregions",    subregion = "Subregions")

  out <- lapply(seq_len(nrow(z)), function(i) {
    fld  <- z$fld[i]
    type <- sub("_key$", "", fld)
    ks   <- sort(unique(scored$zkey[which(!is.na(scored$fld) & scored$fld == fld)]))
    if (length(ks) < 2 || is.na(z$pmtiles[i])) return(NULL)
    if (!is.null(gk <- geom_keys[[type]])) {
      ks <- intersect(ks, as.character(gk))
      if (length(ks) < 2) return(NULL)
    }
    list(zone_type    = type,
         zone_set_key = if ("zone_set_key" %in% names(z)) z$zone_set_key[i] else NULL,
         fld          = fld,
         # the ONE table this unit publishes, so the notebook's geometry check can
         # assert its GeoPackage holds the same keys
         zone_tbl     = chosen$tbl[match(fld, chosen$fld)],
         label        = unname(if (type %in% names(lab)) lab[[type]] else
           paste0(toupper(substring(type, 1, 1)), substring(type, 2), "s")),
         pmtiles      = z$pmtiles[i],
         source_layer = type,       # the PMTiles layer is the zone TYPE, not the table
         keys         = as.list(ks))
  })
  out <- Filter(Negate(is.null), out)
  # ONE unit per field. `manifest$zones` has a row per (zone_set_key, tbl, fld), so a
  # release with two tables for one field -- v2's subregion_key -- produced TWO
  # units for it, offering the same picker entry twice with different key sets.
  out <- out[!duplicated(vapply(out, function(u) u$fld, ""))]
  # D17: AT MOST ONE unit per release, the first type in APP_UNIT_TYPES that
  # qualifies. v9 scores all four types and publishes only `programarea`; v1 has no
  # Program Areas and publishes `planarea`. A type that is scored but not drawable
  # keeps every one of its scores in boot$zones -- nothing is lost, it is just not
  # a place a user draws a report for.
  ty <- vapply(out, function(u) u$zone_type, "")
  pick <- APP_UNIT_TYPES[APP_UNIT_TYPES %in% ty][1]
  if (is.na(pick)) list() else out[ty == pick]
}

#' Per-zone summaries, so a report on a Program Area never loads a cell
#'
#' `n_cells` and the coverage-weighted `area_km2` come from `zone_cell` joined to
#' `cell`; `metrics` is that zone's `zone_metric` keyed by `metric_key` (never by
#' `metric_seq`, which is dropped and recreated every run). `coverage` carries the
#' v7.1-only `{component}_coverage` rows when the release has them and is `NULL`
#' otherwise — **optional by presence**, like the `methods` table.
#'
#' A component with no row is ABSENT, not zero: that is how the release records "not
#' reportable", and the app must render it as such.
#'
#' @param con a DBI connection
#' @param flds zone fields to summarise (default: every `fld` in `zone`)
#' @return a named list `zone_type -> list of zone objects`
#' @importFrom DBI dbGetQuery
#' @export
#' @concept app
app_zones <- function(con, flds = NULL, chosen = NULL) {
  vz <- sdm_val_col(con, "zone")
  vm <- sdm_val_col(con, "zone_metric")
  if (is.null(chosen)) chosen <- app_zone_tbl(con)
  # ONE table per field: v2 carries two for subregion_key, and merging them gave
  # boot$zones$subregion six rows with the two shared keys resolved arbitrarily
  w <- .app_zone_where(chosen, "z")
  if (is.null(flds)) flds <- DBI::dbGetQuery(con, glue::glue(
    "SELECT DISTINCT fld FROM zone z WHERE {w} ORDER BY fld"))$fld
  d <- DBI::dbGetQuery(con, glue::glue("
    SELECT z.fld, z.{vz} AS zkey, count(*) AS n_cells,
           sum(c.area_km2 * zc.pct_covered / 100.0) AS area_km2
      FROM zone z JOIN zone_cell zc USING (zone_seq) JOIN cell c USING (cell_id)
     WHERE {w}
     GROUP BY 1, 2 ORDER BY 1, 2"))
  m <- DBI::dbGetQuery(con, glue::glue("
    SELECT z.fld, z.{vz} AS zkey, mt.metric_key, zm.{vm} AS val
      FROM zone z JOIN zone_metric zm USING (zone_seq) JOIN metric mt USING (metric_seq)
     WHERE {w}
     ORDER BY 1, 2, 3"))
  # How many species rows this zone actually has. v8 scores subregion `AT` and gives
  # it 52,674 cells but publishes NO `zone_taxon` rows for it (v9 has 7,562). That is
  # a gap in the release's table, not ours: nothing is invented and the unit is not
  # dropped -- the score is real -- so the count is published and the app says "no
  # species table published for this zone" where it is 0.
  nt <- if ("zone_taxon" %in% DBI::dbListTables(con))
    DBI::dbGetQuery(con, "SELECT zone_fld AS fld, CAST(zone_value AS VARCHAR) AS zkey,
                                 count(*) AS n_taxa FROM zone_taxon GROUP BY 1, 2") else
      data.frame(fld = character(), zkey = character(), n_taxa = integer())
  is_cov <- grepl("_coverage$", m$metric_key)

  stats::setNames(lapply(flds, function(fld) {
    rows <- d[which(!is.na(d$fld) & d$fld == fld), , drop = FALSE]
    lapply(seq_len(nrow(rows)), function(i) {
      k  <- rows$zkey[i]
      mi <- m[which(!is.na(m$fld) & !is.na(m$zkey) & m$fld == fld & m$zkey == k),
              , drop = FALSE]
      sc <- mi[!grepl("_coverage$", mi$metric_key), , drop = FALSE]
      cv <- mi[ grepl("_coverage$", mi$metric_key), , drop = FALSE]
      ni <- nt$n_taxa[nt$fld == fld & nt$zkey == k]
      list(key      = k,
           n_cells  = as.integer(rows$n_cells[i]),
           area_km2 = as.numeric(rows$area_km2[i]),
           n_taxa   = if (length(ni)) as.integer(ni[1]) else 0L,
           metrics  = .obj(stats::setNames(as.list(as.numeric(sc$val)), sc$metric_key)),
           coverage = if (nrow(cv))
             stats::setNames(as.list(as.numeric(cv$val)),
                             sub("_coverage$", "", cv$metric_key)) else NULL)
    })
  }), sub("_key$", "", flds))
}

#' Build `app/boot.json`
#'
#' Everything the app needs for first paint with no WASM and no second request:
#' the release's identity and access, the grid, the study-area camera presets, the
#' drawable units and their keys, the cell layers with their COGs and rescales, every
#' zone's precomputed metrics, the versioned flower default, the datasets, the colour
#' ramps and the table manifest with a digest per table (the OPFS invalidation key).
#'
#' `flower_default` is **versioned here**, which retires the shared, unversioned
#' `scores/cache/flower_default_subregions.csv`: the first release to run wrote it
#' and every other release then read it, so the committed file holds 10 components
#' from a release that had 10 while v8/v9 have 8.
#'
#' @param con a DBI connection to a release database
#' @param ver version label
#' @param manifest a manifest from [manifest_build()]; `metrics` supplies the COGs
#'   and `zones` the PMTiles
#' @param tables named list of `name -> list(href, bytes, digest)`; see
#'   [app_table_manifest()]
#' @param geom_keys optional geometry keys per zone type, see [app_units()]
#' @param zone_sets the zone-set registry, used by [app_zone_tbl()] to choose the one
#'   table per field when a release carries more than one
#' @param chosen a precomputed [app_zone_tbl()] result, so every builder in one
#'   bundle agrees on the same table
#' @param built_at ISO timestamp to stamp (default: now, UTC, second precision)
#' @return a validated `boot.json` object
#' @importFrom DBI dbGetQuery dbListTables
#' @importFrom utils packageVersion
#' @export
#' @concept app
app_boot <- function(con, ver, manifest, tables = list(), geom_keys = list(),
                     zone_sets = NULL, chosen = NULL,
                     built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  if (is.null(chosen)) chosen <- app_zone_tbl(con, manifest, geom_keys, zone_sets)
  g   <- grid_spec_for(manifest$grid_id %||% grid_for_ver(ver))
  vm  <- sdm_val_col(con, "zone_metric")
  vz  <- sdm_val_col(con, "zone")

  # layers: only metrics that have cell rows, joined to the manifest's COGs on
  # metric_key. 16 of a release's 41 metric keys have ZERO cell_metric rows.
  keys <- .app_cell_metric_keys(con)
  met  <- manifest$metrics
  layers <- lapply(seq_along(keys), function(i) {
    k <- keys[i]
    r <- if (!is.null(met) && nrow(met))
      met[which(!is.na(met$metric_key) & met$metric_key == k), , drop = FALSE] else NULL
    by <- list()
    if (!is.null(r) && nrow(r) && "cog" %in% names(r)) {
      ok <- !is.na(r$cog)
      sub_col <- if ("subregion_key" %in% names(r)) r$subregion_key else rep("FULL", nrow(r))
      for (j in which(ok))
        by[[as.character(sub_col[j])]] <- list(
          cog = r$cog[j],
          rescale = c(as.numeric(r$rescale_min[j] %||% 0),
                      as.numeric(r$rescale_max[j] %||% 100)))
    }
    list(metric_key = k,
         label      = .metric_label(k, if (!is.null(r) && nrow(r)) r$description[1] else NA),
         category   = .metric_category(k),
         order      = i,
         colormap   = if (!is.null(r) && nrow(r) && "colormap" %in% names(r))
           r$colormap[1] else "spectral_r",
         by_subregion = .obj(by))
  })

  sa <- study_areas()
  out <- list(
    schema   = .APP_SCHEMA,
    ver      = ver,
    built_at = built_at,
    # THE one generation fact the contract allows the browser to need: which column
    # the cell_model join keys on. v8+ store a compact integer `mdl_id` and join
    # `model` back to the stable `mdl_key`; v1-v7 store `mdl_seq`, which `taxon`
    # already carries. Everything else about a release is normalised away.
    id_field = manifest$id_field %||% "mdl_key",
    msens    = as.character(utils::packageVersion("msens")),
    release  = list(title    = manifest$title %||% NULL,
                    status   = manifest$status %||% "released",
                    access   = manifest$access %||% atlas_access_default(manifest$status %||% "released"),
                    released = manifest$released %||% NULL),
    grid = list(grid_id = g$grid_id, nc = as.integer(g$nc), nr = as.integer(g$nr),
                xmin = as.numeric(g$xmin), ymax = as.numeric(g$ymax),
                resx = as.numeric(g$resx), resy = as.numeric(g$resy),
                lon360 = isTRUE(g$lon360), tile = list(size = .APP_TILE_SIDE)),
    study_areas = lapply(seq_len(nrow(sa)), function(i)
      list(key = sa$key[i], label = sa$label[i], lon = sa$lon[i], lat = sa$lat[i],
           zoom = sa$zoom[i], ecoregions = sa$ecoregions[i])),
    units          = app_units(con, manifest, geom_keys, chosen),
    layers         = layers,
    zones          = .obj(app_zones(con, chosen = chosen)),
    flower_default = .obj(app_flower_default(con)),
    datasets       = app_datasets(con),
    palettes       = app_palettes(.app_colormaps(con, manifest)),
    tables         = .obj(tables))

  # v7.1's methods table is OPTIONAL by presence; no other release has it.
  #
  # The SOURCE table is `release_method` -- that is what v7b's sdm.duckdb actually
  # holds, and what backfill_versions.qmd maps into `manifest$methods`. Looking only
  # for a table named `methods` found nothing on the real release and emitted no
  # block at all, silently: the one release with methods was the one release no dry
  # run had covered. `methods` is still accepted, for a release that already exposes
  # it under the manifest's name.
  mt <- intersect(c("release_method", "methods"), DBI::dbListTables(con))[1]
  if (!is.na(mt)) {
    md <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s ORDER BY method_key", mt))
    out$methods <- lapply(seq_len(nrow(md)), function(i)
      # `val`, not `value`: the BUNDLE never publishes the word `value` (the manifest
      # keeps its own spelling). A published object that says `value` is the one
      # thing the review checklist forbids by name.
      list(method_key = md$method_key[i], val = as.character(md$value[i]),
           description = md$description[i] %||% NULL))
  }
  app_validate(out, "boot", "boot.json")
}

# NULL-or-NA coalescing. The NA arm matters: a manifest read back from JSON turns an
# absent scalar into NA, not NULL, so a plain null-coalesce would happily publish
# "NA" as a release title.
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

.metric_label <- function(key, description = NA) {
  if (!is.na(description) && nzchar(description)) return(description)
  key
}

.metric_category <- function(key) {
  if (grepl("^score_", key)) return("composite")
  if (grepl("_ecoregion_rescaled$", key)) return("component")
  "raw"
}

#' Versioned flower-plot defaults, per subregion
#'
#' @param con a DBI connection
#' @return a named list `subregion_key -> list of {component, score}`
#' @importFrom DBI dbGetQuery
#' @export
#' @concept app
app_flower_default <- function(con) {
  vz <- sdm_val_col(con, "zone"); vm <- sdm_val_col(con, "zone_metric")
  d <- DBI::dbGetQuery(con, glue::glue("
    SELECT z.{vz} AS zkey, m.metric_key, zm.{vm} AS score
      FROM zone z JOIN zone_metric zm USING (zone_seq) JOIN metric m USING (metric_seq)
     WHERE z.fld = 'subregion_key'
       AND regexp_matches(m.metric_key, '_ecoregion_rescaled$')
     ORDER BY 1, 2"))
  if (!nrow(d)) return(.obj())
  d$component <- .component_of(d$metric_key)
  d <- d[d$component != "all", , drop = FALSE]
  stats::setNames(lapply(split(d, d$zkey), function(s)
    lapply(seq_len(nrow(s)), function(i)
      list(component = s$component[i], score = as.numeric(s$score[i])))),
    names(split(d, d$zkey)))
}

#' The release's datasets, normalised
#'
#' @param con a DBI connection
#' @return a list of dataset objects
#' @importFrom DBI dbGetQuery dbListFields
#' @export
#' @concept app
app_datasets <- function(con) {
  if (!"dataset" %in% DBI::dbListTables(con)) return(list())
  f <- DBI::dbListFields(con, "dataset")
  col <- function(nm, type) if (nm %in% f) nm else sprintf("CAST(NULL AS %s) AS %s", type, nm)
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT ds_key, %s, %s, %s, %s, %s, %s, %s FROM dataset ORDER BY ds_key",
    col("name_display", "VARCHAR"), col("value_info", "VARCHAR"),
    col("is_mask", "BOOLEAN"), col("on_grid", "BOOLEAN"),
    col("sort_order", "INTEGER"), col("citation", "VARCHAR"),
    col("link_info", "VARCHAR")))
  lapply(seq_len(nrow(d)), function(i) as.list(d[i, ]))
}

# ---- taxa.json / taxon shards / alias shards ---------------------------------

# the picker/scoring columns, resolved once per connection
.app_taxon_cols <- function(con) {
  f <- DBI::dbListFields(con, "taxon")
  list(
    f      = f,
    key    = if ("ms_merge_key" %in% f) "ms_merge_key" else "mdl_seq",
    marine = if ("is_marine" %in% f) "is_marine" else NA_character_,
    ok     = if ("is_ok" %in% f) "is_ok" else NA_character_,
    usa    = if ("is_valid_usa" %in% f) "is_valid_usa" else NA_character_,
    glob   = if ("is_valid_global" %in% f) "is_valid_global" else NA_character_)
}

# Cast an id column to VARCHAR without inventing a decimal point.
#
# v7 stores `taxon.taxon_id` as a DOUBLE, and `CAST(22725044.0 AS VARCHAR)` in DuckDB
# is the string "22725044.0" -- so the published contract carried a trailing ".0" on
# all 16,153 v7 rows, every WoRMS link built from it 404s, and a join against any
# other release's integer-typed id matches nothing. Route a DOUBLE or DECIMAL through
# a lossless integer type first. HUGEINT, not BIGINT: an id is not guaranteed to fit
# 63 bits forever and silently wrapping is the same class of bug.
.app_id_cast <- function(con, tbl, col, expr) {
  # describe the TABLE and look the column up by name: describing `SELECT t.taxon_id`
  # would need the caller's alias to be in scope, which it is not here
  ty <- tryCatch({
    d <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM %s LIMIT 0", tbl))
    d$column_type[match(col, d$column_name)]
  }, error = function(e) NA_character_)
  if (!is.na(ty) && grepl("^(DOUBLE|FLOAT|REAL|DECIMAL)", toupper(ty)))
    # ONLY when the value is whole. `CAST(12.7 AS HUGEINT)` ROUNDS, to 13 -- so a
    # fractional id would come out as a different, possibly existing id, and nothing
    # downstream could tell. A non-integral id is a data error; keep its own text so
    # the check below can name it rather than publishing a plausible lie.
    # NULL stays NULL: `x = floor(x)` is NULL for NULL and the ELSE branch casts it.
    # 2^53 + 1 and negatives go through HUGEINT unharmed.
    sprintf("CASE WHEN %s = floor(%s) THEN CAST(CAST(%s AS HUGEINT) AS VARCHAR)
                  ELSE CAST(%s AS VARCHAR) END", expr, expr, expr, expr)
  else sprintf("CAST(%s AS VARCHAR)", expr)
}

# Every published id must be digits only. A fractional taxon_id is a data error the
# RELEASE has to hear about: publishing a rounded neighbour would hide it forever,
# and the contract promises one shape to every consumer.
.app_assert_integral_ids <- function(d, what = "taxon_id") {
  x   <- d[[what]]
  bad <- which(!is.na(x) & !grepl("^-?[0-9]+$", x))
  n   <- length(bad)
  if (n) {
    ex <- utils::head(bad, 3)
    stop(sprintf(paste0(
      "%d of %d `%s` values are not integral, so the bundle was not written.\n",
      "  e.g. row %s: %s = %s%s\n",
      "  A non-integral id cannot be published: casting it to an integer ROUNDS ",
      "(12.7 -> 13), which is a different and possibly existing id. Fix it in the ",
      "release."),
      n, length(x), what, paste(ex, collapse = ", "),
      what, paste(x[ex], collapse = ", "),
      if (!is.null(d$sci)) paste0(" (", paste(d$sci[ex], collapse = ", "), ")") else ""),
      call. = FALSE)
  }
  invisible(n)
}

#' Normalise an id column to the text every generation must publish
#'
#' The R-side twin of the SQL cast. An id reaches a published object by several
#' routes — a `SELECT` this package writes, a `SELECT *` of a precomputed table, a
#' column already read into R — and only the first was covered, so v7's
#' `zone_taxon.parquet` still carried `taxon_id` as a DOUBLE and published
#' `"22725044.0"` while `taxon.parquet` beside it said `"22725044"`. One contract,
#' two spellings, and every join between them empty.
#'
#' Whole numbers become digits only; `NA` stays `NA`; a trailing `.0` on a string is
#' stripped (it means whole); anything genuinely fractional is an **error**, never a
#' rounded neighbour — see [app_taxon_table()].
#'
#' @param x an id vector (numeric, integer, integer64 or character)
#' @param what name used in the error message
#' @return a character vector
#' @examples
#' app_id_chr(c(137162, NA))          # "137162" NA
#' app_id_chr(c("22725044.0", "7"))   # "22725044" "7"
#' @export
#' @concept app
app_id_chr <- function(x, what = "id") {
  if (is.null(x) || !length(x)) return(character(0))
  if (inherits(x, "integer64")) x <- as.character(x)
  chr <- if (is.character(x)) x else if (is.numeric(x)) {
    frac <- !is.na(x) & x != floor(x)
    if (any(frac))
      stop(sprintf(paste0(
        "%d of %d `%s` values are not integral (e.g. %s). Casting one to an ",
        "integer ROUNDS, which is a different and possibly existing id."),
        sum(frac), length(x), what, paste(utils::head(x[frac], 3), collapse = ", ")),
        call. = FALSE)
    # sprintf("%.0f"), not format(): exact to 2^53 and never scientific notation,
    # which is the other way an id turns into something that is not an id
    ifelse(is.na(x), NA_character_, sprintf("%.0f", x))
  } else as.character(x)

  chr <- sub("^(-?[0-9]+)[.]0+$", "\\1", chr)          # ".0" means whole
  bad <- which(!is.na(chr) & !grepl("^-?[0-9]+$", chr))
  if (length(bad))
    stop(sprintf("%d of %d `%s` values are not integral (e.g. %s).",
                 length(bad), length(chr), what,
                 paste(utils::head(chr[bad], 3), collapse = ", ")), call. = FALSE)
  chr
}

# every column name this package treats as an id wherever it publishes one
.APP_ID_COLS <- c("taxon_id", "mdl_seq", "mdl_id", "worms_id", "sp_id", "zone_seq",
                  "metric_seq", "programarea_id")

# the normalised taxon SELECT every app_* builder reads. One SQL, four schemas:
# v1/v2 have no ER columns at all, v3-v7 keep er_score on the raw 1-100 scale.
.app_taxon_sql <- function(con) {
  k  <- .app_taxon_cols(con)
  f  <- k$f
  nn <- function(nm, expr, type) if (nm %in% f) expr else sprintf("CAST(NULL AS %s)", type)
  # v7's `is_ok` already bakes in the marine/category cull; v8+ splits it out
  valid <- if (!is.na(k$ok)) sprintf("COALESCE(t.%s, FALSE)", k$ok) else
    paste0("(", paste(c(
      if (!is.na(k$usa))  sprintf("COALESCE(t.%s, FALSE)", k$usa),
      if (!is.na(k$glob)) sprintf("COALESCE(t.%s, FALSE)", k$glob)),
      collapse = " OR "), ")")
  marine <- if (!is.na(k$marine)) sprintf(" AND t.%s", k$marine) else ""
  glue::glue("
    SELECT {.app_id_cast(con, 'taxon', k$key, paste0('t.', k$key))} AS key,
           t.scientific_name                 AS sci,
           {nn('common_name', 't.common_name', 'VARCHAR')}      AS common,
           t.sp_cat                          AS sp_cat,
           {.app_id_cast(con, 'taxon', 'taxon_id', 't.taxon_id')} AS taxon_id,
           t.taxon_authority                 AS taxon_authority,
           {nn('redlist_code', 't.redlist_code', 'VARCHAR')}    AS rl,
           {nn('iucn_code', 't.iucn_code', 'VARCHAR')}          AS rl2,
           {nn('extrisk_code', 't.extrisk_code', 'VARCHAR')}    AS er_code,
           {nn('esa_source', 't.esa_source', 'VARCHAR')}        AS esa_source,
           {nn('er_score', 't.er_score', 'DOUBLE')}             AS er_score,
           {nn('is_mmpa', 't.is_mmpa', 'BOOLEAN')}              AS is_mmpa,
           {nn('is_mbta', 't.is_mbta', 'BOOLEAN')}              AS is_mbta,
           {if (is.na(k$usa))  'CAST(TRUE AS BOOLEAN)'  else sprintf('COALESCE(t.%s, FALSE)', k$usa)}  AS valid_usa,
           {if (is.na(k$glob)) 'CAST(NULL AS BOOLEAN)'  else sprintf('COALESCE(t.%s, FALSE)', k$glob)} AS valid_global
      FROM taxon t
     WHERE t.{k$key} IS NOT NULL
       AND {valid}{marine}
       AND t.sp_cat NOT IN ('reptile', 'amphibian')
     ORDER BY t.sp_cat, t.scientific_name")
}

#' The normalised scored-taxon table (`app/taxon.parquet` and the shard source)
#'
#' One schema for every release: `key, sci, common, sp_cat, taxon_id,
#' taxon_authority, rl, er_code, esa_source, er_score, is_mmpa, is_mbta,
#' valid_usa, valid_global`. v1/v2 have no extinction-risk columns at all, so they
#' arrive as typed NULLs rather than making those two releases fail outright with
#' `Binder Error: ... does not have a column named "extrisk_code"`.
#'
#' The set is the one the species app offers: `key` not null, the release's own
#' validity (`is_ok` on v1-v7, `is_valid_usa OR is_valid_global` on v8+), `is_marine`
#' where the column exists, and `sp_cat NOT IN ('reptile','amphibian')`.
#'
#' Every `taxon_id` is asserted to be digits only. A DOUBLE column is cast through
#' an integer type **only where the value is whole**, because `CAST(12.7 AS HUGEINT)`
#' rounds to 13 — a different and possibly existing id that nothing downstream could
#' tell from a real one. A non-integral id is therefore a hard stop naming the row,
#' not something to publish.
#'
#' @param con a DBI connection to a release database
#' @param n_nonintegral optional environment; `$n` is set to the count (always 0 on
#'   success, since a non-zero count stops the build)
#' @return a data frame
#' @importFrom DBI dbGetQuery
#' @export
#' @concept app
app_taxon_table <- function(con, n_nonintegral = NULL) {
  d <- DBI::dbGetQuery(con, .app_taxon_sql(con))
  # rl: v1-v7 spell it redlist_code, v8+ iucn_code. Coalesce, then drop the twin.
  d$rl <- ifelse(is.na(d$rl), d$rl2, d$rl)
  d$rl2 <- NULL
  # counted and reported on every build, and a hard stop when it is not zero
  n <- .app_assert_integral_ids(d, "taxon_id")
  if (is.environment(n_nonintegral)) n_nonintegral$n <- n
  d
}

#' Build `app/taxa.json`, the picker index
#'
#' Column-oriented arrays rather than an array of objects: the picker reads every
#' taxon on load and the parallel form is a fraction of the gzipped size.
#'
#' @param con a DBI connection
#' @param ver version label
#' @return a validated `taxa.json` object
#' @export
#' @concept app
app_taxa <- function(con, ver) {
  d   <- app_taxon_table(con)
  cat <- sort(unique(d$sp_cat))
  out <- list(
    schema  = .APP_SCHEMA, ver = ver, n = nrow(d),
    cat     = as.list(cat),
    key     = as.list(d$key),
    sci     = as.list(d$sci),
    common  = as.list(d$common),
    cat_idx = as.list(as.integer(match(d$sp_cat, cat) - 1L)),
    # one small integer instead of two booleans: bit 1 = usa, bit 2 = global
    flags   = as.list(as.integer(
      ifelse(isTRUE_v(d$valid_usa), 1L, 0L) + ifelse(isTRUE_v(d$valid_global), 2L, 0L))))
  app_validate(out, "taxa", "taxa.json")
}

isTRUE_v <- function(x) !is.na(x) & x

# the normalised input edges: every non-merged model feeding a merged key.
# v1-v7 `taxon_model` INCLUDES an `ms_merge` self-edge and `n_ds` counts it; v8+
# does not. Dropping it here is what makes "3 inputs" mean the same thing on both.
.app_edges <- function(con) {
  tb <- DBI::dbListTables(con)
  if (!"taxon_model" %in% tb) return(data.frame(
    key = character(), mdl_key = character(), ds_key = character()))
  f <- DBI::dbListFields(con, "taxon_model")
  d <- if ("ms_merge_key" %in% f)
    DBI::dbGetQuery(con, "SELECT CAST(ms_merge_key AS VARCHAR) AS key,
                                 CAST(mdl_key AS VARCHAR) AS mdl_key, ds_key
                            FROM taxon_model")
  else {
    k <- .app_taxon_cols(con)
    DBI::dbGetQuery(con, glue::glue(
      "SELECT CAST(t.{k$key} AS VARCHAR) AS key,
              {.app_id_cast(con, 'taxon_model', 'mdl_seq', 'tm.mdl_seq')} AS mdl_key,
              tm.ds_key
         FROM taxon_model tm JOIN taxon t
           ON {.app_id_cast(con, 'taxon', 'taxon_id', 't.taxon_id')}
            = {.app_id_cast(con, 'taxon_model', 'taxon_id', 'tm.taxon_id')}"))
  }
  # `which()` and explicit !is.na(), NOT a bare logical subset. On v7
  # `CAST(t.mdl_seq AS VARCHAR)` is NA for every taxon with no merged model, so
  # `d$mdl_key != d$key` is NA there -- and `d[NA, ]` INJECTS an all-NA row rather
  # than dropping it. Measured on the real release: 2,354 of 14,501 edges came back
  # all-NA, and card()'s own `e[e$key == key, ]` then matched every one of them into
  # EVERY taxon (38 M phantom inputs; the shard step died there).
  keep <- which(!is.na(d$key) & !is.na(d$mdl_key) & !is.na(d$ds_key) &
                d$ds_key != "ms_merge" & d$mdl_key != d$key)
  d <- d[keep, , drop = FALSE]
  d[order(d$key, d$ds_key, d$mdl_key), , drop = FALSE]
}

# the asset registry, normalised to the v8 shape. v1-v7 publish `model_asset`
# (mdl_seq, cog_url) and nothing else, so the constants the species app already
# substitutes are substituted here instead: cog / 1-100 / spectral_r / no bbox.
.app_assets <- function(con) {
  tb <- DBI::dbListTables(con)
  none <- data.frame(key = character(), mdl_key = character(), ds_key = character(),
                     asset_type = character(), representation = character(),
                     asset_url = character(), rescale_min = numeric(),
                     rescale_max = numeric(), colormap = character(),
                     source_layer = character(), xmin = numeric(), xmax = numeric(),
                     ymin = numeric(), ymax = numeric(), stringsAsFactors = FALSE)
  if ("native_asset" %in% tb)
    return(DBI::dbGetQuery(con, "
      SELECT CAST(ms_merge_key AS VARCHAR) AS key, CAST(mdl_key AS VARCHAR) AS mdl_key,
             ds_key, asset_type, representation, asset_url,
             rescale_min, rescale_max, colormap, source_layer, xmin, xmax, ymin, ymax
        FROM native_asset"))
  if (!"model_asset" %in% tb) return(none)
  k <- .app_taxon_cols(con)
  DBI::dbGetQuery(con, glue::glue("
    SELECT {.app_id_cast(con, 'taxon', k$key, paste0('t.', k$key))} AS key,
           {.app_id_cast(con, 'model_asset', 'mdl_seq', 'ma.mdl_seq')} AS mdl_key,
           ma.ds_key, 'cog' AS asset_type, 'native' AS representation,
           ma.cog_url AS asset_url,
           1.0 AS rescale_min, 100.0 AS rescale_max, 'spectral_r' AS colormap,
           CAST(NULL AS VARCHAR) AS source_layer,
           CAST(NULL AS DOUBLE) AS xmin, CAST(NULL AS DOUBLE) AS xmax,
           CAST(NULL AS DOUBLE) AS ymin, CAST(NULL AS DOUBLE) AS ymax
      FROM model_asset ma JOIN taxon t
        ON {.app_id_cast(con, 'taxon', k$key, paste0('t.', k$key))}
         = {.app_id_cast(con, 'model_asset', 'mdl_seq', 'ma.mdl_seq')}"))
}

# the merged surface of a taxon: the `ms_merge` COG published for its own key.
# `bbox` is precomputed IN THE lon_span_agg FRAME (xmax may exceed 180) and is NULL
# when the distribution spans the globe, which retires mdl_bbox() -- the species
# app's most expensive query.
.app_merged <- function(a) {
  m <- a[which(!is.na(a$key) &
               ((!is.na(a$mdl_key) & a$key == a$mdl_key) |
                (!is.na(a$ds_key) & a$ds_key == "ms_merge"))), , drop = FALSE]
  m <- m[which(!is.na(m$asset_type) & m$asset_type == "cog"), , drop = FALSE]
  m[!duplicated(m$key), , drop = FALSE]
}

# The stored extent is used VERBATIM, never re-derived: `native_asset.xmin/xmax`
# were written by the release through `lon_span_agg()`, so `xmax` may already exceed
# 180 (a Bering Sea range is 165..205, which is what MapLibre's fitBounds wants) and
# re-running the rule on just those two numbers would break it -- with only the two
# extremes there is no way to tell 165..205 from a genuine -180..180.
#
# A whole-world box is a legitimate answer for an ASSET and a useless one for a
# CAMERA ("look at everything" shows nothing), so it becomes null and the app falls
# back exactly as it does for a missing one.
.app_bbox <- function(xmin, xmax, ymin, ymax) {
  if (any(is.na(c(xmin, xmax, ymin, ymax)))) return(NULL)
  bb <- as.numeric(c(xmin, ymin, xmax, ymax))
  if (bbox_spans_globe(bb)) NULL else bb
}

#' Build the `app/taxon/{xx}.json` shards
#'
#' 256 shards keyed by `sprintf('%02x', trailing_integer(key) %% 256)` (no trailing
#' digits, `00`). Each holds the full card per taxon: ids, listings, validity, the
#' merged surface with a **precomputed bbox**, and the normalised input edges with
#' their assets — no `ms_merge` self-edge, so an input count means the same thing on
#' every generation.
#'
#' @param con a DBI connection
#' @param ver version label
#' @return a named list `shard -> validated shard object`
#' @export
#' @concept app
app_taxon_shards <- function(con, ver) {
  d <- app_taxon_table(con)
  if (!nrow(d)) return(list())
  a <- .app_assets(con)
  e <- .app_edges(con)
  mg <- .app_merged(a)
  # v1's `dataset` has no `is_mask` at all -- selecting it unconditionally made the
  # whole shard stage fail with `Binder Error: Referenced column "is_mask" not
  # found`. D11: a release that cannot supply a capability simply does not
  # advertise it, so the flag comes back NULL rather than invented.
  ds <- if ("dataset" %in% DBI::dbListTables(con))
    DBI::dbGetQuery(con, sprintf("SELECT ds_key, %s AS is_mask FROM dataset",
      if ("is_mask" %in% DBI::dbListFields(con, "dataset")) "is_mask"
      else "CAST(NULL AS BOOLEAN)")) else
      data.frame(ds_key = character(), is_mask = logical())

  card <- function(i) {
    key <- d$key[i]
    # which(), never a bare `==`: a NA on either side of the comparison selects an
    # all-NA row instead of nothing, and one such row in `e` reappears under every
    # taxon in the bundle
    m   <- mg[which(!is.na(mg$key) & mg$key == key), , drop = FALSE]
    ei  <- e[which(!is.na(e$key) & e$key == key), , drop = FALSE]
    inputs <- lapply(seq_len(nrow(ei)), function(j) {
      ai <- a[which(!is.na(a$mdl_key) & !is.na(a$ds_key) &
                    a$mdl_key == ei$mdl_key[j] & a$ds_key == ei$ds_key[j]),
              , drop = FALSE]
      list(ds_key  = ei$ds_key[j],
           mdl_key = ei$mdl_key[j],
           is_mask = {v <- ds$is_mask[match(ei$ds_key[j], ds$ds_key)]
                      if (length(v) && !is.na(v)) as.logical(v) else NULL},
           assets  = lapply(seq_len(nrow(ai)), function(r) list(
             rep = ai$representation[r], type = ai$asset_type[r], url = ai$asset_url[r],
             rescale = if (is.na(ai$rescale_min[r])) NULL else
               c(as.numeric(ai$rescale_min[r]), as.numeric(ai$rescale_max[r])),
             colormap = ai$colormap[r], source_layer = ai$source_layer[r],
             bbox = .app_bbox(ai$xmin[r], ai$xmax[r], ai$ymin[r], ai$ymax[r]))))
    })
    list(key = key, sci = d$sci[i], common = d$common[i], sp_cat = d$sp_cat[i],
         taxon_id = d$taxon_id[i], taxon_authority = d$taxon_authority[i],
         rl = d$rl[i],
         esa = list(code = d$er_code[i], source = d$esa_source[i]),
         mmpa = d$is_mmpa[i], mbta = d$is_mbta[i], er_score = d$er_score[i],
         valid_usa = d$valid_usa[i], valid_global = d$valid_global[i],
         merged = if (!nrow(m)) NULL else list(
           type = "cog", url = m$asset_url[1],
           rescale = if (is.na(m$rescale_min[1])) NULL else
             c(as.numeric(m$rescale_min[1]), as.numeric(m$rescale_max[1])),
           colormap = m$colormap[1],
           bbox = .app_bbox(m$xmin[1], m$xmax[1], m$ymin[1], m$ymax[1])),
         inputs = inputs)
  }

  sh <- .shard_of(d$key)
  stats::setNames(lapply(sort(unique(sh)), function(s) {
    idx <- which(sh == s)
    out <- list(schema = .APP_SCHEMA, ver = ver, shard = s,
                taxa = .obj(stats::setNames(lapply(idx, card), d$key[idx])))
    app_validate(out, "taxon", sprintf("taxon/%s.json", s))
  }), sort(unique(sh)))
}

#' Build the `app/alias/{xx}.json` shards
#'
#' Every raw input key and every legacy `mdl_seq` maps to `[merged_key, ds_key]`, so
#' a published `?mdl_seq=` or `?mdl_key=am|…` link resolves without opening DuckDB.
#' Sharded like the taxon cards, on the ALIAS's own trailing integer.
#'
#' @param con a DBI connection
#' @param ver version label
#' @return a named list `shard -> validated shard object`
#' @export
#' @concept app
app_alias_shards <- function(con, ver) {
  keys <- app_taxon_table(con)$key
  e <- .app_edges(con)
  e <- e[e$key %in% keys, , drop = FALSE]   # %in% is NA-safe: NA %in% x is FALSE
  # the merged key is its own alias, so a ?mdl_key=ms_merge|… link resolves too
  al <- rbind(e[, c("mdl_key", "key", "ds_key")],
              data.frame(mdl_key = keys, key = keys, ds_key = "ms_merge",
                         stringsAsFactors = FALSE))
  al <- al[!duplicated(al$mdl_key), , drop = FALSE]
  if (!nrow(al)) return(list())
  sh <- .shard_of(al$mdl_key)
  stats::setNames(lapply(sort(unique(sh)), function(s) {
    idx <- which(sh == s)
    out <- list(schema = .APP_SCHEMA, ver = ver, shard = s,
                alias = .obj(stats::setNames(
                  lapply(idx, function(i) list(al$key[i], al$ds_key[i])),
                  al$mdl_key[idx])))
    app_validate(out, "alias", sprintf("alias/%s.json", s))
  }), sort(unique(sh)))
}

#' The normalised `app/zone_taxon.parquet`
#'
#' [.zone_taxon_normalize()] applied at publish: one schema for v1…v9, with
#' `er_score` always a 0-1 fraction (v3-v7 stored 1-100) and the model id always
#' `key`, whatever the release called it.
#'
#' @param con a DBI connection
#' @return a data frame
#' @importFrom DBI dbGetQuery dbListTables
#' @export
#' @concept app
app_zone_taxon <- function(con, chosen = NULL) {
  if (!"zone_taxon" %in% DBI::dbListTables(con))
    return(data.frame(zone_fld = character(), zone_value = character()))
  if (is.null(chosen)) chosen <- app_zone_tbl(con)
  # `zone_tbl` was DROPPED here, so v2's two subregion tables merged into 13,077
  # duplicated (zone_fld, zone_value, key) rows -- the same taxon in the same
  # subregion with two different area_km2, and the app listing it twice. The other
  # table's rows are dropped, never merged.
  keep_sql <- if ("zone_tbl" %in% DBI::dbListFields(con, "zone_taxon") && nrow(chosen))
    sprintf(" WHERE %s", paste(sprintf(
      "(zone_fld = %s AND zone_tbl = %s)", .sql_str(chosen$fld), .sql_str(chosen$tbl)),
      collapse = " OR ")) else ""
  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM zone_taxon", keep_sql))
  parts <- split(seq_len(nrow(d)), paste(d$zone_fld, d$zone_value, sep = "\u001f"))
  out <- do.call(rbind, lapply(parts, function(i) {
    n <- .zone_taxon_normalize(d[i, , drop = FALSE])
    cbind(zone_fld = d$zone_fld[i][1], zone_value = d$zone_value[i][1],
          as.data.frame(n), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  # The SQL cast reached app_taxon_table() and stopped there. `zone_taxon` is a
  # PRECOMPUTED table read with SELECT *, so v7's DOUBLE taxon_id came straight
  # through and this file published "22725044.0" beside taxon.parquet's "22725044".
  for (nm in intersect(names(out), .APP_ID_COLS))
    out[[nm]] <- app_id_chr(out[[nm]], nm)
  # the model id is an id too, whatever the generation calls it
  if ("mdl_key" %in% names(out) && !any(grepl("[|]", out$mdl_key, fixed = FALSE)))
    out$mdl_key <- app_id_chr(out$mdl_key, "mdl_key")
  out
}

#' The WoRMS hierarchy the Composition treemap joins, restricted to this release
#'
#' Written by the NOTEBOOK until now, outside `app_bundle_build()` and therefore
#' outside `boot$tables` — so the browser had no digest for it and **OPFS could
#' never invalidate it**: a cached taxonomy from a previous release stayed until
#' someone cleared site data by hand. An object the app reads is an object the
#' contract has to describe.
#'
#' The CSV is an INPUT, not something msens goes looking for: the hierarchy is a
#' dated WoRMS export shared across releases
#' (`apps/scores/data/taxonomic_hierarchy_worms_2025-10-30.csv`), and a builder that
#' guessed its path would silently publish whichever copy happened to be nearest.
#'
#' @param con a DBI connection to a release database
#' @param ver version label
#' @param dir_out the `app/` directory
#' @param taxonomy_csv path to the WoRMS hierarchy CSV, or `NULL` to write nothing
#'   (a release with no hierarchy on hand advertises none — nothing is invented)
#' @return a list with `path`, `rows` and `taxa`, or `NULL` when nothing was written
#' @importFrom utils read.csv
#' @export
#' @concept app
app_taxonomy <- function(con, ver, dir_out, taxonomy_csv = NULL) {
  if (is.null(taxonomy_csv) || !nzchar(taxonomy_csv) || !file.exists(taxonomy_csv))
    return(NULL)
  tx <- app_taxon_table(con)
  d  <- utils::read.csv(taxonomy_csv, stringsAsFactors = FALSE, colClasses = "character")
  # `species_id` is what the shared export actually calls it; the others are the
  # spellings WoRMS itself uses, kept so a re-export under any of them still works
  idc <- intersect(c("taxon_id", "species_id", "AphiaID", "aphia_id", "worms_id"),
                   names(d))
  if (!length(idc))
    stop(sprintf("`%s` has no recognizable taxon id column (looked for %s)",
                 basename(taxonomy_csv),
                 paste(c("taxon_id", "species_id", "AphiaID", "aphia_id", "worms_id"),
                       collapse = ", ")),
         call. = FALSE)
  names(d)[names(d) == idc[1]] <- "taxon_id"
  d$taxon_id <- app_id_chr(d$taxon_id, "taxon_id")   # same text as every other object
  d <- d[!is.na(d$taxon_id) & d$taxon_id %in% tx$taxon_id, , drop = FALSE]
  d <- d[!duplicated(d$taxon_id), , drop = FALSE]
  rownames(d) <- NULL
  p <- file.path(dir_out, "taxonomy.parquet")
  write_atlas_parquet(d, p)
  list(path = p, rows = nrow(d), taxa = nrow(tx), columns = names(d))
}

#' The `mdl_id` -> `mdl_key` mapping the `cell_model` join needs
#'
#' `cell_model` stores the compact integer `mdl_id` on v8+; every name in the app
#' comes from `mdl_key`. The mapping lived only in `{ver}/tables/model.parquet`,
#' OUTSIDE `app/` and outside `boot$tables`, so the browser fetched a 1.1 MB table
#' it could not cache-invalidate to resolve a click. This writes the three columns
#' the join actually needs.
#'
#' **v8+ only.** A release whose `model` table has no `mdl_id` joins `cell_model`
#' directly on `mdl_seq` and needs no mapping at all: it writes nothing and
#' advertises nothing (D11 — a release that cannot supply a capability simply does
#' not advertise it).
#'
#' @param con a DBI connection
#' @param ver version label
#' @param dir_out the `app/` directory
#' @return a list with `path` and `rows`, or `NULL` on a release without `mdl_id`
#' @importFrom DBI dbListTables dbListFields dbGetQuery
#' @export
#' @concept app
app_model <- function(con, ver, dir_out) {
  if (!"model" %in% DBI::dbListTables(con)) return(NULL)
  f <- DBI::dbListFields(con, "model")
  if (!all(c("mdl_id", "mdl_key") %in% f)) return(NULL)   # v1-v7: no mapping needed
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS mdl_id, CAST(mdl_key AS VARCHAR) AS mdl_key, %s
       FROM model WHERE mdl_id IS NOT NULL ORDER BY mdl_id",
    .app_id_cast(con, "model", "mdl_id", "mdl_id"),
    if ("ds_key" %in% f) "ds_key" else "CAST(NULL AS VARCHAR) AS ds_key"))
  d$mdl_id <- app_id_chr(d$mdl_id, "mdl_id")
  p <- file.path(dir_out, "model.parquet")
  write_atlas_parquet(d, p)
  list(path = p, rows = nrow(d), columns = names(d))
}

#' Assert every zone-keyed object holds each zone exactly once
#'
#' A hard stop, not a warning. Two `zone.tbl` for one `fld` (v2's `subregion_key`)
#' silently duplicated 13,077 `(zone_fld, zone_value, key)` rows in
#' `zone_taxon.parquet` and gave `boot$zones$subregion` six rows for four subregions.
#' Nothing errored; the app would simply have shown every taxon twice.
#'
#' @param zt the zone_taxon frame from [app_zone_taxon()]
#' @param boot the boot object
#' @return `TRUE`, invisibly; errors naming the first duplicated keys
#' @export
#' @concept app
app_zones_unique <- function(zt, boot) {
  if (!is.null(zt) && nrow(zt) && all(c("zone_fld", "zone_value") %in% names(zt))) {
    kc <- intersect(c("key", "mdl_key"), names(zt))[1]
    if (!is.na(kc)) {
      k <- paste(zt$zone_fld, zt$zone_value, zt[[kc]], sep = "\u001f")
      dup <- unique(k[duplicated(k)])
      if (length(dup))
        stop(sprintf(paste0(
          "zone_taxon has %d duplicated (zone_fld, zone_value, %s) group(s) over ",
          "%d rows.\n  e.g. %s\n  More than one `zone.tbl` for a field reached the ",
          "bundle: choose ONE (app_zone_tbl()) and drop the other's rows."),
          length(dup), kc, nrow(zt),
          paste(gsub("\u001f", " / ", utils::head(dup, 2)), collapse = " | ")),
          call. = FALSE)
    }
  }
  for (unit in names(boot$zones)) {
    ks <- vapply(boot$zones[[unit]], function(z) z$key, "")
    if (anyDuplicated(ks))
      stop(sprintf("boot$zones$%s lists %s more than once (%d entries, %d keys)",
                   unit, paste(unique(ks[duplicated(ks)]), collapse = ", "),
                   length(ks), length(unique(ks))), call. = FALSE)
  }
  invisible(TRUE)
}

#' Assert `boot$tables` names exactly the Parquet objects under `app/`
#'
#' No object without a digest, no digest without an object. A missing entry is an
#' object OPFS can never invalidate (`taxonomy.parquet` and the model mapping were
#' both in that state); a surplus entry is a fetch that 404s.
#'
#' @param dir_out the `app/` directory
#' @param boot the boot object
#' @return `TRUE`, invisibly; errors naming both differences
#' @export
#' @concept app
app_tables_match <- function(dir_out, boot) {
  files <- list.files(dir_out, "[.]parquet$", recursive = TRUE)
  # a partitioned object is named by its directory, the way boot$tables names it
  on_disk <- unique(ifelse(grepl("/", files, fixed = TRUE),
                           sub("/.*$", "", files), sub("[.]parquet$", "", files)))
  named <- names(boot$tables)
  miss <- setdiff(on_disk, named); extra <- setdiff(named, on_disk)
  if (length(miss) || length(extra))
    stop(sprintf(paste0(
      "boot$tables does not describe app/ exactly.\n",
      "  written but NOT in boot$tables (no digest, so OPFS can never ",
      "invalidate it): %s\n",
      "  in boot$tables but NOT written (the app would 404): %s"),
      if (length(miss)) paste(miss, collapse = ", ") else "(none)",
      if (length(extra)) paste(extra, collapse = ", ") else "(none)"), call. = FALSE)
  invisible(TRUE)
}

# ---- wide cell tiles ---------------------------------------------------------

#' Write the wide `app/cell/tile={t}/` Parquet tiles
#'
#' One DOUBLE column per scored `metric_key`, so nothing joins on `metric_seq` —
#' which `score_cell_metrics.qmd` drops and recreates every run.
#'
#' **Which rows.** Every `cell` row of any tile that holds a `cell_metric` or
#' `cell_model` row, so the browser's `JOIN cell c USING (cell_id)` keeps exactly the
#' rows R's does. `in_usa` / `in_pra` are NULL where a release has no such column.
#'
#' The tile key is [cell_model_tile_sql()] with the RELEASE'S OWN grid width. A wrong
#' width computes a different, perfectly valid tile id, `WHERE tile IN (…)` prunes
#' away the only partition holding the cells, and the query returns nothing at all —
#' see [cell_model_tile_check()] and [app_cell_tile_check()].
#'
#' @param con a DBI connection
#' @param dir_out directory to write `tile={t}/data_0.parquet` into
#' @param ncol grid width; defaults to the release's own [cell_grid_ncol()]
#' @return a list: `dir`, `tiles`, `rows`, `metric_keys`, `ncol`
#' @importFrom DBI dbGetQuery dbListTables dbListFields
#' @importFrom glue glue
#' @export
#' @concept app
app_cell_tiles <- function(con, dir_out, ncol = cell_grid_ncol(con)) {
  keys <- .app_cell_metric_keys(con)
  stopifnot("release has no cell_metric rows" = length(keys) > 0)
  vc   <- sdm_val_col(con, "cell_metric")
  cf   <- DBI::dbListFields(con, "cell")
  tile <- function(col) cell_model_tile_sql(col, ncol = ncol)

  opt <- function(nm) if (nm %in% cf) sprintf("c.%s", nm) else
    sprintf("CAST(NULL AS BOOLEAN) AS %s", nm)
  cols <- paste(vapply(keys, function(k) glue::glue(
    "max(CASE WHEN m.metric_key = {DBI::dbQuoteString(con, k)} THEN cm.{vc} END) AS {DBI::dbQuoteIdentifier(con, k)}"),
    ""), collapse = ",\n           ")

  src <- c(glue::glue("SELECT DISTINCT {tile('cell_id')} AS tile FROM cell_metric"),
           if ("cell_model" %in% DBI::dbListTables(con))
             glue::glue("SELECT DISTINCT {tile('cell_id')} AS tile FROM cell_model"))
  sql <- glue::glue("
    WITH t AS ({paste(src, collapse = ' UNION ')}),
    u AS (
      SELECT c.cell_id, c.area_km2, {opt('in_usa')}, {opt('in_pra')},
             {tile('c.cell_id')} AS tile
        FROM cell c
       WHERE {tile('c.cell_id')} IN (SELECT tile FROM t)
    )
    SELECT u.cell_id, u.area_km2, u.in_usa, u.in_pra, u.tile,
           {cols}
      FROM u
      LEFT JOIN cell_metric cm ON cm.cell_id = u.cell_id
      LEFT JOIN metric m       ON m.metric_seq = cm.metric_seq
     GROUP BY u.cell_id, u.area_km2, u.in_usa, u.in_pra, u.tile")

  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  # ONE file per tile. DuckDB's PARTITION_BY writes one part PER THREAD, so a busy
  # tile came out as data_0..data_5.parquet -- and since anonymous LIST is denied on
  # the bucket, the browser can only ever construct `data_0.parquet` and would have
  # read a sixth of that tile, silently. Measured on v9: 422 tile directories,
  # 1,687 files, 290 of the directories multi-part.
  #
  # Written per tile instead of coalesced afterwards: a coalesce would mean writing
  # the whole surface twice, and `preserve_insertion_order = false` (which
  # copy_atlas_parquet sets for byte-sized row groups) makes the parts arbitrary, so
  # merging them is a second full scan with no ordering to preserve. One COPY per
  # tile is a single pass and the file it names is the file that exists.
  # Materialise the pivot ONCE. Re-running the query per tile would rescan `cell`
  # and `cell_metric` 422 times (17 M and 10 M rows on v9); with a temp table each
  # tile is a cheap filtered write and the whole surface is read once.
  tmp <- "app_cell_wide_tmp"
  DBI::dbExecute(con, glue::glue("CREATE OR REPLACE TEMP TABLE {tmp} AS {sql}"))
  on.exit(try(DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {tmp}")),
              silent = TRUE), add = TRUE)
  tiles <- DBI::dbGetQuery(con, glue::glue(
    "SELECT DISTINCT tile FROM {tmp} ORDER BY tile"))$tile
  stopifnot("no tiles to write" = length(tiles) > 0)
  for (t in tiles) {
    td <- file.path(dir_out, sprintf("tile=%s", format(t, scientific = FALSE)))
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    copy_atlas_parquet(
      con, glue::glue("SELECT * EXCLUDE (tile) FROM {tmp} WHERE tile = {t}"),
      file.path(td, "data_0.parquet"))
  }
  app_one_file_per_partition(dir_out)          # and prove it, every time

  n <- DBI::dbGetQuery(con, glue::glue(
    "SELECT count(*) n FROM read_parquet('{dir_out}/*/data_0.parquet',
       hive_partitioning = true)"))$n
  list(dir = dir_out, tiles = length(tiles), rows = as.integer(n),
       files = length(list.files(dir_out, "[.]parquet$", recursive = TRUE)),
       metric_keys = keys, ncol = as.integer(ncol))
}

#' Assert a partitioned bundle directory holds exactly one file per partition
#'
#' **Anonymous LIST is denied on the bucket**, so a static client cannot discover
#' parts: it constructs `tile={t}/data_0.parquet` and reads whatever is there. A
#' second part is therefore not a performance detail, it is missing data that nobody
#' is told about — and a gate that globs `**/*.parquet` reads all six parts and sees
#' nothing wrong. Every partitioned thing the bundle writes goes through this.
#'
#' @param dir a partitioned directory (`tile={t}/` beneath it)
#' @param name what to call it in the error
#' @return the number of partitions, invisibly; errors on any partition with a file
#'   count other than one
#' @export
#' @concept app
app_one_file_per_partition <- function(dir, name = basename(dir)) {
  parts <- list.dirs(dir, recursive = FALSE)
  if (!length(parts))
    stop(sprintf("`%s` has no partitions at all", name), call. = FALSE)
  n <- vapply(parts, function(p) length(list.files(p, "[.]parquet$")), 0L)
  if (any(n != 1L)) {
    bad <- parts[n != 1L]
    stop(sprintf(paste0(
      "`%s`: %d of %d partitions do not hold exactly one parquet file.\n",
      "  e.g. %s holds %d (%s)\n",
      "  Anonymous LIST is denied, so a static client reads only data_0.parquet ",
      "and would silently miss the rest."),
      name, length(bad), length(parts), basename(bad[1]), n[n != 1L][1],
      paste(list.files(bad[1], "[.]parquet$"), collapse = ", ")), call. = FALSE)
  }
  invisible(length(parts))
}

# read a partitioned bundle directory THE WAY THE BROWSER DOES: by constructing
# `tile={t}/data_0.parquet`, never by globbing. A gate that globs `**/*.parquet`
# happily reads parts the client can never ask for.
.app_tiles_as_client <- function(dir)
  sprintf("read_parquet('%s/*/data_0.parquet', hive_partitioning = true)", dir)

#' Assert every tile's cell ids satisfy the tile key at a given grid width
#'
#' The check that **cannot pass on a mismatch**: it recomputes the tile id from each
#' `cell_id` and compares it with the partition the row is stored in. Counting rows
#' or listing files cannot tell a right width from a wrong one, because a wrong tile
#' id is still a valid tile id.
#'
#' @param dir the `app/cell` directory written by [app_cell_tiles()]
#' @param ncol grid width to check against (7200 `global05`, 3103 `usa05`)
#' @param con optional DuckDB connection to run the scan on
#' @return `TRUE`, invisibly; errors on mismatch
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
#' @export
#' @concept app
app_cell_tile_check <- function(dir, ncol, con = NULL) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  }
  expr <- cell_model_tile_sql("cell_id", ncol = ncol)
  d <- DBI::dbGetQuery(con, glue::glue("
    SELECT count(*) n_bad,
           min(cell_id) AS cell_id, min(tile) AS tile, min({expr}) AS want
      FROM {.app_tiles_as_client(dir)}
     WHERE tile <> {expr}"))
  if (d$n_bad[1] > 0)
    stop(sprintf(paste0(
      "cell tile ids do not satisfy the tile key at ncol = %d.\n",
      "  e.g. cell_id %s is stored in tile %s but computes to tile %s\n",
      "  %d rows disagree. Tile pruning would silently return NO rows."),
      as.integer(ncol), format(d$cell_id[1]), format(d$tile[1]), format(d$want[1]),
      as.integer(d$n_bad[1])), call. = FALSE)
  invisible(TRUE)
}

#' Per-metric multiset digest of the wide tiles, and of `cell_metric`
#'
#' The wide tiles are a pivot of `cell_metric`; a pivot that dropped or duplicated a
#' row is invisible in a row count. Both sides are reduced with [hash_query()]'s
#' order-independent fingerprint over `(cell_id, val)` for cells that HAVE the
#' metric, so the comparison cannot be fooled by row order or row-group layout.
#'
#' @param con a DBI connection to the release
#' @param dir the `app/cell` directory
#' @return a data frame `metric_key`, `tiles`, `cell_metric`, `ok`
#' @importFrom DBI dbGetQuery
#' @export
#' @concept app
app_cell_tile_digests <- function(con, dir) {
  keys <- .app_cell_metric_keys(con)
  vc   <- sdm_val_col(con, "cell_metric")
  d <- do.call(rbind, lapply(keys, function(k) {
    kq <- DBI::dbQuoteIdentifier(con, k)
    # as the CLIENT reads it: a second part in any tile makes this digest differ
    a <- hash_query(con, glue::glue(
      "SELECT cell_id, {kq} AS val FROM {.app_tiles_as_client(dir)}
         WHERE {kq} IS NOT NULL"),
      cols = c("cell_id", "val"))
    # ...and the SOURCE side restricted to the SAME universe the tiles publish:
    # every `cell` row of a tile that holds a metric row. v3 has 703 `cell_metric`
    # cells with no `cell` row at all, so an unrestricted source digest compared a
    # set the contract deliberately excludes (the browser's `JOIN cell USING
    # (cell_id)` drops them too) and reported 0 of 17 metrics matching on v3-v6.
    b <- hash_query(con, glue::glue(
      "SELECT cm.cell_id, cm.{vc} AS val FROM cell_metric cm
         JOIN metric m USING (metric_seq)
         JOIN cell   c ON c.cell_id = cm.cell_id
        WHERE m.metric_key = {DBI::dbQuoteString(con, k)}
          AND cm.{vc} IS NOT NULL"),
      cols = c("cell_id", "val"))
    data.frame(metric_key = k, tiles = a, cell_metric = b, ok = identical(a, b),
               stringsAsFactors = FALSE)
  }))
  rownames(d) <- NULL
  d
}

# ---- capabilities, probed rather than copied ---------------------------------

#' What a STATIC client can actually fetch, by anonymous HTTPS HEAD
#'
#' **Never copied from `manifest$capabilities`.** v7 and v7b both advertise
#' `cell_species_list = TRUE` because the server-side apps can read a `cell_model`
#' that lives only on the server; S3 holds `tables/` and `manifest.json` for those
#' releases and nothing else. A manifest describes the SERVER's tables; this
#' describes the bucket.
#'
#' A capability is TRUE only on a 200 (or 206). Anything else — 403, 404, a redirect,
#' a timeout — is FALSE, and the probed URL and status are returned beside it so a
#' FALSE can be explained rather than guessed at.
#'
#' @param ver version label
#' @param base atlas base URL from [atlas_base_url()]
#' @param sample named list of `capability -> key relative to {base}/{ver}/`,
#'   overriding the defaults
#' @param timeout seconds per probe
#' @return a list with `capabilities` (named logicals) and `probed`
#' @importFrom httr2 request req_method req_timeout req_error req_perform resp_status
#' @export
#' @concept app
app_capabilities <- function(ver, base = atlas_base_url(), sample = list(), timeout = 20) {
  def <- list(
    cell       = "app/cell/tile=0/data_0.parquet",
    cell_model = "serve/cell_model/tile=0/data_0.parquet",
    taxonomy   = "app/taxonomy.parquet",
    alias      = "app/alias/00.json",
    pmtiles_s3 = "native/pmtiles/index.json")
  def <- utils::modifyList(def, sample)
  probed <- list(); caps <- list()
  for (nm in names(def)) {
    url <- sprintf("%s/%s/%s", base, ver, def[[nm]])
    st <- tryCatch(
      httr2::resp_status(httr2::req_perform(
        httr2::req_error(httr2::req_timeout(
          httr2::req_method(httr2::request(url), "HEAD"), timeout),
          is_error = function(resp) FALSE))),
      error = function(e) NA_integer_)
    probed[[nm]] <- list(url = url, status = if (is.na(st)) NULL else as.integer(st))
    caps[[nm]]   <- !is.na(st) && st %in% c(200L, 206L)
  }
  list(capabilities = caps, probed = probed)
}

#' The `app` block for `manifest.json`
#'
#' @param ver version label
#' @param base atlas base URL
#' @param capabilities result of [app_capabilities()], or `NULL` to probe now
#' @param built_at ISO timestamp
#' @return a validated `app` block
#' @export
#' @concept app
app_manifest_block <- function(ver, base = atlas_base_url(), capabilities = NULL,
                               built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  p <- capabilities %||% app_capabilities(ver, base)
  out <- list(schema = .APP_SCHEMA,
              base = sprintf("%s/%s/app", base, ver),
              boot = sprintf("%s/%s/app/boot.json", base, ver),
              built_at = built_at,
              capabilities = p$capabilities, probed = p$probed)
  app_validate(out, "manifest", "manifest$app")
}

# ---- the table manifest and the bundle ---------------------------------------

#' Describe a written `app/` Parquet file for `boot.json$tables`
#'
#' @param path the written file or directory
#' @param name table name
#' @param ver version label
#' @param base atlas base URL
#' @param columns declared column contract, `list(list(name=, type=))`
#' @param rows row count
#' @param partitioned_by partition column, or `NULL`
#' @return a validated table descriptor
#' @export
#' @concept app
app_table_descriptor <- function(path, name, ver, base = atlas_base_url(),
                                 columns, rows, partitioned_by = NULL) {
  files <- if (dir.exists(path))
    list.files(path, "\\.parquet$", recursive = TRUE, full.names = TRUE) else path
  out <- list(
    schema = .APP_SCHEMA, ver = ver, name = name, path = basename(path),
    bytes  = as.integer(sum(file.size(files))),
    rows   = as.integer(rows),
    # parquet_digest() returns a ONE-ROW FRAME; `$digest` is the combined
    # schema+data fingerprint, which is the OPFS invalidation key the app caches on
    digest = as.character(parquet_digest(
      if (dir.exists(path)) file.path(path, "*", "data_0.parquet") else path)$digest[1]),
    partitioned_by = partitioned_by,
    files   = as.integer(length(files)),
    columns = columns)
  app_validate(out, "table", sprintf("%s descriptor", name))
}

#' @rdname app_table_descriptor
#' @param descriptors a list of descriptors from [app_table_descriptor()]
#' @return for `app_table_manifest()`, the `tables` object of `boot.json`
#' @export
#' @concept app
app_table_manifest <- function(descriptors, ver, base = atlas_base_url())
  stats::setNames(lapply(descriptors, function(d) list(
    href   = sprintf("%s/%s/app/%s", base, ver, d$path),
    bytes  = d$bytes,
    digest = d$digest)), vapply(descriptors, function(d) d$name, ""))

#' Build the whole `{ver}/app/` bundle into a directory
#'
#' Composes every builder, validates each output against its schema, and writes the
#' bytes. Nothing is uploaded here: publishing is the notebook's job and happens
#' only under `APP_BUNDLE_S3=1`.
#'
#' @param con a DBI connection to the release database
#' @param ver version label
#' @param dir_out output directory (created)
#' @param manifest a manifest from [manifest_build()]; built from `con` when `NULL`
#' @param base atlas base URL
#' @param geom_keys optional geometry keys per zone type, see [app_units()]
#' @param cell_tiles write the wide cell tiles (default `TRUE`)
#' @param taxonomy_csv path to the WoRMS hierarchy CSV, passed to [app_taxonomy()].
#'   `NULL` writes no `taxonomy.parquet` and advertises none.
#' @param zone_sets the zone-set registry (`data/zone_sets.csv`), passed to
#'   [app_zone_tbl()] so a release with two tables for one field publishes the one
#'   the geometry actually corresponds to
#' @param strict stop at the first failing stage (`TRUE`, the default), so a
#'   half-written bundle is never published by accident. `FALSE` runs every stage,
#'   records what failed in `$failed`, and leaves the stages that worked on disk —
#'   the notebook's diagnostic pass. Either way a failure NAMES its stage and the
#'   outputs already written, so nobody needs to build a parallel composition to
#'   get at the parts.
#' @param built_at ISO timestamp
#' @return a list describing everything written, invisibly
#' @importFrom jsonlite toJSON
#' @export
#' @concept app
app_bundle_build <- function(con, ver, dir_out, manifest = NULL,
                             base = atlas_base_url(), geom_keys = list(),
                             cell_tiles = TRUE, strict = TRUE,
                             taxonomy_csv = NULL, zone_sets = NULL,
                             built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  if (is.null(manifest)) manifest <- manifest_build(con, ver, base = base)
  # resolved ONCE, so zone_taxon, boot$zones and boot$units cannot disagree
  chosen <- app_zone_tbl(con, manifest, geom_keys, zone_sets)
  wrote <- list(); failed <- list()
  put <- function(rel, obj) {
    p <- file.path(dir_out, rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines(app_json(obj), p)
    wrote[[rel]] <<- file.size(p)
    p
  }
  # PER-STAGE ISOLATION. This is the ONE composition a notebook calls, so a stage
  # that fails must name itself and leave the other stages' outputs on disk --
  # otherwise the next person writes a parallel composition to get at the parts,
  # and then there are two contracts. With `strict = TRUE` (the default) the first
  # failure still stops the build, because a half-written bundle must not be
  # published by accident; `strict = FALSE` is for the notebook's diagnostic pass.
  note <- function(rel, n) wrote[[rel]] <<- n
  # `expr` is a PROMISE evaluated in this function's own frame, so a `<<-` inside it
  # skips this frame and lands in the namespace. Stage bodies assign with `<-`
  # (which lands here, because that is where the promise evaluates) and record bytes
  # through note(), a real closure whose enclosure is this frame.
  stage <- function(name, expr) {
    out <- tryCatch(expr, error = function(e) {
      failed[[name]] <<- conditionMessage(e)
      if (isTRUE(strict))
        stop(sprintf("app_bundle_build(): stage '%s' failed: %s\n  completed: %s",
                     name, conditionMessage(e),
                     if (length(wrote)) paste(names(wrote), collapse = ", ") else "(none)"),
             call. = FALSE)
      NULL
    })
    out
  }

  # parquet tables ---------------------------------------------------------
  descr <- list()
  tx <- stage("taxon.parquet", {
    d <- app_taxon_table(con)
    p <- file.path(dir_out, "taxon.parquet")
    write_atlas_parquet(d, p)
    descr$taxon <- app_table_descriptor(
      p, "taxon", ver, base, rows = nrow(d),
      columns = lapply(names(d), function(n) list(name = n, type = class(d[[n]])[1])))
    note("taxon.parquet", file.size(p))
    d
  })

  zt <- stage("zone_taxon.parquet", {
    d <- app_zone_taxon(con, chosen)
    if (nrow(d)) {
      p <- file.path(dir_out, "zone_taxon.parquet")
      write_atlas_parquet(d, p)
      descr$zone_taxon <- app_table_descriptor(
        p, "zone_taxon", ver, base, rows = nrow(d),
        columns = lapply(names(d), function(n) list(name = n, type = class(d[[n]])[1])))
      note("zone_taxon.parquet", file.size(p))
    }
    d
  })

  stage("taxonomy.parquet", {
    t <- app_taxonomy(con, ver, dir_out, taxonomy_csv)
    if (!is.null(t)) {
      descr$taxonomy <- app_table_descriptor(
        t$path, "taxonomy", ver, base, rows = t$rows,
        columns = lapply(t$columns, function(n) list(name = n, type = "character")))
      note("taxonomy.parquet", file.size(t$path))
    }
    t
  })
  stage("model.parquet", {
    t <- app_model(con, ver, dir_out)
    if (!is.null(t)) {
      descr$model <- app_table_descriptor(
        t$path, "model", ver, base, rows = t$rows,
        columns = lapply(t$columns, function(n) list(name = n, type = "character")))
      note("model.parquet", file.size(t$path))
    }
    t
  })

  tiles <- NULL
  if (isTRUE(cell_tiles)) tiles <- stage("cell/", {
    t <- app_cell_tiles(con, file.path(dir_out, "cell"))
    descr$cell <- app_table_descriptor(
      file.path(dir_out, "cell"), "cell", ver, base, rows = t$rows,
      partitioned_by = "tile",
      columns = lapply(c("cell_id", "area_km2", "in_usa", "in_pra", "tile",
                         t$metric_keys),
                       function(n) list(name = n, type = "double")))
    note("cell/", t$files)
    t
  })

  # json objects -----------------------------------------------------------
  boot <- stage("boot.json", {
    b <- app_boot(con, ver, manifest,
                  tables = app_table_manifest(unname(descr), ver, base),
                  geom_keys = geom_keys, zone_sets = zone_sets, chosen = chosen,
                  built_at = built_at)
    put("boot.json", b)
    # a hard stop: one zone, one row, in every object keyed by zone
    app_zones_unique(zt, b)
    # no object without a digest, no digest without an object
    app_tables_match(dir_out, b)
    b
  })
  stage("taxa.json", put("taxa.json", app_taxa(con, ver)))
  sh <- stage("taxon/", {
    x <- app_taxon_shards(con, ver)
    for (k in names(x)) put(file.path("taxon", paste0(k, ".json")), x[[k]])
    x
  })
  al <- stage("alias/", {
    x <- app_alias_shards(con, ver)
    for (k in names(x)) put(file.path("alias", paste0(k, ".json")), x[[k]])
    x
  })

  invisible(list(dir = dir_out, ver = ver, bytes = wrote, tables = descr,
                 tiles = tiles, boot = boot, failed = failed,
                 n_taxa = if (is.null(tx)) NA_integer_ else nrow(tx),
                 n_shards = length(sh), n_alias = length(al)))
}

utils::globalVariables(c("tile", "metric_key"))
