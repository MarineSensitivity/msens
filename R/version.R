# version.R — the atlas VERSION registry: latest.txt, versions.json, manifest.json
#
# Historically the MST version was a CODE fact: `ver <- "v8"` hardcoded in each
# Shiny app, which forced a fork of the apps repo per release (apps_v1..apps_v6,
# branch v7, apps_v8) and stranded every app improvement in the newest fork.
# These helpers make it a DATA fact instead, so one app renders any version:
#
#   latest.txt          the newest RELEASED version (never a pre-release)
#   versions.json       {"versions":[{ver,status,released,...}]}
#   {ver}/manifest.json everything needed to render that version (see atlas_manifest)
#
# Design rules, learned from the CalCOFI release registry:
#  - resolution NEVER silently falls back to a hardcoded version. A wrong-but-
#    plausible default is worse than a clear error: it renders the wrong science
#    under the right label.
#  - a pre-release is reachable ONLY by naming it explicitly, so promoting is a
#    deliberate act (writing latest.txt) gated on validation, not a side effect
#    of publishing data.
#  - all three files are tiny and immutable-ish, so they are memoised per session;
#    pass `refresh = TRUE` to re-fetch.

# session cache for the registry files (all small; keyed by URL)
.atlas_reg <- new.env(parent = emptyenv())

.atlas_fetch <- function(url, refresh = FALSE) {
  if (!refresh && !is.null(.atlas_reg[[url]])) return(.atlas_reg[[url]])
  txt <- tryCatch(
    paste(readLines(url, warn = FALSE), collapse = "\n"),
    warning = function(w) stop(sprintf("could not read '%s': %s", url, conditionMessage(w)), call. = FALSE),
    error   = function(e) stop(sprintf("could not read '%s': %s", url, conditionMessage(e)), call. = FALSE))
  .atlas_reg[[url]] <- txt
  txt
}

#' Base HTTPS URL of the marine-atlas release tree
#'
#' Path-style, because the bucket name contains dots and so breaks
#' virtual-hosted-style TLS (see [attach_atlas()] for the same constraint).
#'
#' @param bucket S3 URI of the atlas root
#' @param region S3 region
#' @return an `https://` base URL with no trailing slash
#' @export
#' @concept version
atlas_base_url <- function(bucket = "s3://oceanmetrics.io-public/marine-atlas",
                           region = "us-east-1")
  sub("^s3://", sprintf("https://s3.%s.amazonaws.com/", region), bucket)

#' The promoted (latest released) atlas version
#'
#' Reads `latest.txt`. Errors if unreachable rather than guessing — see the design
#' note at the top of this file.
#'
#' @param base atlas base URL from [atlas_base_url()]
#' @param refresh re-fetch instead of using the session cache
#' @return a version string, e.g. `"v8"`
#' @export
#' @concept version
atlas_latest <- function(base = atlas_base_url(), refresh = FALSE) {
  v <- trimws(.atlas_fetch(sprintf("%s/latest.txt", base), refresh))
  v <- trimws(strsplit(v, "\n", fixed = TRUE)[[1]][1])
  if (!nzchar(v) || !.is_ver(v))
    stop(sprintf("latest.txt does not name a valid version (got '%s')", v), call. = FALSE)
  v
}

# a version label: v1, v4b, v10 ...  (NOT a free-form string: it becomes a URL path)
.is_ver <- function(x) grepl("^v[0-9]+[a-z]?$", x)

#' The atlas version registry
#'
#' Parses `versions.json`. `status` is one of `released`, `prerelease`, `retired`;
#' anything else is rejected, because an unrecognized status silently becoming
#' "showable" is how a half-built release reaches users.
#'
#' @param base atlas base URL from [atlas_base_url()]
#' @param refresh re-fetch instead of using the session cache
#' @return a data frame with at least `ver` and `status`, newest first
#' @importFrom jsonlite fromJSON
#' @export
#' @concept version
atlas_versions <- function(base = atlas_base_url(), refresh = FALSE) {
  j <- jsonlite::fromJSON(.atlas_fetch(sprintf("%s/versions.json", base), refresh),
                          simplifyDataFrame = TRUE)
  d <- if (is.data.frame(j)) j else j$versions
  if (!is.data.frame(d) || !all(c("ver", "status") %in% names(d)))
    stop("versions.json must hold an array of objects with at least `ver` and `status`", call. = FALSE)
  ok <- c("released", "prerelease", "retired")
  if (any(bad <- !d$status %in% ok))
    stop(sprintf("versions.json has unknown status %s (expected one of %s)",
                 paste(sQuote(unique(d$status[bad])), collapse = ", "),
                 paste(ok, collapse = ", ")), call. = FALSE)
  d
}

#' Resolve a requested version to a real, published one
#'
#' The single entry point every caller (app URL `?ver=`, report endpoint, notebook)
#' should use. `NULL`/`""`/`"latest"` resolve to [atlas_latest()]; anything else must
#' name a version present in [atlas_versions()]. A **pre-release is never returned
#' for "latest"** — it is reachable only by asking for it by name.
#'
#' @param ver requested version, or `NULL`/`"latest"`
#' @param base atlas base URL from [atlas_base_url()]
#' @param allow statuses that may be resolved when named explicitly
#' @param refresh re-fetch the registry instead of using the session cache
#' @return a validated version string
#' @export
#' @concept version
atlas_resolve_ver <- function(ver = NULL, base = atlas_base_url(),
                              allow = c("released", "prerelease", "retired"),
                              refresh = FALSE) {
  if (is.null(ver) || !nzchar(ver <- trimws(as.character(ver)[1])) || identical(ver, "latest"))
    return(atlas_latest(base, refresh))
  if (!.is_ver(ver))
    stop(sprintf("'%s' is not a version label (expected e.g. 'v8', 'v4b', or 'latest')", ver),
         call. = FALSE)
  d <- atlas_versions(base, refresh)
  i <- match(ver, d$ver)
  if (is.na(i))
    stop(sprintf("unknown version '%s'; published: %s", ver,
                 paste(d$ver, collapse = ", ")), call. = FALSE)
  if (!d$status[i] %in% allow)
    stop(sprintf("version '%s' has status '%s', which is not allowed here (allowed: %s)",
                 ver, d$status[i], paste(allow, collapse = ", ")), call. = FALSE)
  ver
}

#' A version's manifest — the contract between a release and the apps
#'
#' `{ver}/manifest.json` declares everything needed to render that version, so the
#' app holds no per-version knowledge and publishing v9 needs no app edit. Required
#' keys are asserted here rather than defaulted: a **missing `capabilities` block
#' must fail loudly**, since defaulting an unknown capability to "supported" makes
#' the app promise a panel the data cannot fill.
#'
#' @param ver version (resolved through [atlas_resolve_ver()] if `NULL`/`"latest"`)
#' @param base atlas base URL from [atlas_base_url()]
#' @param refresh re-fetch instead of using the session cache
#' @return a list, with the resolved version in `$ver`
#' @importFrom jsonlite fromJSON
#' @export
#' @concept version
atlas_manifest <- function(ver = NULL, base = atlas_base_url(), refresh = FALSE) {
  ver <- atlas_resolve_ver(ver, base, refresh = refresh)
  m <- jsonlite::fromJSON(.atlas_fetch(sprintf("%s/%s/manifest.json", base, ver), refresh),
                          simplifyVector = TRUE, simplifyDataFrame = TRUE)
  validate_manifest(m, ver = ver)
}

#' Validate a version manifest
#'
#' Split from [atlas_manifest()] so the publishing notebook asserts the same
#' contract it writes, and the unit tests can exercise it without network.
#'
#' @param m parsed manifest (list)
#' @param ver expected version, or `NULL` to skip the cross-check
#' @return `m`, invisibly-but-returned (with `$ver` guaranteed)
#' @export
#' @concept version
validate_manifest <- function(m, ver = NULL) {
  if (!is.list(m)) stop("manifest must be an object", call. = FALSE)
  req <- c("ver", "status", "grid_id", "id_field", "capabilities", "tables")
  if (length(miss <- setdiff(req, names(m))))
    stop(sprintf("manifest is missing required key(s): %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  if (!is.list(m$capabilities) || !length(m$capabilities))
    stop("manifest `capabilities` must be a non-empty object; ",
         "an absent capability block would let the app assume every feature is supported",
         call. = FALSE)
  if (!m$id_field %in% c("mdl_seq", "mdl_key"))
    stop(sprintf("manifest `id_field` must be 'mdl_seq' or 'mdl_key' (got '%s')", m$id_field),
         call. = FALSE)
  if (!is.null(ver) && !identical(m$ver, ver))
    stop(sprintf("manifest declares ver '%s' but was fetched as '%s'", m$ver, ver), call. = FALSE)
  m
}

#' Build a version manifest from that release's database
#'
#' Introspects the release rather than being told about it, so a manifest cannot
#' drift from the data it describes. Everything that differs between releases —
#' which model id is public, which tables exist, which metrics are cell-level,
#' which spatial units were scored — is *read*, not assumed.
#'
#' Capabilities are derived from presence, and default to **FALSE**: a release
#' that lacks `cell_model` must not advertise a per-cell species list.
#'
#' @param con open DuckDB connection to that release's database
#' @param ver version label
#' @param status `released`, `prerelease` or `retired`
#' @param grid_id defaults to [grid_for_ver()]
#' @param base atlas base URL from [atlas_base_url()]
#' @param metrics optional data frame of published metric COGs to merge in
#'   (`metric_key`, `subregion_key`, `cog`, `rescale_min`, `rescale_max`); until
#'   score COGs are published the `score_cogs` capability stays FALSE
#' @param capabilities named logicals overriding the derived ones. Needed
#'   whenever a surface is NOT a table in `con`: v8's `cell_model`, for instance,
#'   is published as a Parquet directory beside the database, so deriving from
#'   `con` alone would advertise `cell_species_list = FALSE` and switch off a
#'   panel that works. Overrides must be justified by checking the RELEASE, not
#'   by optimism.
#' @param extra named list merged into the manifest (e.g. `zone_sets`)
#' @return a validated manifest list
#' @importFrom DBI dbListTables dbListFields dbGetQuery
#' @importFrom utils modifyList
#' @export
#' @concept version
manifest_build <- function(con, ver, status = "released",
                           grid_id = grid_for_ver(ver), base = atlas_base_url(),
                           metrics = NULL, capabilities = list(), extra = list()) {
  tbls <- DBI::dbListTables(con)
  has  <- function(x) x %in% tbls
  http <- sprintf("%s/%s", base, ver)

  # the PUBLIC model id: v8+ mint a stable mdl_key; v1-v7 only ever had the
  # autoincrement mdl_seq, which renumbers per rebuild (hence the crosswalk)
  id_field <- if (has("model") && "mdl_key" %in% DBI::dbListFields(con, "model"))
    "mdl_key" else "mdl_seq"

  want <- c("cell", "taxon", "model", "model_asset", "metric", "cell_metric",
            "zone", "zone_cell", "zone_metric", "zone_taxon", "dataset",
            "taxon_model", "listing", "native_asset")
  tables <- stats::setNames(
    as.list(sprintf("%s/tables/%s.parquet", http, intersect(want, tbls))),
    intersect(want, tbls))

  # cell-level metrics only: zone-only metrics (ecoregion min/max) never rasterize
  met <- if (has("metric") && has("cell_metric"))
    DBI::dbGetQuery(con,
      "SELECT m.metric_key, m.description
         FROM metric m
        WHERE m.metric_seq IN (SELECT DISTINCT metric_seq FROM cell_metric)
        ORDER BY m.metric_seq") else data.frame()
  if (!is.null(metrics) && nrow(met))
    met <- merge(met, metrics, by = "metric_key", all.x = TRUE)

  zones <- if (has("zone"))
    DBI::dbGetQuery(con, "SELECT tbl, fld, count(*) n FROM zone GROUP BY 1,2 ORDER BY 2") else data.frame()

  caps <- list(
    cell_species_list     = has("cell_model"),
    native_representation = has("native_asset"),
    programareas          = nrow(zones) > 0 && "programarea_key" %in% zones$fld,
    planareas             = nrow(zones) > 0 && "planarea_key"    %in% zones$fld,
    zone_taxon            = has("zone_taxon"),
    score_cogs            = !is.null(metrics) && nrow(met) > 0 && !all(is.na(met$cog)))

  if (length(capabilities)) {
    if (is.null(names(capabilities)) || !all(nzchar(names(capabilities))))
      stop("`capabilities` must be a NAMED list", call. = FALSE)
    if (!all(vapply(capabilities, function(x) is.logical(x) && length(x) == 1, logical(1))))
      stop("`capabilities` values must be single logicals", call. = FALSE)
    caps <- utils::modifyList(caps, capabilities)
  }

  m <- c(list(ver = ver, status = status, grid_id = grid_id, id_field = id_field,
              capabilities = caps, tables = tables,
              metrics = met, zones = zones), extra)
  validate_manifest(m, ver = ver)
}

#' Does this version support a named capability?
#'
#' Unknown capabilities are `FALSE`, never `TRUE` — the app must not offer a panel
#' a release never declared.
#'
#' @param m manifest from [atlas_manifest()]
#' @param what capability name, e.g. `"cell_species_list"`
#' @return logical scalar
#' @export
#' @concept version
manifest_can <- function(m, what) isTRUE(m$capabilities[[what]])
