# msens 0.12.1

* **`cells_in_polygon()` dispatches with `methods::is()`, not `inherits()`.** A `duckdb_connection`
  is an **S4** object, whose superclasses `inherits()` does not reliably see — so a connection could
  fall through to the raster branch and fail with
  `unable to find an inherited method for 'rasterize' for signature 'y = "duckdb_connection"'`.
  `methods` is now declared in `Imports:` (it was in `NAMESPACE` only).

# msens 0.12.0

*Drawn areas resolve against the grid of the version being queried*

* **`cells_in_polygon(poly, src)` now takes a DB connection** (still accepts a `SpatRaster` for
  back-compat) and picks the grid from the database it is querying, so the two cannot disagree.
  v8 (`cell` has `lon`/`lat`) takes a **SQL bbox select** on `cell` plus exact `sf` coverage on just
  those candidates; v7 falls back to [cell_id_raster()], the 0-360 regional raster that IS v7's grid.
* **Fixes silently EMPTY drawn-area reports on v8.** `cell_id_raster()` is the **v7** raster —
  regional, 0-360 longitudes, holding v7 `cell_id`s — but the `/report` and `/species.csv` endpoints
  passed it for every version. Against v8 those ids are valid but denote different places: a polygon
  off Santa Barbara resolved to ids 2,924,984-3,015,011, and id 2,928,088 is **lon 64.375 / lat
  69.675 — the Arctic**. Nothing errored; `species_for_cells()` simply returned **0 species**.
  Regression-tested, since a wrong `cell_id` is still a *valid* `cell_id` and so cannot fail loudly.
* **~31x faster on the same query.** For a 2 x 1.5-degree area off California, measured on the
  server: the raster path took **35.7 s and returned 0 species**; the SQL path takes **1.15 s and
  returns 2,572 species** (bbox select 0.02 s, `species_for_cells` 0.75 s, `scores_for_cells`
  0.37 s). It no longer reads the whole cell-id raster, and it joins `cell_model`, which is already
  partitioned by spatial tile.
* `pct_covered` semantics are preserved exactly — computed planar in degrees, as terra's
  `cover = TRUE` does — because [scores_for_cells()] and [species_for_cells()] weight by it.
* Antimeridian-safe: the candidate longitude span splits into two ranges across 180 degrees.

# msens 0.11.0

*Read mapgl's drawn polygons whichever shape they arrive in*

* **`drawn_features_sf()`** — turns the raw `input$<map_id>_drawn_features` value from a mapgl
  draw control into `sf` (EPSG:4326), or `NULL` when nothing is drawn. Accepts **both** payload
  shapes: the character GeoJSON older mapgl stringified, and the object current mapgl
  (`_mapglSyncDrawnFeatures`, >= 0.5.0) sends, which Shiny delivers as a nested list.
* Fixes the scores Report tab ("Add drawn polygon" answering *"Draw a polygon on the map first."*
  for every polygon drawn) in **both** the v7 and v8 apps, which gated on `is.character()` and so
  discarded every payload from the newer mapgl. Regression-tested against both shapes.
* **`Remotes:` now points at the antimeridian-fixed mapgl fork**
  (`bbest/mapgl@484e869f` = walkerke/mapgl `main` + walkerke/mapgl#211), not `walkerke/mapgl`.
  Twelve Shiny apps `librarian::shelf(MarineSensitivity/msens)` at startup, so every msens install
  from GitHub resolved this field and silently overwrote the fork that the rstudio image pins —
  which is how the h3-db globe went back to showing a gap at the antimeridian. Revert to
  `walkerke/mapgl` once #211 merges.

# msens 0.10.0

*Log the real client IP, and the commit that produced the row*

* **`ga_js()` / `ga_head()` gain `ip`.** Stamps a client IP on every logged row, taken from the
  **page** request. Behind shiny-server that is the only place a real one exists: shiny-server does
  not proxy the websocket upgrade — it opens a fresh localhost connection to the R worker — so
  `session$request` has no `X-Forwarded-For` and `REMOTE_ADDR` is always `127.0.0.1`, however
  correctly Caddy is configured. Make the app's `ui` a `function(req)` and pass
  `ip = ms_client_ip(req)`.
* **`ms_client_ip()`** — reads `X-Forwarded-For` (first entry of the chain) then `REMOTE_ADDR`,
  from either a `session` or the `req` of a `ui` function. Never errors.
* **`ms_track_session()`** — hands the browser the Shiny session token, which JavaScript cannot
  read. Its IP is a **fallback, never an override**: otherwise the websocket's `127.0.0.1` would
  clobber the good page-supplied address moments later. Regression-tested, since it would silently
  undo the whole fix.
* **`ms_track_query()`** — wrap a query to record its row count, duration and any error. The result
  (including a lazy `dbplyr` table) passes through untouched and an error is re-raised after logging.
* **The Sheet gains six columns**, matching CalCOFI's shape:
  `timestamp, ip, session, event, params, n_rows, ms, status, error, app_version, app, client_id,
  session_id, page, referrer, user_agent`. `n_rows`/`ms` stay **numeric** so they remain chartable —
  Apps Script `setValues()` would write a JS string as text. `n_rows`, `ms`, `status` and `error` are
  reserved parameter names, hoisted out of `params` into their own columns.
* **`app_version` is now the deployed commit** in the apps, so a row ties back to the exact code.

# msens 0.9.2

* **Never inspect `model_cell` when `cell_model` is available.** `.sdm_cols()` read `model_cell`'s
  schema *before* choosing a source; on the server that alone makes DuckDB LIST the S3 prefix and
  fail (`IO Error: SSL peer certificate … HTTP GET …/serve/model_cell/`), so the clicked cell stayed
  broken even once `cell_model` existed. The source is now chosen first and only that table's schema
  is read. Guarded by a test that drops `model_cell` outright, so any reference at all fails.

# msens 0.9.1

* **`sdm_db_path()` falls back to `serve.duckdb`** when the full `sdm.duckdb` is absent. The server
  deliberately carries only the KB-sized view DB for v8, so `sdm_db_con()` — and therefore the
  `/report` endpoint — failed outright there. The Shiny apps already did this inline; centralising
  it stops the two from drifting, and it is what lets a v8 report reach the new `cell_model`
  surface at all.

# msens 0.9.0

* **`cell_model` — the cell-oriented twin of `model_cell`** (`cell_model.R`). `serve/model_cell` is
  partitioned by `mdl_id`, which makes a titiler tile one point read but makes any per-**cell** or
  per-**polygon** question (the scores app's clicked cell, the Report tab's arbitrary area) a scan
  of all ~580M rows — over S3 that fails outright. `cell_model` holds the same rows partitioned by a
  **2.5° spatial tile**: 422 partitions, avg 1.4M rows, 1.4 GB total, and a single cell resolves in
  **0.066 s**. Row counts match the source exactly (580,568,326 / 634,208 cells / 17,763 models).

  `cell_model_tile_sql()` and `cell_model_tiles()` are the SQL and R halves of one formula — the
  writer partitions with the first, readers prune with the second, and a test asserts they agree on
  identical inputs, because a mismatch makes queries silently return **nothing**.
  `species_for_cells()` uses `cell_model` automatically when present.

  Deliberately kept as LOCAL Parquet on the server, not S3: these queries touch many partitions,
  which is exactly the access pattern that fails over HTTPS.

# msens 0.8.0

* **`widget_png()`** — render a heavyweight htmlwidget to a static PNG (`htmlwidgets::saveWidget()`
  + headless Chrome via `webshot2`), for use as
  `knitr::include_graphics(widget_png(m, "figures/map.png"))` in a notebook chunk.

  An interactive widget serialises its ENTIRE data payload into the rendered HTML, which had
  produced published pages of 43–53 MB dominated by a single `<script>` block (one turtle map
  embedded 26 MB of GeoJSON; a study-area map, 40 MB) — over GitHub's 50 MB warning, and downloaded
  in full by every visitor just to look at a picture. `show_study-area` went from **43 MB to
  2.7 MB** with a 684 KB image. Side benefit: the MapTiler style URL carries an API key that the
  embedded JSON published verbatim; a screenshot leaves it out of the HTML entirely.

  Use it where interactivity isn't the point — keep printing the widget where panning/zooming is.

* **The test suite is green.** The two long-standing failures were both in `hex.R`/`interp.R`, marked
  DORMANT since v8 rolled back from H3 to the 0.05° cell grid; their SQL no longer binds under
  DuckDB 1.5 (tighter GROUP BY/CTE scoping). They are now `skip()`ped with that reason rather than
  deleted, since those modules are explicitly retained for future H3 use: **0 failures, 223 passing,
  2 skips.**

# msens 0.7.1

* **`build_zone_taxon()` + `species_for_zone()` now prefers the precomputed table.** Computing a
  zone's species list live works locally but **cannot work on the server**, which holds only the
  KB-sized `serve.duckdb` whose `model_cell` is an S3 view *partitioned by `mdl_id`* for per-model
  point reads (titiler tiles). Aggregating there means listing and scanning ~580M rows over HTTPS
  and fails with `IO Error: … HTTP GET …/serve/model_cell/`. This is exactly why v7 shipped a
  `zone_taxon` table, and why v8 dropping it broke the app's species table.

  `build_zone_taxon()` precomputes every zone where `model_cell` is local (115,700 rows across 36
  zones, a few MB) and the pipeline releases it; `species_for_zone()` reads it when present and
  falls back to the live aggregation otherwise, so local development still works without it.
  Subregion USA went from **5.7 s to 0.021 s**.

# msens 0.7.0

* **The species table works on v8 again** — `species_for_cells()` is now schema-adaptive and a new
  `species_for_zone()` computes the table for a subregion / Program Area / ecoregion.

  The v8 rewrite renamed `is_ok`→`is_valid_usa`, `mdl_seq`→`ms_merge_key`/`mdl_key` and
  `value`→`val`, **and dropped the precomputed `zone_taxon` table** that v7 read. The query had also
  been duplicated inline in the scores app against the old names. Together that left the v8 "Table
  of Species" tab empty (`Can't select columns that don't exist`) and its CSV download unable to
  produce a file. The app now *calls* these functions, so app and tests cannot drift.

* **Scoring eligibility is enforced, not just cell presence.** v7 baked the marine/category cull
  into `is_ok`; v8's `is_valid_usa` only means "has ≥1 merged cell in US waters". Filtering on
  validity alone listed ineligible taxa — the first row of the real v8 study-area table was a
  **cane toad**. Now also requires `is_marine` (where present) and excludes `reptile`/`amphibian`,
  giving 9,632 species for subregion USA across the seven scoring categories.

* No precomputed table is needed: the largest zone (~349k cells, ~10k species) computes in ~5 s.
  Guarded by `tests/testthat/test-species-table.R` — synthetic v7 **and** v8 fixtures with identical
  numbers, so both schemas must agree.

# msens 0.6.1

* **`ms_apps_script()` gains a `doGet()` health check.** Opening the deployed `/exec` URL in a
  browser previously returned *"Script function not found: doGet"*, which reads as a broken or
  unauthorized deployment — it is not, since the client only ever POSTs, but it sends you hunting
  through Apps Script deployment settings. A GET now answers `{ok:true, rows:<n>}`, so the endpoint
  (and that writes are landing) can be verified at a glance.

# msens 0.6.0

* **Usage analytics for the browser-facing products** (`analytics.R`), so the toolkit can
  report which layers, tabs, species and downloads are actually used. Two channels, one
  code path — and **neither performs network I/O on the R side**, so instrumenting a hot
  control adds no latency to the reactive that follows:

  * `ga_js()` / `ga_head()` — the `<head>` snippet: GA4 (gtag) for aggregate, bounded
    behaviour, plus a client-side queue that beacons detail to a Google Sheet. Batched
    (10 events / 15 s / page-hidden) via `navigator.sendBeacon`, which keeps the Apps
    Script execution quota flat regardless of interaction rate. The beacon body is
    `text/plain` on purpose: that keeps it a CORS *simple* request, and an Apps Script
    `/exec` endpoint does not answer the `OPTIONS` preflight an `application/json` body
    would trigger.
  * `ms_track(session, event, ...)` — send an event from the Shiny **server** for facts
    only R knows (the scientific name behind a picker value, a report's parameters, a row
    count, an error). It pushes one message over the session's existing websocket; it
    never opens an HTTP connection, and it swallows errors so instrumentation can never
    take down an app. Verified to work from inside `downloadHandler(content=)`.
  * `ms_event()` — the payload constructor: normalises event names to GA4's rules
    (lowercase `[a-z0-9_]`, leading letter, ≤40 chars) and drops absent parameters.
  * `ms_log_header()` / `ms_apps_script()` — the Sheet's column header and the `doPost()`
    Apps Script that appends a **batch** in one `setValues()`. Column order comes from
    `ms_log_header()` so the Sheet, the script and the client payload cannot drift.

  One GA4 measurement ID is shared by every product (gtag scopes the `_ga` cookie to the
  registrable domain, so a single stream already spans `marinesensitivity.org` and
  `app.marinesensitivity.org`); products are separated by `content_group`. Guarded by
  `tests/testthat/test-analytics.R`.

* **`_pkgdown.yml`**: the reference site now carries the same GA4 tag (`content_group: msens`).

# msens 0.5.1

* **Getting-started article: score-surface section.** The `msens` article now maps the
  **sensitivity score surfaces** — the equal-weight composite plus each per-category component —
  served as raster tiles by titiler-v8 via a live `cell_id → value` SQL over `cell_metric` (no
  per-metric COG; the scored cells *are* the raster). It reports titiler `cell_stats()` per surface
  (proof the tiles carry real values) and a leaflet layers-control to toggle between them.

* **`pra_score_delta()` is now schema-adaptive** across the v8 `value`→`val` reserved-word rename.
  The Program-Area score/key column is `value` in a v7 `sdm.duckdb` but `val` in v8, so the previously
  hard-coded `z.value` errored on any v8 database ("Table z does not have a column named value") — it
  had only ever worked for v7. The column is now resolved per connection, so v7↔v8 and v8↔v8
  comparisons both work. Regression-tested in `test-validate.R` against synthetic `val`/`value` DBs.
  Powers the new parameterized `workflows/validate_versions.qmd` report.

# msens 0.5.0

The **v8 "Marine Atlas"** modeling + serving pass, part 2 — merge rules extracted to the package
(single source of truth) and the read/score API made to compose off one connection.

* **Merge rules as a single source of truth.** New `merge.R`: `merge_sql()` and `turtle_sql()`
  return the exact SQL for the two-surface merge (global viz `am ∪ range`; US-scoped, v7-faithful
  scoring surface with the AquaMaps no-EEZ constraint) and the multiplicative turtle merge. The
  `workflows` notebooks now *call* these and `test-merge.R` *asserts* them (one synthetic fixture per
  taxon category), so the notebook and the tests can never drift.
* **`attach_atlas()` now creates the table views** (via the new exported `atlas_views()`), mirroring
  the serving `serve.duckdb`, so the calc/score helpers that reference bare table names
  (`scores_for_pra()`, `species_for_cells()`, `scores_for_cells()`) compose directly — e.g.
  `scores_for_pra(attach_atlas(anon = TRUE), pra_key)` just works. `test-atlas.R` guards it.
* **Getting-started article + STAC alignment.** The `msens` article now walks attach → browse →
  search the STAC catalog → retrieve + map a whole-range COG → score a Program Area (flower plot) →
  species. The v8 STAC catalog reads cleanly in R (`rstac::read_stac()` for the static catalog — not
  `stac()`, which is for STAC *API* servers) and Python (`pystac`); `rstac` added to Suggests.
* `cells_from_ranges()` uses terra touches-rasterize as the fast default (keeping the `exact_extract`
  coverage option) — big speed-up on large ranges.
* `cell_tile_url()` / `cell_stats()` default `base` → the v8 `titiler-v8` factory (accepts
  `?mdl_key=`). The legacy v7 `titilecache` Varnish takes `?sql=` and 422s on `mdl_key`, which had
  left default-base callers (e.g. the article's map) with blank tiles.

# msens 0.4.0

The **v8 "Marine Atlas"** foundation: read the S3 Parquet release, ingest source models onto the
global 0.05° grid, publish native + gridded representations, and emit a STAC catalog.

* **Read the release.** `attach_atlas()` — canonical DuckDB reader for the marine-atlas Parquet on
  S3 (path-style, credential-chain), with `atlas_path()` / `atlas_tbl()` accessors.
* **Standardized Parquet + content-addressed change detection.** `write_atlas_parquet()` /
  `copy_atlas_parquet()` (Parquet V2, zstd, ~80 MB byte-sized row groups) behind a `require_duckdb()`
  version floor; `hash_parquet()` / `hash_query()` order-independent fingerprints +
  `write_manifest()` / `force_target()` for deterministic, timestamp-free manifests; `report_table()`
  / `report_parquet_summary()` for the notebook `## Outputs` sections.
* **Stable model id.** `mdl_key_raw()` / `mdl_key_merged()` build the `{ds_key}|{sp_id}` key that
  replaces the volatile `mdl_seq`; renamed the model-cell field `value` → `val` (SQL reserved word).
* **Ingest helpers.** `cells_from_ranges()` / `cells_from_raster()` / `cells_pct_marine()` rasterize
  a source model onto the global grid capturing the **whole** range (no land mask; `pct_marine`
  derived), via `exactextractr`. `clean_sci_name()` for taxonomic matching.
* **Native + gridded publishing.** `publish_cog()` (COG with overviews), `publish_pmtiles()` /
  `publish_pmtiles_models()` (per-model PMTiles), and `cog_tile_url()` for titiler `/cog` tiles.
* **STAC v8** (`stac.R`): `stac_build()` and the collection/item generators emit both `native` and
  `model` representations per dataset on `model_cell` Items, keyed on the stable `mdl_key`.
* **Pipeline generator.** `build_targets_list()` parses the `msens:` front-matter of the workflow
  `*.qmd` into a `targets` list. `pra_score_delta()` is the version-equivalence gate.
* `hex.R` + `interp.R` are marked **DORMANT** — v8 rolled back from an H3 grid to the 0.05° cell grid.

# msens 0.3.4

* `cell_tile_url()` gains a `color` argument for single-color mask tiles:
  when set to a hex string (e.g. `"#222222"`), the URL uses the msens
  TiTiler factory's `color=` query param, which renders every valid pixel
  in that flat RGBA color and ignores `colormap` / `rescale`. Used by the
  mapgl app's "Cells outside Program Areas" overlay — replaces the old
  `msens::add_cells(r_outside_pra, colors = c("#222222","#222222"), ...)`
  pattern that shipped a terra raster as a base64 image source.

# msens 0.3.3

* Added `add_cell_tiles()`, `cell_tile_url()`, `cell_stats()` for TiTiler 
  endpoint to support mapgl app.

# msens 0.3.2

* Added `cells_in_pra()` and `scores_for_pra()` — fast Program Area
  lookups that read from `zone` / `zone_cell` / `zone_metric` instead
  of rasterizing the polygon and aggregating across cells. Same
  output shape as `cells_in_polygon()` / `scores_for_cells()` so
  they're drop-in replacements when the area is a Program Area.

# msens 0.3.1

* Pin `mapgl (>= 0.4.5.9000)` and add `Remotes: walkerke/mapgl` so
  `install_github()` pulls the dev build that exports `add_pmtiles_source()`
  (needed by `add_pmfill()` / `add_pmline()`). Fixes a silent install
  failure on fresh environments where the pinned CRAN snapshot still
  served `mapgl 0.1.3`.

# msens 0.3.0

* Added score-calculation helpers migrated out of the mapgl app so they can be
  reused by the report and API: `cells_in_polygon()`, `scores_for_cells()`,
  `species_for_cells()`, `mean_score()` and `cell_id_raster()` (new `R/calc.R`).
* Added visualization helpers for multi-format (html / pdf / docx) reports:
  `ggplot_flower()` and `ggmap_areas()`. `plot_flower()` and `tbl_species()`
  gained an `interactive=` argument so they can emit static output for
  non-html Quarto formats.
* `sdm_db_path()` / `sdm_db_con()` now default to the v6 species-distribution
  database.

# msens 0.2.1

* Added mapping functions for use in docs and map apps, including raster cells 
  (`add_cells()`), and using PMtiles as vectors sources to add fills (`add_pmfill()`, 
  eg for Program Area scores), outlines (`add_pmline()`, eg for Ecoregion outlines),
  and labels (`add_pmlabel()`, eg for Ecoregion names and Program Area acronymns).

# msens 0.2.0

* Swapped polygons:
  - OLD: hierarchy `ply_shlfs` > `ply_rgns` (and `*_s05`
  simplifications) that were clipped to US EEZ.
  - NEW: hierarchy `ply_boemrgns` > `ply_ecorgns` | `ply_planareas` > 
  `ply_ecoareas`, which are the intersection of `ply_ecorgns` and `ply_planareas`.
  Created `*_s05` simplifications of each. The new polygons conform to BOEM's
  original nomenclature for "OCS Regions", Planning Areas" and "Ecoregions". 
  These polygons are not clipped to the US EEZ.

# msens 0.1.2

* Added `get_species_by_feature()` to read from API endpoint.

# msens 0.1.1

* Added simple `ms_basemap()` to support map app.

# msens 0.1

* Added [`data`](../reference/index.html#data) basic Outer Continental Shelf (OCS) regions `ply_shlfs` and BOEM Planning Regions `ply_rgns` with simplified to 5% variants (`ply_shlfs_s05`, `ply_rgns_s05`).
