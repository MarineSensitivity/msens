# msens 0.24.0

* **`registry_merge()`** — merges a freshly built asset registry with the one already published,
  so `publish_native.qmd` can no longer delete an asset class it simply did not rebuild.

  `native_asset` records what is *published*, but it was assembled from what a given run
  *built* and written with `overwrite = TRUE`. Skipping a build chunk (`NATIVE_SKIP_PMTILES`,
  or leaving an opt-in class such as `PUBLISH_MERGED_COG` off) therefore rewrote the table from
  that run's products alone. In v8 this silently removed all 2,234 vector-range PMTiles rows
  while the tiles themselves stayed on S3 and the file host: the species app offered AquaMaps
  as the only input for every taxon — 2,240 input edges across 2,121 scored taxa lost their
  layer — and the content-hash checkpoint recorded the loss as a clean run (`n_pmtiles_native: 0`).

  A **class** is one `(ds_key, asset_type, representation)` triple. `registry_merge()` carries
  forward, verbatim, every prior class the new registry has no rows for, and **errors** when a
  class it does cover came back smaller — a partial build must not publish as a complete one.
  Deliberate removals are explicit: `allow_shrink = TRUE`, wired to `NATIVE_REGISTRY_REBUILD=1`.

# msens 0.23.0

* **`zone_score_delta()`** — compares per-zone scores between two releases across **every** metric,
  for the versioned documentation's "what changed" summary. `pra_score_delta()` answers "did this
  bump move the Program-Area composite?" for one metric; this answers "what did this release
  actually move", which the species count cannot: v4, v4b and v5 all report an unchanged 9,795
  valid species while changing the values substantially.

  Comparability is asserted rather than assumed. Two releases can label a spatial unit identically
  and mean different things by it — every release resolves `subregion_key` to the same zone-set
  vintage while the member keys are `AK, AKL48, L48, USA` in v1, `AK, FULL, GA, PA, USA` in v7 and
  `AK, AT, GA, PA, USA` in v8, so a vintage check alone would report a delta for a redefinition.
  It compares only zones and metrics present on BOTH sides and returns what it had to drop.
  Metrics move too: `extrisk_reptile` exists only in v1, `extrisk_all` only in v1–v2, and v8
  replaces `extrisk_other` with `extrisk_primary_producer`.

# msens 0.22.0

* **`assign_mdl_id()`** — assigns the dense integer `mdl_id` that partitions the serving surface
  (`serve/model_cell/mdl_id={id}/`). Previously this was an inline `dense_rank(mdl_key)` in
  `build_registry.qmd`, which makes the id a function of the model *set*: **adding any model
  renumbers every model sorted after it**. Ingesting `gm` + `nc` into v8's `dist/` (registered but
  deliberately not merged) moved **45,499 of 80,261** ids, and nothing would have failed — the
  registry and the published partitions would simply disagree, and titiler would have served the
  wrong species' distribution for every model past `ch_nmfs`. `assign_mdl_id()` preserves the id of
  any `mdl_key` already in the published registry and appends new keys above the maximum, so a
  published partition can never be renumbered. With no published registry it falls back to
  `dense_rank`, and a test asserts that fallback reproduces v8's published ids exactly.

* **`dataset_is_scored()`** — a release's `dataset` table registers what was *ingested*, which is
  not what the scores were computed from. v8 ingests the NOAA SEFSC (`gm`) and NCCOS (`nc`) density
  models into `dist/`, but their `#/km²` units are not yet mapped onto the `[0,100]` suitability
  scale, so they are excluded from the merge and contribute to no score — reading the registry
  straight gives 11 input datasets where 8 produced the numbers. The test is introspected from
  `taxon_model` (did this dataset feed a merged taxon?), never declared, and `build_registry.qmd`
  now stamps it as `dataset.is_scored`.

* **`sdm_cols()` is now exported** (was internal `.sdm_cols()`). It resolves the v1–v7 vs v8 column
  names — `is_ok`/`is_valid_usa`, `mdl_seq`/`ms_merge_key`, `value`/`val` — by introspecting the
  connection. The versioned documentation asks the same question of the same published tables, and
  a second copy of the rule in the docs is exactly how a v3 page ends up printing a v8 column name.

# msens 0.21.1

* **Fixes `mdl_key_raw()` losing its export in 0.21.0.** `normalize_ds_key()` was inserted between
  `mdl_key_raw()`'s roxygen block and the function itself, which silently reassigned that block —
  `mdl_key_raw` dropped out of `NAMESPACE` entirely. Nothing failed at build or test time; it
  surfaced as `'mdl_key_raw' is not an exported object from 'namespace:msens'` partway through a
  backfill render.

* **New `test-exports.R` guards the whole class.** It asserts the public surface other repos call,
  and checks that every `@export` is followed by a definition rather than another roxygen block.

# msens 0.21.0

* **`normalize_ds_key()`** — v1–v7 spell AquaMaps `am_0.05`; the `mdl_key` grammar uses `am`.
  The rule had been an inline `sub()` in `backfill_versions.qmd`, so it was untested and invisible
  to anyone reading the package — and minting a key from the raw legacy string yields
  `am_0.05|Fis-29291`, which matches nothing in v8, silently failing to join the two generations.
  Now exported and asserted, including a round-trip of `mdl_key_raw()` against the **real**
  published v1/v3/v7 `model_asset.parquet` rows rather than synthetic fixtures.

# msens 0.20.0

* **Resolve a clicked point through titiler, not a local raster.** `cog_point_value()` calls
  `GET /cog/point/{lon},{lat}` so the number in a popup comes from the same surface the user is
  looking at, and `grid_cellid_url()` names the grid's cell-id COG so the cell id resolves the same
  way. Reading the raster in the app produced three separate bugs: the band is named `r_cellid` on
  `usa05` and `depth_mean` on `global05` (selecting `$cell_id` returned NULL), longitude was
  shifted to 0-360 while both images are stored -180..180 (every Americas click sampled outside the
  image), and a `SpatRaster` cached across Shiny sessions is a stale external pointer that
  **segfaults** the process -- which is why clicking the map disconnected the app with no R error
  and nothing in the log.

* **`cell_from_lonlat()`** — the exact inverse of `cell_lonlat()`, pure arithmetic on the grid
  definition, so a click still resolves if the tile server is briefly unavailable. Round-trips with
  `cell_lonlat()` on both grids; a point outside the grid is `NA` rather than a wrapped-around cell.

# msens 0.19.0

* **`manifest_build()` emits one zone row per spatial unit.** v2 and v3 each carry two subregion
  tables under a single `fld` (a legacy of synthesising subregions per release); both resolve to
  the same canonical vintage, so the manifest listed `subregion` twice and any app keying its
  picker on the manifest offered the same choice twice. Rows now collapse on `zone_set_key`,
  keeping the largest `n` so the count describes the unit rather than whichever table sorted first.

# msens 0.18.0

* **Registry fetches are cached on DISK, not just in-process.** `latest.txt`, `versions.json` and
  `manifest.json` were memoised per R process — but shiny-server starts a fresh process per
  session, so every visitor re-fetched all three over HTTPS. Measured at **0.58 s**, landing
  squarely inside time-to-first-byte for each session (TTFB was ~0.87–1.14 s total). Off disk the
  same reads are microseconds. TTL'd at 300 s rather than permanent, because promoting a release
  rewrites `latest.txt` and republishing rewrites a manifest; `MSENS_ATLAS_TTL=0` or
  `refresh = TRUE` bypasses it, `MSENS_ATLAS_CACHE` relocates it. Written via write-then-rename so
  a concurrent reader never sees a half-written file.

# msens 0.17.0

* **`cell_lonlat(wrap = )`** — `wrap = FALSE` keeps a 0-360 grid in its own frame. Wrapping is
  right for plotting a point but wrong for an **extent**: `usa05` runs 141.10 E across the
  antimeridian, so a wrapped Alaska spans -180..180 and its bounding box comes out as the whole
  globe instead of the Bering Sea. This is what lets subregion bboxes be derived from `zone_cell`
  instead of a per-version metrics raster, which existed only for v3-v8 and so made older
  releases unopenable.

# msens 0.16.0

* **`zone_set_resolve()`** — a release whose own `zone` table predates the `zone_set_key` column
  (v1–v7; only v8 stamps it) resolves its spatial units against the zone-set registry instead.
  Without it `manifest_build()` emitted no `zone_set_key`, hence no `pmtiles`, for every historical
  release: the app could not draw a zone outline on any version but the newest. `manifest_build()`
  gains a `zone_sets =` argument to supply the registry. Resolution returns `NA` rather than
  guessing when a zone type is absent or lists two vintages for one version — a wrong outline draws
  the 2026 Program Areas over scores computed on different geometry and looks entirely plausible.
  A zone type may declare a **canonical** vintage in the registry — a frame applied uniformly to
  every release rather than matched per version. Subregions need it: the two 4-zone vintages are
  indistinguishable by count and v7's own 5-zone set (which adds `FULL`) matches neither, so
  without it every release resolved to `NA` and drew no subregion outline. Program and Planning
  Areas declare none and stay strictly version-matched — their geometry must be the one scored.

# msens 0.15.0

*Publishing surface for the multi-version app: spatial units as reusable vintages, object keys that
identify bytes, full-resolution vector tiles, and a browsable public bucket.*

* **Zone sets are reusable, and keyed on the right column** — `zone_cells()` computes the
  `(zone_set × grid)` intersection **once** so every MST version on that grid reuses it, instead of
  each release recomputing its own. `zone_key_col()` picks the layer's *own* `{zone_type}_key`
  rather than the first column matching `key$` — that bug selected `region_key` for a subregion
  layer, collapsing 20 zones to 3 and, because the resulting order was not total, made
  `zone_geom_hash()` unstable across runs.

* **Version equivalence is asserted on a fixed spatial unit** — `pra_score_delta(zone_set_key=)`
  pins which zone vintage is compared; previously it silently compared whatever each version
  happened to call "programarea", so a geometry change could read as a score change. `score_delta()`
  now refuses two identical labels rather than reporting a self-comparison as agreement.

* **Content-addressed object keys cover the encoding** — `content_hash_encoded()` folds the encoding
  parameters into the key. The payload hash alone is not enough: republishing the same cells with
  different scaling reused the URL, and `/vsicurl` caches headers **per URL**, so stale ranges were
  served against new bytes and low-zoom tiles failed with HTTP 500. `cog_store_index()` now returns
  an empty `character()` for an empty store instead of erroring, so the first publish into a fresh
  store works.

* **Vector tiles at full resolution** — `publish_pmtiles()` / `publish_pmtiles_models()` default to
  `maxzoom = 10` and pass `--simplify-only-low-zooms --no-tiny-polygon-reduction`. Simplification
  previously applied at *every* zoom including the deepest, so the tiles every higher zoom
  overzooms from were coarser than the source, and small polygons were dropped outright at low zoom.

* **Manifest zones carry their tiles** — `manifest_build()` emits `zone_set_key` and a `pmtiles` URL
  per zone vintage, so an app can draw a version's spatial units without knowing their filenames.

* **Browsable public storage** — `s3_list_all()`, `storage_page()`, `build_storage_index()` generate
  the `storage.marinesensitivity.org` index, with per-directory READMEs pointing at the STAC API,
  `curl`, GDAL and `rstac`. Depth is budgeted by child-directory count (`max_child_dirs`) rather
  than a prefix blocklist — the blocklist left whole trees unbrowsable, which defeats the point.

* **Also** — `version_picker_html()` shares one picker between both apps and the docs;
  `cog_tile_url(color=)` renders a flat binary mask on stock titiler; `mdl_key_raw()` is vectorised;
  `sdm_db_path("v3")` falls back to the standard layout instead of hardcoding the legacy path (its
  test had asserted the legacy path *unconditionally*, encoding the bug it should have caught).

# msens 0.14.0

*Foundations for one app serving every MST version (v1–v8), instead of a forked app per release.*

* **New version registry** — `atlas_base_url()`, `atlas_latest()`, `atlas_versions()`,
  `atlas_resolve_ver()`, `atlas_manifest()`, `validate_manifest()`, `manifest_can()`. Resolves
  `?ver=` (or `"latest"`) against the published `latest.txt` / `versions.json`, and reads a
  release's `manifest.json` — the contract that lets an app render a version it has no code for.
  Resolution **never falls back to a hardcoded version** (a plausible-but-wrong default renders the
  wrong science under the right label), a **pre-release is reachable only by name**, and a manifest
  **missing its `capabilities` block is an error** rather than an implicit "everything supported".

* **New grid registry** — `grid_registry()`, `grid_for_ver()`, `grid_spec_for()`, `cell_lonlat()`.
  Two incompatible grids exist and `cell_id` names a different place on each, so every cell id,
  content hash and COG now carries a `grid_id`:
  - `usa05` (v1–v7): 3103 × 2006 at 0.05°, **longitude 0–360** — the frame runs 141.10°E east
    *across the antimeridian* to 296.25 (= 63.75°W), so Alaska/the Aleutians and the East Coast sit
    in one contiguous raster.
  - `global05` (v8): 7200 × 3600 at 0.05°, −180..180.

* **`publish_cog()` handles a 0–360 grid.** Previously it painted cells straight into the source
  frame, which for `usa05` yields x > 180 — out of domain for EPSG:4326, so web tilers misplace the
  raster. Columns are now re-indexed onto the −180..180 lattice (they align exactly:
  141.10 − (−180) = 6422 × 0.05). v8 (`global05`) behavior is unchanged.

* **New content-addressed COG store** — `content_hash_sql()`, `content_hashes()`, `content_key()`,
  `content_url()`, `cog_store_index()`. A model's COG is stored under a hash of its **payload**, not
  of the `.tif` (a GeoTIFF is not byte-reproducible — GDAL stamps `TIFFTAG_DATETIME`/`SOFTWARE` and
  the COG driver's IFD layout varies — so a file digest would change every rebuild and defeat
  dedup). The reduction is order-independent (count + `bit_xor` + `sum`), one pass and no sort.
  Measured: a full release (1.18B rows, 30,061 models) hashes in 19–28 s, and dedup across
  v3/v5/v6/v7 is 120,974 model-rows → 19,766 unique surfaces (**6.12×**; v6↔v7 is 30,061/30,061
  identical). Guarded against the silent failure where DuckDB's UBIGINT `hash()` returns to R as a
  double, truncating to ~15 digits and aliasing distinct models onto one COG.

* **New zone-set registry** — `zone_geom_hash()`, `zone_set_key()`, `zone_set_group()`,
  `validate_zone_sets()`. A spatial unit is identified by its **geometry**
  (`{zone_type}_{YYYY-MM}`), not by the release that used it, so one Program Area is comparable
  across versions and `zone_cell` — which depends only on (geometry × grid) — is computed once per
  `(zone_set_key, grid_id)` rather than re-extracted per release. Measured across every published
  gpkg: `programarea` is 8 files but only **2** distinct geometries (v2 alone; v3–v8 byte-identical),
  `ecoregion` 10 files → 2, `planarea` 3 → 3. `validate_zone_sets()` enforces both directions —
  one key names one geometry, and one geometry carries one key, since the same polygons published
  under two keys would be scored twice and compared as different places.

* **The reference index now reflects the package.** It had drifted badly: two of its five sections
  (`analyze`, and a `Read` holding one topic) keyed on concepts that no functions carry — the
  analysis concept is `calc` — so ~90 of 126 topics fell into one undifferentiated "Other". Twelve
  purpose-based sections now key on the `@concept` tags that actually exist, and
  `pkgdown::check_pkgdown()` passes, so every topic is listed exactly once. `atlas.R`'s four
  exported functions gained the `atlas` concept they were missing.

* **New `Dormant` section.** The H3 hexagon grid (`hex.R`) and its IDW interpolation (`interp.R`) —
  10 exported functions — were built for a v8 direction that was rolled back to the global 0.05°
  raster cell grid. Nothing in `workflows` or `apps` calls them and their SQL is stale under DuckDB
  1.5 (their tests skip), so they are now tagged `@concept dormant` and grouped under a section that
  says so, rather than sitting in "Other" looking current.

* **Fixed the package URLs.** `URL:` and `BugReports:` pointed at `MarineSensitiviti**es**` — a
  nonexistent GitHub org and domain (the real ones are `MarineSensitivity` / `marinesensitivity.org`),
  so every "source"/"report a bug" link in the docs was broken.

# msens 0.13.1

* **`species_for_cells()` reads a `cell_model` of either generation.** The `use_cm` branch joined
  `model USING (mdl_id)` unconditionally — v8's shape. v7's `cell_model` stores `mdl_seq`, which
  `taxon` already joins on directly, so the v7 surface failed outright with
  `Binder Error: Column "mdl_id" does not exist on left side of join`. The model-id column is now
  resolved from `cell_model` itself (`mdl_id` → join `model`; `mdl_key`/`mdl_seq` → use directly).
  Regression-tested against both shapes, with generation-accurate fixtures — a hybrid `taxon`
  carrying both keys makes `.sdm_cols()` pick the wrong one and hides the bug.

# msens 0.13.0

*The `cell_model` tile key stops assuming one grid*

* **`cell_model_tile_sql(col, ncol)` and `cell_model_tiles(cell_id, ncol)` take the grid width**, and
  **`cell_grid_ncol(con)`** resolves it from the database being read (a `cell_grid` sidecar table,
  else the 7200 default). v8's global grid is 7200 columns; **v7's is 3103** (the regional 0-360
  bio-oracle raster, 3103 x 2006). Applying 7200 to v7 ids partitions consistently, so nothing
  errors — the tiles simply stop corresponding to contiguous ground and a compact polygon scatters
  across many of them, losing the pruning `cell_model` exists to provide.
* **`species_for_cells()` now resolves the width from its connection** instead of assuming 7200.
  Every v8 database predates the `cell_grid` table and falls back to the default, so v8 behavior is
  unchanged.
* Regression-tested on both grids, including that the two widths yield *different* tiles — the
  silent-failure mode, since a wrong tile id is still a valid tile id.

# msens 0.12.2

* **The v7 (raster) branch of `cells_in_polygon()` reads only the polygon's window** —
  `terra::extract(..., exact = TRUE)` instead of `terra::rasterize()` followed by
  `terra::values()` over the whole grid. The old form pulled 6.2M cells into memory to find ~450
  and dominated the entire v7 report. Measured on the server for the same polygon:
  **35.16 s → 0.12 s (295x)**, with an **identical `cell_id` set**. `exact = TRUE` yields each
  cell's covered `fraction`, so `pct_covered` keeps its meaning (it weights `area_km2`/`avg_suit`);
  it differs from the old `cover=` values by a mean 0.66pp on edge cells, being the more precise of
  the two. Overlapping features of a multi-part polygon now sum their fractions (capped at 100)
  rather than double-counting a cell.

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
