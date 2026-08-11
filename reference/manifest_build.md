# Build a version manifest from that release's database

Introspects the release rather than being told about it, so a manifest
cannot drift from the data it describes. Everything that differs between
releases — which model id is public, which tables exist, which metrics
are cell-level, which spatial units were scored — is *read*, not
assumed.

## Usage

``` r
manifest_build(
  con,
  ver,
  status = "released",
  grid_id = grid_for_ver(ver),
  base = atlas_base_url(),
  metrics = NULL,
  capabilities = list(),
  zone_tiles = list(),
  zone_sets = NULL,
  extra = list()
)
```

## Arguments

- con:

  open DuckDB connection to that release's database

- ver:

  version label

- status:

  `released`, `prerelease` or `retired`

- grid_id:

  defaults to
  [`grid_for_ver()`](http://marinesensitivity.org/msens/reference/grid_for_ver.md)

- base:

  atlas base URL from
  [`atlas_base_url()`](http://marinesensitivity.org/msens/reference/atlas_base_url.md)

- metrics:

  optional data frame of published metric COGs to merge in
  (`metric_key`, `subregion_key`, `cog`, `rescale_min`, `rescale_max`);
  until score COGs are published the `score_cogs` capability stays FALSE

- capabilities:

  named logicals overriding the derived ones. Needed whenever a surface
  is NOT a table in `con`: v8's `cell_model`, for instance, is published
  as a Parquet directory beside the database, so deriving from `con`
  alone would advertise `cell_species_list = FALSE` and switch off a
  panel that works. Overrides must be justified by checking the RELEASE,
  not by optimism.

- zone_tiles:

  named list of `zone_set_key` -\> PMTiles URL, attached to the zones
  table so an app resolves outlines by VINTAGE instead of a hardcoded
  unversioned filename on the file host

- zone_sets:

  the zone-set registry (`data/zone_sets.csv`), used to resolve
  `zone_set_key` for a release whose own `zone` table predates that
  column. Only v8 stamps `zone_set_key` into the database; v1–v7 record
  a spatial unit as `tbl`/`fld` only, so without the registry every
  historical manifest comes out with no `zone_set_key` and therefore no
  `pmtiles` — the app then cannot draw a single zone outline on any
  version but the newest.

- extra:

  named list merged into the manifest (e.g. `zone_sets`)

## Value

a validated manifest list

## Details

Capabilities are derived from presence, and default to **FALSE**: a
release that lacks `cell_model` must not advertise a per-cell species
list.
