# Resolve a release's zone columns to zone-set keys via the registry

Only v8 stamps `zone_set_key` into its own `zone` table; v1–v7 identify
a spatial unit by `fld` (`programarea_key`) and a per-version table name
(`ply_programareas_2026_v7`). This maps the former to the vintage that
release actually used, by matching `zone_type` and finding `ver` in the
registry's space-separated `versions` column.

## Usage

``` r
zone_set_resolve(ver, fld, zone_sets)
```

## Arguments

- ver:

  MST version, e.g. `"v7"`

- fld:

  zone key column(s) from the release's `zone` table

- zone_sets:

  the registry (`data/zone_sets.csv`)

## Value

character vector of `zone_set_key`, `NA` where unresolvable

## Details

Returns `NA` rather than guessing when a release's zone type is absent
from the registry, or when the registry lists more than one vintage of
that type for the same version. A wrong outline is worse than a missing
one: it would draw the 2026 Program Areas over scores computed on a
different geometry and look entirely plausible.
