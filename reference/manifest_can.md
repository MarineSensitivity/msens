# Does this version support a named capability?

Unknown capabilities are `FALSE`, never `TRUE` — the app must not offer
a panel a release never declared.

## Usage

``` r
manifest_can(m, what)
```

## Arguments

- m:

  manifest from
  [`atlas_manifest()`](http://marinesensitivity.org/msens/reference/atlas_manifest.md)

- what:

  capability name, e.g. `"cell_species_list"`

## Value

logical scalar
