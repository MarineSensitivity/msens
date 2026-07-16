# v8 per-taxon merge rules (single source of truth)

The SQL that defines how per-dataset model cells are merged into one
surface per taxon. `merge_models.qmd` executes these strings and
`tests/testthat/test-merge.R` asserts them against synthetic fixtures,
so the notebook and the tests can never drift. Changing a rule here that
breaks a documented case fails the unit tests — the guard against the
merge logic being silently lost by a wrong sequence or minor tweak.

## Usage

``` r
merge_sql()
```

## Value

named list of SQL strings: `b_range`, `b_am_all`, `b_am_rng` (CREATE OR
REPLACE TABLE), `global` and `us` (SELECT).

## Details

The batch caller must first materialize three input relations in the
connection:

- `b`:

  `(ms_merge_key, ds_key, cell_id, val)` — the raw model cells for a
  batch of taxa (all datasets; `ds_key = 'am'` is AquaMaps, anything
  else is a range/expert dataset).

- `taxon`:

  `(ms_merge_key, er_score, ...)` — governing extinction-risk score per
  taxon.

- `taxon_flags`:

  `(ms_merge_key, has_am, has_range)` — GLOBAL presence flags from the
  crosswalk `taxon_model` (has_range = the taxon has ANY non-am dataset
  anywhere, NOT just in the US). This global scope is what enforces the
  IUCN-range constraint — see below.

- `us_cells`:

  `(cell_id)` — the in-USA cell ids (scoring extent).

`merge_sql()$b_range`, `$b_am_all`, `$b_am_rng` create the
intermediates; then `$global` and `$us` are the two output surfaces:

**GLOBAL viz surface** (`$global`) = am ∪ range (FULL OUTER of the range
footprint valued by governing er and the taxon's WHOLE am footprint).
am-only taxa are omitted (they reuse am COGs). This is the honest
whole-range merged model painted to COGs.

**US scoring surface** (`$us`) = v7-faithful, US-boundary-aware,
IUCN-CONSTRAINED:

1.  range footprint ∩ US valued `max(er, am-at-range)` — am BEYOND the
    range is MASKED (the expert range constrains AquaMaps
    over-prediction). Covers range-only + "both" taxa.

2.  TRUE am-only taxa (GLOBAL `has_range = FALSE`, i.e. no range dataset
    anywhere) keep their RAW am ∩ US (no dedup — a taxon with \>1
    AquaMaps model keeps duplicate cells).

A species that HAS an expert range whose polygons lie ENTIRELY outside
the US therefore gets NO US presence: its range ∩ US is empty (1) and it
is excluded from (2) because global has_range is TRUE. This is the
`iucn_range_outside_us_eez` exclusion (e.g. *Sotalia guianensis*, a
river dolphin AquaMaps over-predicts into US waters). Keying (2) on
GLOBAL has_range — not range-in-US — is the crux; keying it on
range-in-US silently re-introduces ~750 such species.

## Examples

``` r
if (FALSE) { # \dontrun{
  msq <- merge_sql()
  DBI::dbExecute(con, msq$b_range); DBI::dbExecute(con, msq$b_am_all)
  DBI::dbExecute(con, msq$b_am_rng)
  us <- DBI::dbGetQuery(con, msq$us)      # US scoring surface for the batch
  gl <- DBI::dbGetQuery(con, msq$global)  # global whole-range surface for the batch
} # }
```
