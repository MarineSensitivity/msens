# Compose a merged (`ms_merge`) model key

The stable id for a cross-dataset merged model: the taxon id with a
**taxadb authority prefix**, `ms_merge|{AUTHORITY}:{taxon_id}`.
Vectorised over `taxon_id` (and `taxon_authority`).

## Usage

``` r
mdl_key_merged(taxon_authority, taxon_id)
```

## Arguments

- taxon_authority:

  taxadb authority, one of `"WORMS"`, `"BOTW"`, `"ITIS"`, `"GBIF"`,
  `"SLB"` (case-insensitive)

- taxon_id:

  taxon id(s) in that authority

## Value

character `mdl_key`(s), e.g. `"ms_merge|WORMS:137209"`

## Examples

``` r
mdl_key_merged("WORMS", 137209)  # "ms_merge|WORMS:137209"
#> [1] "ms_merge|WORMS:137209"
mdl_key_merged("botw", 22694927) # "ms_merge|BOTW:22694927"
#> [1] "ms_merge|BOTW:22694927"
```
