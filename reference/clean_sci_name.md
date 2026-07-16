# Clean a scientific name for taxonomic matching

Normalizes messy source names (esp. USFWS) so they match WoRMS/BOTW:
drops parenthetical synonym notation like `(=oxyrhynchus)`, an
`spp.`/`ssp.` marker, and collapses whitespace. Use `binomial = TRUE` to
also reduce a trinomial (subspecies) to its `Genus species` binomial — a
useful fallback when WoRMS has the species but not the subspecies.

## Usage

``` r
clean_sci_name(x, binomial = FALSE)
```

## Arguments

- x:

  character vector of scientific names

- binomial:

  logical; if `TRUE`, keep only the first two words (default `FALSE`)

## Value

cleaned character vector

## Examples

``` r
clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi")  # "Acipenser oxyrinchus desotoi"
#> [1] "Acipenser oxyrinchus desotoi"
clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi", binomial = TRUE)  # "Acipenser oxyrinchus"
#> [1] "Acipenser oxyrinchus"
```
