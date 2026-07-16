# Getting started with the Marine Sensitivity Atlas

The **`msens`** package reads the published **Marine Sensitivity Atlas**
(`v8` “marine-atlas”) — species distribution models from many sources
merged onto a global 0.05° grid, scored for marine sensitivity over the
US study area, and released as partitioned Parquet on S3 with a
[STAC](https://stacspec.org) catalog and
[titiler](https://developmentseed.org/titiler/) tile services.
Everything below runs against the **live public release** — no download,
no credentials.

``` r

library(msens)
library(dplyr)
```

## 1. Attach the atlas

A single
[`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
call configures a DuckDB connection to the S3 release and creates the
table **views**, so every reader and score helper composes off one
connection. Use `anon = TRUE` for anonymous public reads of the released
tables.

``` r

con <- attach_atlas(anon = TRUE)

# the atlas tables are now queryable by name
DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables
                      WHERE table_schema = 'main' ORDER BY 1")$table_name
#>  [1] "cell"         "cell_metric"  "dataset"      "metric"       "model"       
#>  [6] "native_asset" "taxon"        "zone"         "zone_cell"    "zone_metric"
```

## 2. Browse — datasets and taxa

``` r

tbl(con, "dataset") |>
  arrange(sort_order) |>
  select(ds_key, name_short, response_type, native_format) |>
  collect() |>
  knitr::kable()
```

| ds_key | name_short | response_type | native_format |
|:---|:---|:---|:---|
| ms_merge | Marine Sensitivity Merged Model, 2026-03-25 | mixed | parquet |
| am | AquaMaps suitability for all marine taxa (except birds) globally, 2019 | suitability | raster |
| ca_nmfs | NMFS Core Areas, 2019 | range | vector |
| ch_nmfs | NMFS Critical Habitat for USA, 2025-05-12 | range | vector |
| ch_fws | FWS Critical Habitat for USA, 2025-05-05 | range | vector |
| rng_fws | FWS Current Range Maps, 2025-07-23 | range | vector |
| bl | BirdLife Birds of the World, 2024 | range | vector |
| rng_iucn | IUCN Red List Range Maps, 2025-2 | range | vector |
| rng_turtle_swot_dps | SWOT Global + NMFS DPS turtle ranges | range | vector |
| gm | NOAA SEFSC (density) | density | vector |
| nc | NOAA NCCOS (density) | density | raster |

How many merged taxa are valid globally vs. specifically in US waters,
per scoring category?

``` r

tbl(con, "taxon") |>
  group_by(sp_cat) |>
  summarise(
    valid_global = sum(as.integer(is_valid_global), na.rm = TRUE),
    valid_us     = sum(as.integer(is_valid_global & is_valid_usa), na.rm = TRUE),
    .groups = "drop") |>
  arrange(desc(valid_global)) |>
  collect() |>
  knitr::kable()
```

| sp_cat           | valid_global | valid_us |
|:-----------------|-------------:|---------:|
| invertebrate     |        10080 |     9364 |
| fish             |         9242 |     6286 |
| bird             |         8045 |      880 |
| coral            |         1206 |      783 |
| primary_producer |          405 |      319 |
| mammal           |          133 |       75 |
| turtle           |            6 |        6 |
| reptile          |            2 |        2 |
| amphibian        |            0 |        0 |

## 3. Search the STAC catalog

Each dataset is a STAC collection; each model is an item whose assets
point at the published Cloud-Optimized GeoTIFFs, PMTiles, and Parquet.
Read the **static** catalog with
[`rstac::read_stac()`](https://brazil-data-cube.github.io/rstac/reference/static_functions.html)
(use
[`rstac::stac()`](https://brazil-data-cube.github.io/rstac/reference/stac.html)
only for STAC *API* servers).

``` r

library(rstac)
v8 <- read_stac("https://file.marinesensitivity.org/stac/v8/collection.json")

# dataset collections under v8
basename(dirname(vapply(Filter(\(l) l$rel == "child", v8$links), \(l) l$href, character(1))))
#>  [1] "ms_merge"            "am"                  "ca_nmfs"            
#>  [4] "ch_nmfs"             "ch_fws"              "rng_fws"            
#>  [7] "bl"                  "rng_iucn"            "rng_turtle_swot_dps"
#> [10] "gm"                  "nc"
```

``` r

# one dataset's item + its published assets (native vs. gridded-model representations).
# item links are relative to the collection, so resolve against its base URL.
ch_base   <- "https://file.marinesensitivity.org/stac/v8/ch_fws/"
ch        <- read_stac(paste0(ch_base, "collection.json"))
item_href <- Filter(\(l) l$rel == "item", ch$links)[[1]]$href     # "./…json"
it        <- read_stac(paste0(ch_base, sub("^\\./", "", item_href)))
tibble(
  asset = names(it$assets),
  type  = vapply(it$assets, \(a) if (is.null(a$type)) "" else a$type, character(1)),
  href  = vapply(it$assets, \(a) basename(a$href), character(1))) |>
  knitr::kable()
```

| asset | type | href |
|:---|:---|:---|
| data | application/vnd.apache.parquet | dist_merged |
| pmtiles_native | application/vnd.pmtiles | ch_fws |
| cog_model | image/tiff; application=geotiff; profile=cloud-optimized | vec_grid |

> **Python** reads the same catalog with
> [`pystac`](https://pystac.readthedocs.io):
>
> ``` python
> import pystac
> v8   = pystac.Collection.from_file("https://file.marinesensitivity.org/stac/v8/collection.json")
> item = next(pystac.Collection.from_file(
>     "https://file.marinesensitivity.org/stac/v8/ch_fws/collection.json").get_items())
> {k: a.media_type for k, a in item.assets.items()}
> ```

## 4. Retrieve and map a distribution

The `native_asset` table maps each model to its published tile services.
Retrieve a merged taxon’s whole-range Cloud-Optimized GeoTIFF and hand
it to
[`cog_tile_url()`](http://marinesensitivity.org/msens/reference/cog_tile_url.md),
which builds an XYZ tile template that drops straight into a leaflet
map. (For the US-scored surface instead, `cell_tile_url(mdl_key = mk)`
serves the `model_cell` cells.)

``` r

library(leaflet)
mk  <- "ms_merge|WORMS:137209"                    # Dermochelys coriacea (Leatherback Turtle)

# the published whole-range COG for this merged taxon
cog <- tbl(con, "native_asset") |>
  filter(ms_merge_key == mk, ds_key == "ms_merge") |>
  pull(asset_url) |> head(1)
url <- cog_tile_url(cog, colormap = "spectral_r", rescale = c(1, 100))

leaflet() |>
  addProviderTiles("CartoDB.DarkMatter") |>
  addTiles(urlTemplate = url, options = tileOptions(opacity = 0.9)) |>
  setView(0, 20, zoom = 1)
```

## 5. Map the sensitivity score surfaces

Scoring produces a per-cell **value for each metric** — the equal-weight
composite marine-sensitivity score and its per-category components
(bird, mammal, fish, …, ecoregion-rescaled to 0–100). These live in the
`cell_metric` table and are served as raster tiles by titiler-v8, which
runs a `cell_id → value` SQL query over the scored cells on demand
(there is no pre-rendered COG per metric — the cells *are* the raster).

``` r

library(glue)

# the composite plus each per-category, ecoregion-rescaled component
score_metrics <- tbl(con, "metric") |>
  filter(grepl("equalweights$|^extrisk_[a-z]+_ecoregion_rescaled$", metric_key)) |>
  pull(metric_key) |> sort()

# a readable label from the metric_key (descriptions are sparse in the release)
label_of <- function(mk) ifelse(
  grepl("equalweights$", mk), "composite (equal-weight)",
  paste0(sub("^extrisk_([a-z]+)_.*$", "\\1", mk), " extinction-risk"))

# the (cell_id, value) SELECT titiler renders — the same path the scores app uses
cell_sql <- function(mk) glue(
  "SELECT cm.cell_id, cm.val AS value FROM cell_metric cm ",
  "JOIN metric m ON cm.metric_seq = m.metric_seq WHERE m.metric_key = '{mk}'")
```

Ask titiler for each surface’s **raster statistics** — proof the served
tiles carry real per-cell values (not a static image), computed live
over the scored cells:

``` r

stats <- lapply(score_metrics, \(mk) cell_stats(cell_sql(mk)))
names(stats) <- score_metrics

purrr::imap_dfr(stats, \(s, mk) tibble::tibble(
  surface = label_of(mk), cells = s$n,
  min = round(s$min, 1), mean = round(s$mean, 1),
  median = round(s$p50, 1), p98 = round(s$p98, 1), max = round(s$max, 1))) |>
  arrange(desc(mean)) |>
  knitr::kable(caption = "titiler raster statistics per score surface (US study area)")
```

| surface                      |  cells | min | mean | median |  p98 | max |
|:-----------------------------|-------:|----:|-----:|-------:|-----:|----:|
| mammal extinction-risk       | 623212 |   0 | 40.6 |   38.8 | 89.3 | 100 |
| turtle extinction-risk       | 406103 |   0 | 36.8 |   23.8 | 94.2 | 100 |
| bird extinction-risk         | 623212 |   0 | 36.2 |   38.6 | 83.5 | 100 |
| composite (equal-weight)     | 623212 |   0 | 22.1 |   20.0 | 56.0 |  96 |
| fish extinction-risk         | 589800 |   0 | 18.1 |   11.3 | 71.3 | 100 |
| invertebrate extinction-risk | 623212 |   0 | 14.9 |    3.6 | 74.7 | 100 |
| coral extinction-risk        | 542955 |   0 | 11.1 |    3.5 | 65.6 | 100 |

titiler raster statistics per score surface (US study area) {.table}

Hand each metric’s SQL to
[`cell_tile_url()`](http://marinesensitivity.org/msens/reference/cell_tile_url.md)
for a leaflet layer, rescaled to its own range. A layers control toggles
between the composite and its components — one raster surface at a time:

``` r

# add components first, composite last (so it is the default-visible base layer)
ordered <- c(setdiff(score_metrics, grep("equalweights$", score_metrics, value = TRUE)),
             grep("equalweights$", score_metrics, value = TRUE))

m <- leaflet() |> addProviderTiles("CartoDB.DarkMatter") |> setView(-105, 38, zoom = 3)
for (mk in ordered) {
  s <- stats[[mk]]
  m <- m |> addTiles(
    urlTemplate = cell_tile_url(cell_sql(mk), colormap = "spectral_r",
                                rescale = c(s$min, s$max)),
    group = label_of(mk), options = tileOptions(opacity = 0.9))
}
m |> addLayersControl(
  baseGroups = label_of(rev(ordered)),                 # composite listed first
  options = layersControlOptions(collapsed = FALSE))
```

## 6. Score a Program Area

[`scores_for_pra()`](http://marinesensitivity.org/msens/reference/scores_for_pra.md)
returns the per-category marine-sensitivity scores for a BOEM Program
Area — ready for a flower plot, where each petal’s height is the
category score.

``` r

sc <- scores_for_pra(con, "ALA")                  # ALA = a BOEM Program Area key
knitr::kable(sc[c("component", "score")], digits = 1)
```

| component        | score |
|:-----------------|------:|
| bird             |  55.1 |
| coral            |  15.3 |
| fish             |  37.5 |
| invertebrate     |  14.6 |
| mammal           |  35.5 |
| primary producer |  10.4 |
| turtle           |  70.2 |
| primprod         |   1.4 |

``` r

plot_flower(
  sc,
  fld_category = component,
  fld_height   = score,
  fld_width    = even,
  tooltip_expr = "{component}: {round(score, 1)}",
  title        = "ALA")
```

## 7. Species

The `taxon` table carries each merged taxon’s category, governing
extinction-risk score, and protection flags — e.g. the highest-risk
mammals in US waters:

``` r

tbl(con, "taxon") |>
  filter(sp_cat == "mammal", is_valid_usa, er_score >= 80) |>
  select(scientific_name, common_name, extrisk_code, er_score, is_mmpa) |>
  arrange(desc(er_score), scientific_name) |>
  head(10) |>
  collect() |>
  knitr::kable()
```

| scientific_name | common_name | extrisk_code | er_score | is_mmpa |
|:---|:---|:---|---:|:---|
| Balaena mysticetus | Bowhead Whale | NMFS:EN | 100 | TRUE |
| Balaenoptera borealis | Sei Whale | NMFS:EN | 100 | TRUE |
| Balaenoptera edeni | Bryde’s Whale | NMFS:EN | 100 | TRUE |
| Balaenoptera musculus | Blue Whale | NMFS:EN | 100 | TRUE |
| Balaenoptera physalus | Fin Whale | NMFS:EN | 100 | TRUE |
| Balaenoptera ricei | Rice’s Whale | NMFS:EN | 100 | TRUE |
| Delphinapterus leucas | Beluga Whale | NMFS:EN | 100 | TRUE |
| Eschrichtius robustus | Gray Whale | NMFS:EN | 100 | TRUE |
| Eubalaena glacialis | North Atlantic Right Whale | NMFS:EN | 100 | TRUE |
| Eubalaena japonica | North Pacific Right Whale | NMFS:EN | 100 | TRUE |

To go the other way — *which species occur in a set of cells* — use
`species_for_cells(con, cells)`, which joins the merged `model_cell`
surface to `taxon` (that view needs credentials, so call
[`attach_atlas()`](http://marinesensitivity.org/msens/reference/attach_atlas.md)
without `anon`).
