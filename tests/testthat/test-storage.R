# The bucket denies anonymous ListBucket (verified: 403), so a browser can only
# see what a generated index.html tells it. Two failure modes matter: linking to
# a folder URL with no object behind it (every link 404s), and walking a
# machine-only Hive tree into tens of thousands of pages nobody reads.

objs <- function() data.frame(
  key = c("marine-atlas/latest.txt",
          "marine-atlas/versions.json",
          "marine-atlas/v8/manifest.json",
          "marine-atlas/v8/tables/taxon.parquet",
          "marine-atlas/v8/tables/zone.parquet",
          "marine-atlas/v8/serve/model_cell/mdl_id=1/data_0.parquet",
          "marine-atlas/v8/serve/model_cell/mdl_id=2/data_0.parquet",
          "marine-atlas/cog/global05/abc.tif",
          "marine-atlas/index.html"),           # our own page: must be ignored
  size = c(3, 1200, 4000, 5e6, 2e6, 1e6, 1e6, 3e5, 999),
  stringsAsFactors = FALSE)

test_that("a page is generated for every browsable ancestor directory", {
  ix <- build_storage_index(objs())
  expect_true("index.html" %in% ix$key)                       # bucket root
  expect_true("marine-atlas/index.html" %in% ix$key)
  expect_true("marine-atlas/v8/index.html" %in% ix$key)
  expect_true("marine-atlas/v8/tables/index.html" %in% ix$key)
})

test_that("generated index.html objects are never themselves indexed", {
  ix <- build_storage_index(objs())
  expect_false(any(grepl("index\\.html</a>", ix$html)))
})

test_that("machine-only trees are summarized, not walked", {
  ix <- build_storage_index(objs())
  # 17,765 Hive partitions would otherwise become 17,765 pages
  expect_false(any(grepl("model_cell/mdl_id=", ix$key)))
  expect_false(any(grepl("^marine-atlas/cog/", ix$key)))
  v8 <- ix$html[ix$key == "marine-atlas/v8/index.html"]
  expect_match(v8, "not browsable")            # says so rather than dead-linking
})

test_that("folder links point at the browse host, file links at the object host", {
  ix <- build_storage_index(objs())
  root <- ix$html[ix$key == "marine-atlas/index.html"]
  expect_match(root, "https://storage\\.marinesensitivity\\.org/marine-atlas/v8/")
  expect_match(root, "https://s3\\.us-east-1\\.amazonaws\\.com/oceanmetrics\\.io-public/marine-atlas/latest\\.txt")
})

test_that("pages are self-contained (served straight off a bucket)", {
  h <- storage_page("t", "s", "<p>x</p>", "c")
  expect_match(h, "<style>")
  expect_false(grepl("<link[^>]+stylesheet", h))
  expect_false(grepl("<script", h))
})

test_that("sizes and counts render, and empty input is not an error", {
  ix <- build_storage_index(objs())
  expect_match(ix$html[ix$key == "marine-atlas/v8/tables/index.html"], "MB")
  expect_equal(nrow(build_storage_index(data.frame(key = character(), size = numeric()))), 0L)
})
