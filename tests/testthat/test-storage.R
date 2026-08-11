# The bucket denies anonymous ListBucket (verified: 403), so a browser sees only
# what a generated index.html tells it. The cost of this index is the number of
# PAGES, not objects: a directory of 20,000 files is one page, while a tree of
# 17,765 Hive partition DIRECTORIES would be 17,765 pages nobody reads.

objs <- function(extra = NULL) rbind(data.frame(
  key = c("marine-atlas/latest.txt",
          "marine-atlas/versions.json",
          "marine-atlas/",                                   # 0-byte dir marker
          "marine-atlas/v8/manifest.json",
          "marine-atlas/v8/tables/taxon.parquet",
          "marine-atlas/v8/tables/zone.parquet",
          "marine-atlas/cog/global05/abc.tif",
          "marine-atlas/cog/global05/def.tif",
          "marine-atlas/index.html"),                        # our own page
  size = c(3, 1200, 0, 4000, 5e6, 2e6, 3e5, 3e5, 999),
  stringsAsFactors = FALSE), extra)

# a Hive tree: many partition DIRECTORIES, one file each
hive <- function(n) data.frame(
  key = sprintf("marine-atlas/v8/serve/model_cell/mdl_id=%d/data_0.parquet", seq_len(n)),
  size = 1e6, stringsAsFactors = FALSE)

test_that("a directory of many FILES is browsable - it is only one page", {
  ix <- build_storage_index(objs())
  expect_true("marine-atlas/cog/index.html" %in% ix$key)
  expect_true("marine-atlas/cog/global05/index.html" %in% ix$key)
  # and it links, rather than being labelled unbrowsable
  cog <- ix$html[ix$key == "marine-atlas/cog/index.html"]
  expect_match(cog, "href='[^']*/marine-atlas/cog/global05/'")
  expect_false(grepl("not browsable", cog))
})

test_that("a tree of many DIRECTORIES is listed but not expanded", {
  ix <- build_storage_index(objs(hive(600)), max_child_dirs = 500)
  expect_false(any(grepl("mdl_id=", ix$key)))          # no page PER partition
  # ...but the crowded directory itself IS browsable: one page listing them all
  expect_true("marine-atlas/v8/serve/model_cell/index.html" %in% ix$key)
  mc <- ix$html[ix$key == "marine-atlas/v8/serve/model_cell/index.html"]
  expect_match(mc, "mdl_id=1/")
  # each partition holds exactly one object, so it links STRAIGHT to the file
  expect_match(mc, "amazonaws[^']*mdl_id=1/data_0\\.parquet")
  # and `serve/` still links down into it
  expect_match(ix$html[ix$key == "marine-atlas/v8/serve/index.html"],
               "href='[^']*/serve/model_cell/'")
})

test_that("a multi-file directory under a crowded parent is labelled, not dead-linked", {
  multi <- rbind(hive(600), data.frame(
    key = c("marine-atlas/v8/serve/model_cell/mdl_id=1/data_1.parquet"),
    size = 1e6, stringsAsFactors = FALSE))
  ix <- build_storage_index(objs(multi), max_child_dirs = 500)
  mc <- ix$html[ix$key == "marine-atlas/v8/serve/model_cell/index.html"]
  expect_match(mc, "partitioned data")
})

test_that("a tree just under the threshold IS expanded", {
  ix <- build_storage_index(objs(hive(3)), max_child_dirs = 500)
  expect_true("marine-atlas/v8/serve/model_cell/mdl_id=1/index.html" %in% ix$key)
})

test_that("generated pages and directory markers are never indexed", {
  ix <- build_storage_index(objs())
  expect_false(any(grepl("index\\.html</a>", ix$html)))
  root <- ix$html[ix$key == "marine-atlas/index.html"]
  expect_false(grepl("<td><a[^>]*></a></td>", root))    # no blank-name row
})

test_that("huge directories are capped with an explicit note", {
  many <- data.frame(key = sprintf("marine-atlas/native/am/f%04d.tif", 1:50),
                     size = 1e5, stringsAsFactors = FALSE)
  ix <- build_storage_index(objs(many), max_rows = 10)
  am <- ix$html[ix$key == "marine-atlas/native/am/index.html"]
  expect_match(am, "showing 10 of 50 entries")
})

test_that("folder links go to the browse host, file links to the object host", {
  ix <- build_storage_index(objs())
  root <- ix$html[ix$key == "marine-atlas/index.html"]
  expect_match(root, "https://storage\\.marinesensitivity\\.org/marine-atlas/v8/")
  expect_match(root, "amazonaws\\.com/oceanmetrics\\.io-public/marine-atlas/latest\\.txt")
})

test_that("pages are self-contained and empty input is not an error", {
  h <- storage_page("t", "s", "<p>x</p>", "c")
  expect_match(h, "<style>")
  expect_false(grepl("<link[^>]+stylesheet", h))
  expect_equal(nrow(build_storage_index(data.frame(key = character(), size = numeric()))), 0L)
})
