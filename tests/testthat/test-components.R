test_that("components_registry() loads and validates the shipped table", {
  reg <- components_registry()
  expect_s3_class(reg, "tbl_df")
  expect_named(reg, c("key", "label", "short", "order", "colour", "icon", "kind", "label_v10"))
  expect_equal(reg$key[reg$kind == "species" & reg$key != "other"],
               c("bird", "coral", "fish", "invertebrate", "mammal", "primary_producer", "turtle"))
  expect_equal(reg$key[reg$kind == "environment"], "primprod")
  expect_equal(reg$key[nrow(reg)], "other")
  expect_false(is.unsorted(reg$order))
})

test_that("components_registry() refuses a bad table", {
  good <- utils::read.csv(system.file("extdata/components.csv", package = "msens"),
                          stringsAsFactors = FALSE, colClasses = "character")
  write_bad <- function(d) { f <- tempfile(fileext = ".csv"); utils::write.csv(d, f, row.names = FALSE); f }
  d <- good; d$key[2] <- d$key[1]
  expect_error(components_registry(write_bad(d)), "duplicate `key`")
  d <- good; d$order[2] <- d$order[1]
  expect_error(components_registry(write_bad(d)), "duplicate `order`")
  d <- good; d$kind[1] <- "taxon"
  expect_error(components_registry(write_bad(d)), "kind")
  d <- good; d$label[3] <- ""
  expect_error(components_registry(write_bad(d)), "empty `label`")
  expect_error(components_registry(write_bad(good[, setdiff(names(good), "kind")])), "lacks column")
})

test_that("component_label(): v10 wording by default, v11 names on request", {
  expect_equal(component_label("primary_producer"), "Primary Producers")
  expect_equal(component_label("primary_producer", v11 = TRUE), "Flora")
  expect_equal(component_label("primprod"), "Primary productivity")
  expect_equal(component_label("primprod", v11 = TRUE), "Productivity")
  expect_equal(component_label(c("bird", "turtle", "other")), c("Seabirds", "Sea Turtles", "Other Species"))
  expect_true(is.na(component_label("nope")))
})

test_that(".metric_short_label(v11 = TRUE) uses the v11 names; the default is unchanged", {
  expect_equal(.metric_short_label("extrisk_primary_producer_v10_global", v11 = TRUE),
               "Flora: across the nation (global range)")
  expect_equal(.metric_short_label("extrisk_primary_producer_v10_global"),
               "Primary Producers: across the nation (global range)")
  expect_equal(.metric_short_label("primprod_ecoregion_rescaled", v11 = TRUE), "Productivity (ecoregion-rescaled)")
  expect_equal(.metric_short_label("primprod_v10", v11 = TRUE), "Productivity, across the nation")
  expect_equal(.metric_short_label("extrisk_bird", v11 = TRUE), "Seabirds: extinction risk")
  expect_equal(.metric_short_label("extrisk_reptile", v11 = TRUE), "Reptile: extinction risk")   # coined fallback
})

test_that("the shipped components.csv is identical to workflows/data/components.csv", {
  src <- normalizePath(file.path(testthat::test_path(), "..", "..", "..", "workflows", "data", "components.csv"),
                       mustWork = FALSE)
  skip_if_not(file.exists(src), "workflows checkout not beside msens")
  shipped <- system.file("extdata/components.csv", package = "msens")
  expect_identical(readLines(shipped), readLines(src))
})
