# zone_style(): the one table both apps draw spatial units from -------------
#
# The point of these is distinguishability: every unit type the apps draw at
# once must differ from the others in a way a reader can see, and Program Areas
# and Ecoregions must keep the look the species app established (white 1px with
# white labels; black 3px with larger black labels).

test_that("program areas: white thin line, white labels; planning areas match", {
  pa <- zone_style("programarea")
  expect_equal(pa$line$color, "white")
  expect_equal(pa$line$width, 1)
  expect_null(pa$line$dasharray)
  expect_equal(pa$label$color, "white")
  expect_identical(zone_style("planarea"), pa)   # v1's analogue draws the same
})

test_that("ecoregions: thicker black line, larger black labels with a halo", {
  er <- zone_style("ecoregion"); pa <- zone_style("programarea")
  expect_equal(er$line$color, "black")
  expect_gt(er$line$width, pa$line$width)
  expect_equal(er$label$color, "black")
  expect_gt(er$label$size, pa$label$size)
  expect_gt(er$label$halo_width, 0)              # black text on a dark ocean needs one
})

test_that("subregions: dashed context line and NO labels", {
  sr <- zone_style("subregion")
  expect_length(sr$line$dasharray, 2)
  expect_null(sr$label)
  expect_null(zone_label_args("subregion"))      # callers skip the label layer
})

test_that("the three units drawn together are pairwise distinguishable", {
  key <- function(t) { s <- zone_style(t)$line
    paste(s$color, s$width, !is.null(s$dasharray)) }
  ks <- vapply(c("programarea", "ecoregion", "subregion"), key, character(1))
  expect_equal(length(unique(ks)), 3)
})

test_that("an unknown unit type gets the muted context style, not an error", {
  x <- zone_style("newunit")
  expect_lt(x$line$opacity, 1)
  expect_lt(x$line$width, zone_style("programarea")$line$width)
  expect_error(zone_style(c("a", "b")))
  expect_error(zone_style(NA_character_))
})

test_that("zone_line_args / zone_label_args are ready to c() into pm specs", {
  la <- zone_line_args("subregion")
  expect_named(la, c("line_color", "line_width", "line_opacity", "line_dasharray"))
  expect_named(zone_line_args("programarea"), c("line_color", "line_width", "line_opacity"))
  lb <- zone_label_args("ecoregion")
  expect_named(lb, c("text_color", "text_size", "text_halo_color", "text_halo_width"))
  expect_equal(lb$text_color, "black")
})
