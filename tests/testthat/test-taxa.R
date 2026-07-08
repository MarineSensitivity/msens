test_that("clean_sci_name strips synonym/ssp. notation + collapses whitespace", {
  expect_equal(
    clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi"),
    "Acipenser oxyrinchus desotoi")
  expect_equal(clean_sci_name("Chelonia  mydas"), "Chelonia mydas")   # squish
  expect_equal(clean_sci_name("Enhydra lutris ssp. nereis"), "Enhydra lutris nereis")
  expect_equal(clean_sci_name("Chelonia mydas"), "Chelonia mydas")    # already clean
})

test_that("clean_sci_name binomial reduces a trinomial to Genus species", {
  expect_equal(
    clean_sci_name("Acipenser oxyrinchus (=oxyrhynchus) desotoi", binomial = TRUE),
    "Acipenser oxyrinchus")
  expect_equal(clean_sci_name("Ursus maritimus", binomial = TRUE), "Ursus maritimus")
  # vectorised
  expect_equal(
    clean_sci_name(c("Genus species subsp", "Aa bb"), binomial = TRUE),
    c("Genus species", "Aa bb"))
})
