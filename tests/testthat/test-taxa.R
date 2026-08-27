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

test_that("sp_cat_from_taxonomy: one row per branch, no `other`, NA-safe", {
  cls <- c("Mammalia", "Teleostei", "Elasmobranchii", "Hexacorallia", "Reptilia", "Amphibia",
           "Aves", "Bivalvia", NA, "Cheloniidae")
  kgd <- c("Animalia", "Animalia", "Animalia", "Animalia", "Animalia", "Animalia",
           "Animalia", "Animalia", "Plantae", "Animalia")
  expect_equal(
    sp_cat_from_taxonomy(cls, kgd, is_turtle = c(rep(FALSE, 9), TRUE)),
    c("mammal", "fish", "fish", "coral", "reptile", "amphibian",
      "bird", "invertebrate", "primary_producer", "turtle"))
  # BirdLife keys a bird regardless of (missing) WoRMS class
  expect_equal(sp_cat_from_taxonomy(NA, NA, is_botw = TRUE), "bird")
  # unknown class + unknown kingdom is an invertebrate, never `other`
  expect_equal(sp_cat_from_taxonomy(NA, NA), "invertebrate")
  # turtle wins over class (Testudines would otherwise be `reptile`)
  expect_equal(sp_cat_from_taxonomy("Testudines", "Animalia", is_turtle = TRUE), "turtle")
})
