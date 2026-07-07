test_that("mdl_key_raw composes native per-dataset keys", {
  expect_equal(mdl_key_raw("am", "Fis-29291"), "am|Fis-29291")
  expect_equal(mdl_key_raw("gm", 1234, "01"), "gm|1234|01")
  expect_equal(mdl_key_raw("nc", "kelp-guild", "summer"), "nc|kelp-guild|summer")
  # vectorised over sp_id
  expect_equal(mdl_key_raw("am", c("A", "B")), c("am|A", "am|B"))
  # dataset_key must be a scalar without the separator
  expect_error(mdl_key_raw(c("am", "gm"), "x"))
  expect_error(mdl_key_raw("a|b", "x"))
})

test_that("mdl_key_merged composes taxadb-prefixed keys", {
  expect_equal(mdl_key_merged("WORMS", 137209), "ms_merge|WORMS:137209")
  expect_equal(mdl_key_merged("botw", 22694927), "ms_merge|BOTW:22694927")  # case-insensitive
  expect_equal(mdl_key_merged("WORMS", c(1, 2)), c("ms_merge|WORMS:1", "ms_merge|WORMS:2"))
  expect_error(mdl_key_merged("FOO", 1))   # unknown authority
})

test_that("mdl_key_parse splits into components", {
  d <- mdl_key_parse(c("am|Fis-29291", "gm|1234|01", "ms_merge|WORMS:137209"))
  expect_equal(d$dataset_key, c("am", "gm", "ms_merge"))
  expect_equal(d$sp_id, c("Fis-29291", "1234", "WORMS:137209"))
  expect_equal(d$interval, c(NA, "01", NA))
  expect_equal(d$taxon_authority, c(NA, NA, "WORMS"))
  expect_equal(d$taxon_id, c(NA, NA, "137209"))
})

test_that("mdl_key_parse round-trips the composers", {
  expect_equal(mdl_key_parse(mdl_key_raw("am", "Fis-29291"))$sp_id, "Fis-29291")
  expect_equal(mdl_key_parse(mdl_key_raw("gm", "1234", "01"))$interval, "01")
  p <- mdl_key_parse(mdl_key_merged("BOTW", 22694927))
  expect_equal(p$taxon_authority, "BOTW")
  expect_equal(p$taxon_id, "22694927")
})
