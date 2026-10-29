# Guards encounter_weight() (v11 Q3b): the tiers, the cap at 1, the floor, the gate.

row <- function(rate, p_core, verdict = "kept", override = NA_character_, key = "sp") {
  data.frame(mdl_key = key, zone_key = "Z", rate = rate, p_core = p_core, verdict = verdict,
             override = override, stringsAsFactors = FALSE)
}

test_that("a kept zone below its reference is weighted by the rate ratio; at or above it the weight is 1", {
  expect_equal(encounter_weight(row(0.1, 0.4))$w, 0.25)
  expect_equal(encounter_weight(row(0.1, 0.4))$w_reason, "rate ratio")
  expect_equal(encounter_weight(row(0.4, 0.4))$w, 1)
  expect_equal(encounter_weight(row(0.8, 0.4))$w, 1)           # never upweighted
  expect_equal(encounter_weight(row(0.8, 0.4))$w_reason, "at reference")
})

test_that("the gate wins: a dropped zone is 0 whatever its ratio", {
  expect_equal(encounter_weight(row(0.3, 0.4, verdict = "dropped"))$w, 0)
  expect_equal(encounter_weight(row(0, 0.4, verdict = "dropped"))$w_reason, "dropped")
})

test_that("unsampled is unknown, not rare: unsearched and overridden zones keep weight 1", {
  expect_equal(encounter_weight(row(0, 0.4, verdict = "unsearched"))$w, 1)
  expect_equal(encounter_weight(row(0.01, 0.4, override = "protected"))$w, 1)
  expect_equal(encounter_weight(row(0.01, 0.4, override = "narrow range"))$w_reason, "override")
  # a dropped row with an override cannot exist (the rule turns it into kept) -- but if handed one, the
  # override wins over the ratio and the gate wins over both only when the verdict says dropped
  expect_equal(encounter_weight(row(0.01, 0.4, verdict = "kept", override = "protected"))$w, 1)
})

test_that("no reference (p_core 0 or NA) falls back to 1", {
  expect_equal(encounter_weight(row(0.1, 0))$w, 1)
  expect_equal(encounter_weight(row(0.1, NA_real_))$w_reason, "no reference")
})

test_that("the floor bounds a kept zone's weight from below but never lifts dropped or touches 1", {
  x <- rbind(row(0.01, 0.4), row(0.3, 0.4, verdict = "dropped"), row(0.5, 0.4))
  w <- encounter_weight(x, floor = 0.1)$w
  expect_equal(w, c(0.1, 0, 1))
  expect_error(encounter_weight(x, floor = 1.5), "floor")
})

test_that("bad input errors: missing columns, unknown verdict", {
  expect_error(encounter_weight(data.frame(rate = 1)), "columns")
  expect_error(encounter_weight(row(0.1, 0.4, verdict = "maybe")), "verdict")
})

test_that("the right whale shape: 3 of 79 blocks against 0.41 on the Northeast shelf -> a 9 % weight, 0 once dropped", {
  rw <- row(3 / 79, 0.41)
  expect_equal(round(encounter_weight(rw)$w, 3), 0.093)
  expect_equal(encounter_weight(row(3 / 79, 0.41, verdict = "dropped"))$w, 0)
})
