# Tests for iycf_recode_yesno

test_that("iycf_recode_yesno() recodes character values correctly", {
  x <- c("yes", "no", "dnk", "yes", "no")

  out <- iycf_recode_yesno(x)

  expect_equal(out, c(1, 2, NA, 1, 2))
})

test_that("iycf_recode_yesno() handles custom yes/no/dnk values", {
  x <- c("y", "n", "unknown", "y", "n")

  out <- iycf_recode_yesno(x, yes_val = "y", no_val = "n", dnk_val = "unknown")

  expect_equal(out, c(1, 2, NA, 1, 2))
})

test_that("iycf_recode_yesno() returns numeric input as-is", {
  x <- c(1, 2, NA, 1, 2)

  out <- iycf_recode_yesno(x)

  expect_equal(out, x)
})

test_that("iycf_recode_yesno() recodes NA values to NA", {
  x <- c("yes", NA, "no", NA)

  out <- iycf_recode_yesno(x)

  expect_equal(out, c(1, NA, 2, NA))
})

test_that("iycf_recode_yesno() recodes unrecognized values to NA", {
  x <- c("yes", "no", "maybe", "unknown")

  out <- iycf_recode_yesno(x)

  expect_equal(out, c(1, 2, NA, NA))
})

test_that("iycf_recode_yesno() is case-sensitive", {
  x <- c("yes", "Yes", "YES", "no", "NO")

  out <- iycf_recode_yesno(x)

  expect_equal(out, c(1, NA, NA, 2, NA))
})

test_that("iycf_recode_yesno() handles empty vector", {
  x <- character(0)

  out <- iycf_recode_yesno(x)

  expect_equal(out, numeric(0))
})

test_that("iycf_recode_yesno() handles vector with only NAs", {
  x <- c(NA, NA, NA)

  out <- iycf_recode_yesno(x)

  expect_equal(out, c(NA_real_, NA_real_, NA_real_))
})
