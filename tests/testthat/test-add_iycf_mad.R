# Tests for add_iycf_mad

test_that("add_iycf_mad() combines MDD, MMF, and MMFF correctly", {
  df <- make_iycf_data(
    age_months = c(6, 6, 12, 12, 24),
    iycf_4 = c("yes", "yes", "no", "no", "yes"),
    iycf_mdd_cat = c(1, 1, 1, 1, 1),
    iycf_mmf = c(1, 0, 1, 1, 1),
    iycf_mmff = c(0, 1, 1, 0, 1)
  )

  out <- suppressMessages(add_iycf_mad(df))

  expect_equal(out$iycf_mad, c(1, 0, 1, 0, NA))
})

test_that("add_iycf_mad() returns NA when prerequisite indicators are missing", {
  df <- make_iycf_data(age_months = 12, iycf_4 = "no", iycf_mdd_cat = 1, iycf_mmf = 1, iycf_mmff = NA)

  out <- suppressMessages(add_iycf_mad(df))

  expect_true(is.na(out$iycf_mad))
})

test_that("add_iycf_mad() errors on empty or incomplete datasets", {
  expect_error(add_iycf_mad(make_iycf_data(n = 0)))
  expect_error(add_iycf_mad(tibble::tibble(age_months = 12, iycf_4 = "yes")))
})
