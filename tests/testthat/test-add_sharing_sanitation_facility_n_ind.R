# Tests for add_sharing_sanitation_facility_n_ind

test_that("add_sharing_sanitation_facility_n_ind() — people estimate and buckets are correct", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "shared", "shared", "not_shared", "not_applicable", "undefined"),
    wash_sanitation_facility_sharing_n   = c(10, 5, 2, NA, NA, NA),
    hh_size                             = rep(5, 6),
    weight                              = rep(1, 6)
  )

  out <- suppressMessages(add_sharing_sanitation_facility_n_ind(df))

  # mean_hh_size == 5, so shared -> (n - 1) * 5 + 5
  expect_equal(out$wash_sharing_sanitation_facility_n, c(50, 25, 10, 5, NA, NA))
  expect_equal(
    as.character(out$wash_sharing_sanitation_facility_n_ind),
    c("50_and_above", "20_to_49", "19_and_below", "19_and_below", NA, NA)
  )
})


test_that("add_sharing_sanitation_facility_n_ind() — indicator is an ordered factor", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n   = c(3, NA),
    hh_size                             = c(4, 4),
    weight                              = c(1, 1)
  )

  out <- suppressMessages(add_sharing_sanitation_facility_n_ind(df))

  expect_true(is.ordered(out$wash_sharing_sanitation_facility_n_ind))
  expect_equal(
    levels(out$wash_sharing_sanitation_facility_n_ind),
    c("19_and_below", "20_to_49", "50_and_above")
  )
})


test_that("add_sharing_sanitation_facility_n_ind() — mean household size is weighted", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n   = c(6, NA),
    hh_size                             = c(10, 2),
    weight                              = c(1, 3)
  )

  out <- suppressMessages(add_sharing_sanitation_facility_n_ind(df))

  # weighted mean hh size = (10*1 + 2*3) / 4 = 4  ->  (6 - 1) * 4 + 10 = 30
  # (an unweighted mean of 6 would give 40)
  expect_equal(out$wash_sharing_sanitation_facility_n[1], 30)
})


test_that("add_sharing_sanitation_facility_n_ind() — unweighted mean when weight is NULL", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n   = c(6, NA),
    hh_size                             = c(10, 2)
  )

  out <- suppressMessages(add_sharing_sanitation_facility_n_ind(df, weight = NULL))

  # unweighted mean hh size = 6  ->  (6 - 1) * 6 + 10 = 40
  expect_equal(out$wash_sharing_sanitation_facility_n[1], 40)
})


test_that("add_sharing_sanitation_facility_n_ind() — warns and falls back when weight column is absent", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n   = c(6, NA),
    hh_size                             = c(10, 2)
  )

  expect_warning(suppressMessages(add_sharing_sanitation_facility_n_ind(df)))
})


test_that("add_sharing_sanitation_facility_n_ind() — error on empty dataset", {

  df_empty <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = character(0),
    wash_sanitation_facility_sharing_n   = numeric(0),
    hh_size                             = numeric(0)
  )

  expect_error(suppressMessages(add_sharing_sanitation_facility_n_ind(df_empty, weight = NULL)))
})


test_that("add_sharing_sanitation_facility_n_ind() — error when required columns missing", {

  df <- tibble::tibble(other_col = c(1, 2, 3))

  expect_error(suppressMessages(add_sharing_sanitation_facility_n_ind(df, weight = NULL)))
})


test_that("add_sharing_sanitation_facility_n_ind() — error when household size is non-positive", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n   = c(3, NA),
    hh_size                             = c(0, 4)
  )

  expect_error(suppressMessages(add_sharing_sanitation_facility_n_ind(df, weight = NULL)))
})


test_that("add_sharing_sanitation_facility_n_ind() — warning when overwriting existing column", {

  df <- tibble::tibble(
    wash_sharing_sanitation_facility_cat  = c("shared", "not_shared"),
    wash_sanitation_facility_sharing_n    = c(3, NA),
    hh_size                              = c(4, 4),
    wash_sharing_sanitation_facility_n_ind = c("old", "old")
  )

  expect_warning(suppressMessages(add_sharing_sanitation_facility_n_ind(df, weight = NULL)))
})
