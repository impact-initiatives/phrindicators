# Tests for add_age_months_cat

test_that("add_age_months_cat() — valid dataset creates age month categories", {

  df <- tibble::tibble(
    age_months = c(0, 5, 7, 12, 23, 40, 59)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(nrow(out), 7)
  expect_true("age_months_cat" %in% names(out))
  expect_s3_class(out$age_months_cat, "factor")

  # Check specific categories
  expect_equal(as.character(out$age_months_cat[1]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[3]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[4]), "12-17 months")

  # Check roster_age_6_29m and roster_age_30_59m columns exist
  expect_true("roster_age_6_29m" %in% names(out))
  expect_true("roster_age_30_59m" %in% names(out))
})


test_that("add_age_months_cat() — roster_age_6_29m and roster_age_30_59m values are correct", {

  df <- tibble::tibble(
    age_months = c(0, 5, 6, 29, 30, 59, 60)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  # Under 6 months → NA for both columns
  expect_true(is.na(out$roster_age_6_29m[1]))
  expect_true(is.na(out$roster_age_6_29m[2]))
  expect_true(is.na(out$roster_age_30_59m[1]))
  expect_true(is.na(out$roster_age_30_59m[2]))

  # 6-29 months → roster_age_6_29m = 1, roster_age_30_59m = 0
  expect_equal(out$roster_age_6_29m[3], 1)
  expect_equal(out$roster_age_6_29m[4], 1)
  expect_equal(out$roster_age_30_59m[3], 0)
  expect_equal(out$roster_age_30_59m[4], 0)

  # 30-59 months → roster_age_6_29m = 0, roster_age_30_59m = 1
  expect_equal(out$roster_age_6_29m[5], 0)
  expect_equal(out$roster_age_6_29m[6], 0)
  expect_equal(out$roster_age_30_59m[5], 1)
  expect_equal(out$roster_age_30_59m[6], 1)

  # 60+ months → NA for both columns
  expect_true(is.na(out$roster_age_6_29m[7]))
  expect_true(is.na(out$roster_age_30_59m[7]))
})


test_that("add_age_months_cat() — NA values are handled correctly", {

  df <- tibble::tibble(
    age_months = c(5, 10, NA, 25)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(nrow(out), 4)
  expect_true(is.na(out$age_months_cat[3]))
  expect_false(is.na(out$age_months_cat[1]))
})


test_that("add_age_months_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_months = numeric(0)
  )

  expect_error(
    add_age_months_cat(
      .dataset = df_empty,
      age_months_col = "age_months"
    )
  )
})


test_that("add_age_months_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(5, 10, 15)
  )

  expect_error(
    add_age_months_cat(
      .dataset = df,
      age_months_col = "age_months"
    )
  )
})


test_that("add_age_months_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    age_months = c(5, 10, 15),
    age_months_cat = c("old", "old", "old")
  )

  expect_warning(
    add_age_months_cat(
      .dataset = df,
      age_months_col = "age_months"
    )
  )
})


test_that("add_age_months_cat() — boundary values are categorized correctly", {

  df <- tibble::tibble(
    age_months = c(0, 5, 6, 11, 12, 17, 54, 59)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(as.character(out$age_months_cat[1]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[2]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[3]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[4]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[5]), "12-17 months")
  expect_equal(as.character(out$age_months_cat[7]), "54-59 months")
  expect_equal(as.character(out$age_months_cat[8]), "54-59 months")
})


test_that("add_age_months_cat() — values outside range return NA", {

  df <- tibble::tibble(
    age_months = c(60, 70, 100)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_true(all(is.na(out$age_months_cat)))
})
