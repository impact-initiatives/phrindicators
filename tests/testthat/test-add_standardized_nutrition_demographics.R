# Tests for add_standardized_nutrition_demographics

test_that("add_standardized_nutrition_demographics creates canonical columns", {

  df <- tibble::tibble(
    child_id = 1:6,
    calc_age_years = c(0.8, 1.5, 2.5, 4, 6, 1.2)
  )

  result <- add_standardized_nutrition_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )

  # Check all canonical columns exist
  expect_true("nutrition_child_under2" %in% names(result))
  expect_true("nutrition_child_2to5" %in% names(result))
  expect_true("nutrition_child_under5" %in% names(result))

  # Check values
  # <2 years: 0.8, 1.5, 1.2 (indices 1, 2, 6)
  expect_equal(result$nutrition_child_under2, c(1, 1, 0, 0, 0, 1))
  # 2-5 years: 2.5, 4 (indices 3, 4)
  expect_equal(result$nutrition_child_2to5, c(0, 0, 1, 1, 0, 0))
  # <5 years: all except 6 (index 5)
  expect_equal(result$nutrition_child_under5, c(1, 1, 1, 1, 0, 1))
})


test_that("add_standardized_nutrition_demographics handles NA values", {

  df <- tibble::tibble(
    child_id = 1:4,
    calc_age_years = c(0.8, NA, 4, 6)
  )

  result <- add_standardized_nutrition_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )

  # NA age should result in 0 for all indicators
  expect_equal(result$nutrition_child_under2, c(1, 0, 0, 0))
  expect_equal(result$nutrition_child_2to5, c(0, 0, 1, 0))
  expect_equal(result$nutrition_child_under5, c(1, 0, 1, 0))
})


test_that("add_standardized_nutrition_demographics error on missing age column", {

  df <- tibble::tibble(
    child_id = 1:3
  )

  expect_error(
    add_standardized_nutrition_demographics(
      .dataset = df,
      age_years_col = "calc_age_years"
    ),
    "calc_age_years"
  )
})


test_that("add_standardized_nutrition_demographics boundary values", {

  df <- tibble::tibble(
    child_id = 1:5,
    calc_age_years = c(0, 1.9, 2, 4.9, 5)
  )

  result <- add_standardized_nutrition_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )

  # 0, 1.9 should be under 2 (<2)
  expect_equal(result$nutrition_child_under2, c(1, 1, 0, 0, 0))
  # 2, 4.9 should be 2to5 (2-4.9)
  expect_equal(result$nutrition_child_2to5, c(0, 0, 1, 1, 0))
  # 0, 1.9, 2, 4.9 should be under 5 (<5)
  expect_equal(result$nutrition_child_under5, c(1, 1, 1, 1, 0))
})
