# Tests for add_standardized_roster_demographics

test_that("add_standardized_roster_demographics creates canonical columns", {

  df <- tibble::tibble(
    person_id = 1:6,
    calc_age_years = c(1, 4, 8, 25, 40, 18),
    sex = c("M", "F", "M", "F", "F", "F")
  )

  result <- suppressMessages(add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F"
  ))
  # Check all canonical columns exist
  expect_true("roster_child_under2" %in% names(result))
  expect_true("roster_child_under5" %in% names(result))
  expect_true("roster_2to5" %in% names(result))
  expect_true("roster_5plus" %in% names(result))
  expect_true("roster_5_10" %in% names(result))
  expect_true("roster_male" %in% names(result))
  expect_true("roster_female" %in% names(result))
  expect_true("roster_woman_15to49" %in% names(result))

  # Check values
  expect_equal(result$roster_child_under2, c(1, 0, 0, 0, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 1, 0, 0, 0, 0))
  expect_equal(result$roster_2to5, c(0, 1, 0, 0, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1, 1, 1, 1))
  expect_equal(result$roster_5_10, c(0, 0, 1, NA, NA, NA))
  expect_equal(result$roster_male, c(1, 0, 1, 0, 0, 0))
  expect_equal(result$roster_female, c(0, 1, 0, 1, 1, 1))
  expect_equal(result$roster_woman_15to49, c(0, 0, 0, 1, 1, 1))
})


test_that("add_standardized_roster_demographics handles missing sex column", {

  df <- tibble::tibble(
    person_id = 1:3,
    calc_age_years = c(1, 4, 8)
  )

  expect_warning(
    result <- add_standardized_roster_demographics(
      .dataset = df,
      age_years_col = "calc_age_years"
    ),
    "Sex column not provided"
  )

  # Sex-based columns should be 0
  expect_equal(result$roster_male, c(0, 0, 0))
  expect_equal(result$roster_female, c(0, 0, 0))
  expect_equal(result$roster_woman_15to49, c(0, 0, 0))

  # Age-based columns should still work
  expect_equal(result$roster_child_under2, c(1, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 1, 0))
  expect_equal(result$roster_2to5, c(0, 1, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1))
  expect_equal(result$roster_5_10, c(0, 0, 1))
})


test_that("add_standardized_roster_demographics handles NA values", {

  df <- tibble::tibble(
    person_id = 1:4,
    calc_age_years = c(1, NA, 8, 25),
    sex = c("M", "F", NA, "F")
  )

  result <- suppressMessages(add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F"
  ))
  # NA age should result in 0 for age-based indicators
  expect_equal(result$roster_child_under2, c(1, 0, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 0, 0, 0))
  expect_equal(result$roster_2to5, c(0, 0, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1, 1))
  # NA age → NA for roster_5_10; age >= 10 → NA
  expect_equal(result$roster_5_10, c(0, NA, 1, NA))

  # NA sex should result in 0 for sex-based indicators
  expect_equal(result$roster_male, c(1, 0, 0, 0))
  expect_equal(result$roster_female, c(0, 1, 0, 1))
})


test_that("add_standardized_roster_demographics roster_2to5 boundary values", {

  df <- tibble::tibble(
    person_id = 1:5,
    calc_age_years = c(1.9, 2, 4.9, 5, 6)
  )

  suppressWarnings(result <- suppressMessages(add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )))
  expect_equal(result$roster_2to5, c(0, 1, 1, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 0, 1, 1))
})


test_that("add_standardized_roster_demographics roster_5_10 boundary values", {

  df <- tibble::tibble(
    person_id = 1:6,
    calc_age_years = c(4.9, 5, 7, 9.9, 10, 25)
  )

  result <- suppressMessages(suppressWarnings(add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )))
  expect_equal(result$roster_5_10, c(0, 1, 1, 1, NA, NA))
})


test_that("add_standardized_roster_demographics error on missing age column", {

  df <- tibble::tibble(
    person_id = 1:3,
    sex = c("M", "F", "M")
  )

  expect_error(
    add_standardized_roster_demographics(
      .dataset = df,
      age_years_col = "calc_age_years",
      sex_col = "sex"
    ),
    "calc_age_years"
  )
})
