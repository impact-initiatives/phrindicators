# Tests for add_standardized_age

test_that("add_standardized_age handles basic input with only age_years", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  ))
  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match age_years_col

})


test_that("add_standardized_age handles age_years with age_months_col", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    age_months = c(120, 240, 360)
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    age_months_col = "age_months"
  ))
  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match
  expect_equal(result$calc_age_months, df$age_months) # calc_age_months should match
  expect_true(all(abs(result$calc_age_days - df$age_months*30.44) < 1, na.rm = TRUE))
})


test_that("add_standardized_age computes calc_date_birth_final using exact birth first", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", NA)),
    date_birth_approx = as.Date(c(NA, "2003-06-01", "1993-01-01"))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    date_birth_approx_col = "date_birth_approx"
  ))
  expect_equal(result$calc_date_birth_final, as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")))
})


test_that("add_standardized_age computes calc_date_death_final using exact death first", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_death_exact = as.Date(c("2023-01-01", NA, "2023-01-01")),
    date_death_approx = as.Date(c(NA, "2023-06-01", "2023-12-01"))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  ))
  expect_equal(result$calc_date_death_final, as.Date(c("2023-01-01", "2023-06-01", "2023-01-01")))
})


test_that("add_standardized_age calculates age correctly from survey date", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")),
    survey_date = as.Date("2023-01-01")
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    survey_date_col = "survey_date"
  ))
  expect_equal(result$calc_age_years, c(10, 20, 30))
  expect_equal(result$calc_age_months, c(120, 240, 360))
  expect_equal(result$calc_age_days, c(3652, 7305, 10957))  # Approximate days in 10, 20, 30 years.
})


test_that("add_standardized_age does not calculate ages when no age_years values are provided", {
  df <- tibble::tibble(
    age_years = c(NA, NA, NA),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")),
    date_death_exact = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01"))
  )

  # result <- suppressMessages(add_standardized_age(
  #   .dataset = df,
  #   age_years_col = "age_years",
  #   date_birth_exact_col = "date_birth_exact",
  #   date_death_exact_col = "date_death_exact"
  # ))
  suppressMessages(expect_message(
    result <- add_standardized_age(
      .dataset = df,
      age_years_col = "age_years",
      date_birth_exact_col = "date_birth_exact",
      date_death_exact_col = "date_death_exact"
    )
  ))

})


test_that("add_standardized_age handles missing age_months_col gracefully", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  result <- suppressMessages(suppressWarnings(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  )))

  suppressWarnings(expect_equal(result$calc_age_years, df$age_years))  # calc_age_years should match age_years_col
  suppressWarnings(expect_true(all(is.na(result$calc_age_months))))    # calc_age_months should default to NA
  suppressWarnings(expect_true(all(is.na(result$calc_age_days))))      # calc_age_days should default to NA

})


test_that("add_standardized_age handles no date_of_birth or date_of_death inputs", {
  df <- tibble::tibble(
    age_years = c(10, 15, 20)
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  ))
  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match age_years_col
  # calc_date_birth_final and calc_date_death_final should not be created if no date columns provided
  expect_false("calc_date_birth_final" %in% names(result))
  expect_false("calc_date_death_final" %in% names(result))
})


test_that("add_standardized_age returns error for missing age_years_col", {
  df <- tibble::tibble(
    age_months = c(120, 180, 240)
  )

  expect_error(
    add_standardized_age(
      .dataset = df,
      age_months_col = "age_months"
    ),
    regexp = "add_standardized_age: argument \"age_years_col\" is missing, with no default",
    fixed = TRUE
  )
})


test_that("add_standardized_age gracefully handles empty dataset", {
  df <- tibble::tibble()

  expect_error(
    add_standardized_age(
      .dataset = df,
      age_years_col = "age_years"
    ),
    "Dataset is empty."
  )
})


test_that("add_standardized_age warns about overwriting columns", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    calc_age_years = c(1, 1, 1)
  )

  expect_warning(
    add_standardized_age(
      .dataset = df,
      age_years_col = "age_years"
    ),
    "Variable calc_age_years already exists and will be overwritten."
  )
})


test_that("add_standardized_age handles NULL date columns without error", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  # Test with NULL date_birth columns
  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = NULL,
    date_birth_approx_col = NULL,
    date_birth_final_col = NULL
  ))

  # Columns should not be created if no date columns are provided
  expect_false("calc_date_birth_final" %in% names(result))
  expect_equal(result$calc_age_years, df$age_years)
})


test_that("add_standardized_age handles non-existent date columns gracefully", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  # Test with columns that don't exist in the dataset
  # This should not error even though columns don't exist
  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "nonexistent_col",
    date_death_exact_col = "another_nonexistent_col"
  ))
  # The function should not create calc columns when columns don't exist
  expect_false("calc_date_birth_final" %in% names(result))
  expect_false("calc_date_death_final" %in% names(result))
  expect_equal(result$calc_age_years, df$age_years)
})


test_that("add_standardized_age uses only existing date columns in coalesce", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_approx = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01"))
  )

  # Pass both existing and non-existing columns
  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "nonexistent_exact",  # doesn't exist
    date_birth_approx_col = "date_birth_approx", # exists
    date_birth_final_col = "nonexistent_final"   # doesn't exist
  ))
  # Should use the only existing column (date_birth_approx)
  expect_equal(result$calc_date_birth_final, as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")))
})


test_that("add_standardized_age — calc_date_birth_final is NA when all birth date columns are NA", {
  # Regression test: death date columns exist and have values, but birth date columns are all NA.
  # calc_date_birth_final should remain NA for all records, NOT pick up values from death date columns.
  df <- tibble::tibble(
    age_years = c(5, 10, 30),
    dob_exact = as.Date(c(NA, NA, NA)),
    dob_approx = as.Date(c(NA, NA, NA)),
    date_death_exact = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    date_death_approx = as.Date(c(NA, "2023-02-15", NA))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "dob_exact",
    date_birth_approx_col = "dob_approx",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  ))
  # calc_date_birth_final should be NA for all rows — death dates must not bleed into birth dates
  expect_true("calc_date_birth_final" %in% names(result))
  expect_true(all(is.na(result$calc_date_birth_final)))
})


test_that("add_standardized_age — calc_date_death_final uses exact death date when approx is NA", {
  # Regression test: when date_death_exact_col and date_death_approx_col point to different columns,
  # calc_date_death_final should prefer the exact date and fall back to approx only when exact is NA.
  df <- tibble::tibble(
    age_years = c(5, 10, 30),
    date_death_exact = as.Date(c("2023-01-01", NA, "2023-03-01")),
    date_death_approx = as.Date(c(NA, "2023-02-15", "2023-03-20"))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  ))
  # Row 1: exact date available — use it
  expect_equal(result$calc_date_death_final[1], as.Date("2023-01-01"))
  # Row 2: only approx available — use approx
  expect_equal(result$calc_date_death_final[2], as.Date("2023-02-15"))
  # Row 3: both available — prefer exact
  expect_equal(result$calc_date_death_final[3], as.Date("2023-03-01"))
})


test_that("add_standardized_age creates calc_month_birth when calc_date_birth_final exists", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-05-15", "2003-08-22", NA)),
    date_birth_approx = as.Date(c(NA, NA, "1993-12-10"))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    date_birth_approx_col = "date_birth_approx"
  ))
  
  # Check that calc_month_birth column exists
  expect_true("calc_month_birth" %in% names(result))
  
  # Check that month-year values are formatted correctly
  expect_equal(result$calc_month_birth, c("2013-05-01", "2003-08-01", "1993-12-01"))
  
  # Check that values can be converted back to dates
  expect_equal(as.Date(result$calc_month_birth[1]), as.Date("2013-05-01"))
})


test_that("add_standardized_age creates calc_month_death when calc_date_death_final exists", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_death_exact = as.Date(c("2023-03-15", NA, "2023-11-22")),
    date_death_approx = as.Date(c(NA, "2023-07-10", NA))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  ))
  
  # Check that calc_month_death column exists
  expect_true("calc_month_death" %in% names(result))
  
  # Check that month-year values are formatted correctly
  expect_equal(result$calc_month_death, c("2023-03-01", "2023-07-01", "2023-11-01"))
  
  # Check that values can be converted back to dates
  expect_equal(as.Date(result$calc_month_death[1]), as.Date("2023-03-01"))
})


test_that("add_standardized_age handles NA values in calc_month_birth and calc_month_death", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-05-15", NA, "1993-12-10")),
    date_death_exact = as.Date(c(NA, "2023-07-10", NA))
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    date_death_exact_col = "date_death_exact"
  ))
  
  # Check that calc_month_birth has NA where birth date is NA
  expect_equal(result$calc_month_birth, c("2013-05-01", NA_character_, "1993-12-01"))
  
  # Check that calc_month_death has NA where death date is NA
  expect_equal(result$calc_month_death, c(NA_character_, "2023-07-01", NA_character_))
})


test_that("add_standardized_age does not create month columns when date_final columns don't exist", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  result <- suppressMessages(add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  ))
  
  # Check that month columns are not created when date_final columns don't exist
  expect_false("calc_month_birth" %in% names(result))
  expect_false("calc_month_death" %in% names(result))
})

