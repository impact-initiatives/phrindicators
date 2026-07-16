# Tests for add_persontime

test_that("add_persontime() — valid dataset creates person-time columns", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-11-30")),
    dob = as.Date(c("2022-06-01", "2021-01-01")),
    sex = c("Male", "Female"),
    age_years = c(1, 2)
  )

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    dob_col = "dob",
    sex_col = "sex",
    age_years_col = "age_years",
    male_val = "Male",
    female_val = "Female"
  ))
  expect_equal(nrow(out), 2)
  expect_true("person_time" %in% names(out))
  expect_true("entry_date" %in% names(out))
  expect_true("exit_date" %in% names(out))
  expect_true("flag_negative_persontime" %in% names(out))
})


test_that("add_persontime() — person-time calculation is correct", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")))

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date"
  ))
  expected_days <- as.numeric(as.Date("2023-12-31") - as.Date("2023-01-01"))
  expect_equal(out$person_time[1], expected_days)
})


test_that("add_persontime() — entry date uses most recent date", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")),
    dob = as.Date(c("2023-02-01")),
    date_joined = as.Date(c("2023-03-01"))
  )

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    dob_col = "dob",
    date_joined_col = "date_joined"
  ))
  # Entry date should be the most recent: date_joined (2023-03-01)
  expect_equal(out$entry_date[1], as.Date("2023-03-01"))
})


test_that("add_persontime() — exit date uses earliest date", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")),
    date_of_death = as.Date(c("2023-06-01")),
    date_left = as.Date(c("2023-07-01"))
  )

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    date_of_death_col = "date_of_death",
    date_left_col = "date_left"
  ))
  # Exit date should be the earliest: date_of_death (2023-06-01)
  expect_equal(out$exit_date[1], as.Date("2023-06-01"))
})


test_that("add_persontime() — negative person-time is set to zero", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-12-31")),
    survey_date = as.Date(c("2023-01-01"))
  )

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date"
  ))
  expect_equal(out$person_time[1], 0)
  expect_equal(out$flag_negative_persontime[1], 0)
})


test_that("add_persontime() — age and sex columns create stratified person-time", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31")),
    age_years = c(3, 10),
    sex = c("Male", "Female")
  )

  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "Male",
    female_val = "Female"
  ))
  expect_true("person_time_under5" %in% names(out))
  expect_true("person_time_male" %in% names(out))
  expect_true("person_time_female" %in% names(out))
  expect_gt(out$person_time_under5[1], 0)
  expect_equal(out$person_time_under5[2], 0)
})


test_that("add_persontime() — error on empty dataset", {

  df_empty <- tibble::tibble(
    recall_date = as.Date(character(0)),
    survey_date = as.Date(character(0))
  )

  expect_error(
    add_persontime(
      df_empty,
      recall_date_col = "recall_date",
      survey_date_col = "survey_date"
    )
  )
})


test_that("add_persontime() — error on missing columns", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01"))
  )

  expect_error(
    add_persontime(
      df,
      recall_date_col = "recall_date",
      survey_date_col = "survey_date"
    )
  )
})


test_that("add_persontime handles NULL optional columns without error", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31"))
  )

  # Test with NULL optional columns
  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = NULL,
    sex_col = NULL
  )

  # Should not create person_time_under5, person_time_male, person_time_female
  expect_false("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
  expect_true("person_time" %in% names(out))
})


test_that("add_persontime handles non-existent optional columns gracefully", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31"))
  )

  # Test with columns that don't exist in the dataset
  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "nonexistent_age",
    sex_col = "nonexistent_sex"
  ))
  # Should not create optional columns when columns don't exist
  expect_false("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
  expect_true("person_time" %in% names(out))
})


test_that("add_persontime creates optional columns only when columns exist", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31")),
    age_years = c(3, 10)
  )

  # Test with age_years existing but sex not existing
  out <- suppressMessages(add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "age_years",
    sex_col = "nonexistent_sex"
  ))
  # Should create person_time_under5 but not sex-based columns
  expect_true("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
})
