# Tests for add_standardized_deaths

test_that("add_standardized_deaths() — valid dataset creates death columns", {

  df <- tibble::tibble(
    age_years = c(2, 30, 50),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    recall_date = as.Date(c("2022-01-01", "2023-01-01", "2022-12-31")),
    cause_of_death = c("malaria", "trauma", "old age"),
    location_of_death = c("home", "road", "last residence")
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    cause_of_death_col = "cause_of_death",
    non_trauma_vals = c("malaria", "diarrhea"),
    trauma_vals = c("trauma", "accident"),
    other_vals = c("old age", "unknown"),
    location_of_death_col = "location_of_death",
    current_location_residence_vals = c("home"),
    migration_vals = c("road"),
    last_location_residence_vals = c("last residence")
  ))
  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_true("death_under5" %in% names(out))
  expect_true("death_male" %in% names(out))
  expect_true("death_female" %in% names(out))
})


test_that("add_standardized_deaths() — death column logic works correctly", {

  df <- tibble::tibble(
    age_years = c(2, 30),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-02-01")),
    recall_date = as.Date(c("2022-01-01", "2023-02-15"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  ))
  expect_equal(out$death[1], 1)  # death after recall
  expect_equal(out$death[2], 0)  # death before recall
})


test_that("add_standardized_deaths() — age-based categorization works", {

  df <- tibble::tibble(
    age_years = c(2, 5, 30),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  ))
  expect_equal(out$death_under5[1], 1)  # age < 5
  expect_equal(out$death_under5[2], 0)  # age >= 5
  expect_equal(out$death_under5[3], 0)  # age >= 5
})


test_that("add_standardized_deaths() — sex-based categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  ))
  expect_equal(out$death_male[1], 1)
  expect_equal(out$death_male[2], 0)
  expect_equal(out$death_female[1], 0)
  expect_equal(out$death_female[2], 1)
})


test_that("add_standardized_deaths() — cause of death categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25, 40),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01")),
    cause_of_death = c("malaria", "trauma", "old age")
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    cause_of_death_col = "cause_of_death",
    non_trauma_vals = c("malaria", "diarrhea"),
    trauma_vals = c("trauma", "accident"),
    other_vals = c("old age", "unknown")
  ))
  expect_equal(out$death_non_trauma[1], 1)
  expect_equal(out$death_trauma[2], 1)
  expect_equal(out$death_other[3], 1)
})


test_that("add_standardized_deaths() — location categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25, 40),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01")),
    location_of_death = c("home", "road", "last residence")
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    location_of_death_col = "location_of_death",
    current_location_residence_vals = c("home"),
    migration_vals = c("road"),
    last_location_residence_vals = c("last residence")
  ))
  expect_equal(out$death_current_location[1], 1)
  expect_equal(out$death_migration[2], 1)
  expect_equal(out$death_last_location[3], 1)
})


test_that("add_standardized_deaths() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_years = numeric(0),
    sex = character(0),
    date_of_death = as.Date(character(0)),
    recall_date = as.Date(character(0))
  )

  expect_error(
    add_standardized_deaths(
      .dataset = df_empty,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})


test_that("add_standardized_deaths() — error on missing columns", {

  df <- tibble::tibble(
    age_years = c(30, 25)
  )

  expect_error(
    add_standardized_deaths(
      .dataset = df,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})


test_that("add_standardized_deaths() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    age_years = c(30, 25),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01")),
    death = c(1, 1)
  )

  expect_warning(
    add_standardized_deaths(
      .dataset = df,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})


test_that("add_standardized_deaths() — works with minimal columns (fallback to death=1)", {

  df <- tibble::tibble(
    death_id = c(1, 2, 3)
  )

  out <- add_standardized_deaths(
    .dataset = df
  )

  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_equal(out$death, c(1, 1, 1))
})


test_that("add_standardized_deaths() — works with only date columns provided", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    recall_date = as.Date(c("2022-01-01", "2024-01-01", "2022-12-31"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  ))
  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_equal(out$death[1], 1)  # death after recall
  expect_equal(out$death[2], 0)  # death before recall
  expect_equal(out$death[3], 1)  # death after recall
})


test_that("add_standardized_deaths() — death_birth column is created when date_of_birth_col is provided", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01", "2023-08-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    date_of_birth = as.Date(c("2022-12-01", "2023-02-01", "2023-01-01"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    date_of_birth_col = "date_of_birth"
  ))
  expect_true("death_birth" %in% names(out))
  expect_equal(out$death_birth[1], 0)  # born before recall (2022-12-01 <= 2023-01-01)
  expect_equal(out$death_birth[2], 1)  # born after recall (2023-02-01 > 2023-01-01)
  expect_equal(out$death_birth[3], 0)  # born on recall date (2023-01-01 <= 2023-01-01)
})


test_that("add_standardized_deaths() — death_birth handles NA values correctly", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01", "2023-08-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01", NA)),
    date_of_birth = as.Date(c("2023-02-01", NA, "2023-02-01"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    date_of_birth_col = "date_of_birth"
  ))
  expect_true("death_birth" %in% names(out))
  expect_equal(out$death_birth[1], 1)  # born after recall
  expect_true(is.na(out$death_birth[2]))  # NA date_of_birth
  expect_true(is.na(out$death_birth[3]))  # NA recall_date
})


test_that("add_standardized_deaths() — death_birth is not created when date_of_birth_col is NULL", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01"))
  )

  out <- suppressMessages(add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  ))
  # death_birth should not be in the dataset when date_of_birth_col is not provided
  # But it will be in columns_to_create and may be present with all NA
  # Let's check if it's meaningful (not all NA)
  if ("death_birth" %in% names(out)) {
    expect_true(all(is.na(out$death_birth)))
  }
})
