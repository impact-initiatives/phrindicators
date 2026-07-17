# Tests for add_healthcare_access_one_hour

test_that("add_healthcare_access_one_hour() — numeric minutes works correctly", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes", "num_minutes", "num_minutes"),
    travel_minutes = c(45, 65, 30),
    travel_range = c(NA, NA, NA)
  )

  out <- suppressMessages(add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  ))
  expect_equal(nrow(out), 3)
  expect_true("health_healthcare_access_one_hour" %in% names(out))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[3]))
})


test_that("add_healthcare_access_one_hour() — range-based classification works", {

  df <- tibble::tibble(
    travel_time_type = c("range", "range", "range"),
    travel_minutes = c(NA, NA, NA),
    travel_range = c("<1_hour", ">=1_hour", "<1_hour")
  )

  out <- suppressMessages(add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  ))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[3]))
})


test_that("add_healthcare_access_one_hour() — numeric takes priority over range", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(45),
    travel_range = c(">=1_hour")  # conflicting but should be ignored
  )

  out <- suppressMessages(add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  ))
  # Should use numeric (45 minutes) not range (>=1_hour)
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
})


test_that("add_healthcare_access_one_hour() — missing data returns dont_know", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(NA),
    travel_range = c(NA)
  )

  out <- suppressMessages(suppressWarnings(add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )))
  expect_true(grepl("dont_know", out$health_healthcare_access_one_hour[1]))
})


test_that("add_healthcare_access_one_hour() — error on empty dataset", {

  df_empty <- tibble::tibble(
    travel_time_type = character(0),
    travel_minutes = numeric(0),
    travel_range = character(0)
  )

  expect_error(
    add_healthcare_access_one_hour(
      .dataset = df_empty,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  )
})


test_that("add_healthcare_access_one_hour() — error on missing columns", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes")
  )

  expect_error(
    add_healthcare_access_one_hour(
      .dataset = df,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  )
})


test_that("add_healthcare_access_one_hour() — warning when overwriting existing column", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(45),
    travel_range = c(NA),
    health_healthcare_access_one_hour = "old"
  )

  suppressWarnings(expect_warning(
    add_healthcare_access_one_hour(
      .dataset = df,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  ))
})


test_that("add_healthcare_access_one_hour() — boundary value at 60 minutes", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes", "num_minutes"),
    travel_minutes = c(59, 60),
    travel_range = c(NA, NA)
  )

  out <- suppressMessages(add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  ))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))  # < 60
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))   # >= 60
})

