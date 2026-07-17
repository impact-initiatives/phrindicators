# Tests for add_drinking_water_time_cat

test_that("add_drinking_water_time_cat() — numeric minutes categorization works", {

  df <- tibble::tibble(
    number_minutes = c(15, 30, 45, 60),
    categorical_time = c(NA, NA, NA, NA)
  )

  out <- suppressMessages(add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time"
  ))
  expect_equal(nrow(out), 4)
  expect_true("wash_drinking_water_time_cat" %in% names(out))
  expect_equal(out$wash_drinking_water_time_cat[1], 1)  # <= 30
  expect_equal(out$wash_drinking_water_time_cat[2], 1)  # <= 30
  expect_equal(out$wash_drinking_water_time_cat[3], 0)  # > 30
  expect_equal(out$wash_drinking_water_time_cat[4], 0)  # > 30
})


test_that("add_drinking_water_time_cat() — categorical time works", {

  df <- tibble::tibble(
    number_minutes = c(NA, NA),
    categorical_time = c("under_30min", "more_than_30min")
  )

  out <- suppressMessages(add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min"
  ))
  expect_equal(out$wash_drinking_water_time_cat[1], 1)
  expect_equal(out$wash_drinking_water_time_cat[2], 0)
})


test_that("add_drinking_water_time_cat() — numeric takes priority over categorical", {

  df <- tibble::tibble(
    number_minutes = c(20),
    categorical_time = c("more_than_30min")  # Conflicting but should be ignored
  )

  out <- suppressMessages(add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min"
  ))
  # Should use numeric (20 minutes) not categorical
  expect_equal(out$wash_drinking_water_time_cat[1], 1)
})


test_that("add_drinking_water_time_cat() — undefined values return NA", {

  df <- tibble::tibble(
    number_minutes = c(NA, NA),
    categorical_time = c("dnk", "pnta")
  )

  out <- suppressMessages(add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min",
    undefined = c("dnk", "pnta")
  ))
  expect_true(is.na(out$wash_drinking_water_time_cat[1]))
  expect_true(is.na(out$wash_drinking_water_time_cat[2]))
})


test_that("add_drinking_water_time_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    number_minutes = numeric(0),
    categorical_time = character(0)
  )

  expect_error(
    add_drinking_water_time_cat(
      .dataset = df_empty,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})


test_that("add_drinking_water_time_cat() — error when both columns missing", {

  df <- tibble::tibble(
    other_col = c(1, 2, 3)
  )

  expect_error(
    add_drinking_water_time_cat(
      .dataset = df,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})


test_that("add_drinking_water_time_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    number_minutes = c(20),
    categorical_time = c("under_30min"),
    wash_drinking_water_time_cat = 99
  )

  expect_warning(
    add_drinking_water_time_cat(
      .dataset = df,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})
