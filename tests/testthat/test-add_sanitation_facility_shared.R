# Tests for add_sanitation_facility_shared

test_that("add_sanitation_facility_shared() — numeric input works correctly", {

  df <- tibble::tibble(
    num_households = c(1, 2, 3, 5)
  )

  out <- suppressMessages(add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2
  ))
  expect_equal(nrow(out), 4)
  expect_true("wash_sanitation_facility_shared_cat" %in% names(out))
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # < 2
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # >= 2
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "yes")  # >= 2
  expect_equal(out$wash_sanitation_facility_shared_cat[4], "yes")  # >= 2
})


test_that("add_sanitation_facility_shared() — categorical input works correctly", {

  df <- tibble::tibble(
    shared_response = c("shared", "not_shared", "shared")
  )

  out <- suppressMessages(add_sanitation_facility_shared(
    .dataset = df,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  ))
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "yes")
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "no")
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "yes")
})


test_that("add_sanitation_facility_shared() — numeric takes priority over categorical", {

  df <- tibble::tibble(
    num_households = c(1, 3),
    shared_response = c("shared", "not_shared")
  )

  out <- suppressMessages(add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  ))
  # Should use numeric values
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # num=1
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # num=3
})


test_that("add_sanitation_facility_shared() — fallback to categorical when numeric is NA", {

  df <- tibble::tibble(
    num_households = c(1, NA, NA),
    shared_response = c(NA, "shared", "not_shared")
  )

  out <- suppressMessages(add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  ))
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # numeric
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # categorical fallback
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "no")   # categorical fallback
})


test_that("add_sanitation_facility_shared() — error on empty dataset", {

  df_empty <- tibble::tibble(
    num_households = numeric(0)
  )

  expect_error(
    add_sanitation_facility_shared(
      .dataset = df_empty,
      num_households_col = "num_households"
    )
  )
})


test_that("add_sanitation_facility_shared() — error when no input provided", {

  df <- tibble::tibble(
    some_col = c(1, 2, 3)
  )

  expect_error(
    add_sanitation_facility_shared(
      .dataset = df
    )
  )
})


test_that("add_sanitation_facility_shared() — warning when overwriting existing column", {

  df <- tibble::tibble(
    num_households = c(1, 2),
    wash_sanitation_facility_shared_cat = c("old", "old")
  )

  expect_warning(
    add_sanitation_facility_shared(
      .dataset = df,
      num_households_col = "num_households"
    )
  )
})
