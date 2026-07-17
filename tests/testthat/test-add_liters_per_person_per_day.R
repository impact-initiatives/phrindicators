# Tests for add_liters_per_person_per_day

test_that("add_liters_per_person_per_day() — valid dataset creates all columns", {

  df <- tibble::tibble(
    wash_total_liters = c(100, 200, 300),
    hh_size = c(4, 5, 6),
    wash_num_days_water_collection = c(1, 2, 3)
  )

  out <- suppressMessages(add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "wash_total_liters",
    household_size_col = "hh_size",
    num_days_col = "wash_num_days_water_collection"
  ))
  expect_equal(nrow(out), 3)
  expect_true("liters_pppd" %in% names(out))
  expect_true("liters_z_score" %in% names(out))
  expect_true("liters_pppd_z_score" %in% names(out))
  expect_true("liters_log" %in% names(out))
  expect_true("liters_pppd_log" %in% names(out))
  expect_true("wash_lppd_cat" %in% names(out))
})


test_that("add_liters_per_person_per_day() — calculation is correct", {

  df <- tibble::tibble(
    total_liters = c(100),
    hh_size = c(5),
    num_days = c(2)
  )

  out <- suppressMessages(add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  ))
  # 100 / (5 * 2) = 10 liters per person per day
  expect_equal(out$liters_pppd[1], 10)
})


test_that("add_liters_per_person_per_day() — categorization works correctly", {

  df <- tibble::tibble(
    total_liters = c(10, 30, 80, 150),
    hh_size = c(5, 5, 5, 5),
    num_days = c(1, 1, 1, 1)
  )

  out <- suppressMessages(add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  ))
  # Row 1: 10/5/1 = 2 LPPD (< 3)
  expect_true(grepl("Less than 3 LPPD", out$wash_lppd_cat[1]))
  # Row 2: 30/5/1 = 6 LPPD (3-7.5)
  expect_true(grepl("3-7.5 LPPD", out$wash_lppd_cat[2]))
  # Row 3: 80/5/1 = 16 LPPD (>= 15)
  expect_true(grepl("Greater than 15 LPPD", out$wash_lppd_cat[3]))
})


test_that("add_liters_per_person_per_day() — error on empty dataset", {

  df_empty <- tibble::tibble(
    total_liters = numeric(0),
    hh_size = numeric(0),
    num_days = numeric(0)
  )

  expect_error(
    add_liters_per_person_per_day(
      .dataset = df_empty,
      total_liters_col = "total_liters",
      household_size_col = "hh_size",
      num_days_col = "num_days"
    )
  )
})


test_that("add_liters_per_person_per_day() — error on missing columns", {

  df <- tibble::tibble(
    total_liters = c(100, 200)
  )

  expect_error(
    add_liters_per_person_per_day(
      .dataset = df,
      total_liters_col = "total_liters",
      household_size_col = "hh_size",
      num_days_col = "num_days"
    )
  )
})


test_that("add_liters_per_person_per_day() — handles NA values", {

  df <- tibble::tibble(
    total_liters = c(100, NA, 200),
    hh_size = c(5, 5, NA),
    num_days = c(2, 2, 2)
  )

  out <- add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  )

  expect_equal(nrow(out), 3)
  expect_false(is.na(out$liters_pppd[1]))
  expect_true(is.na(out$liters_pppd[2]))
  expect_true(is.na(out$liters_pppd[3]))
})
