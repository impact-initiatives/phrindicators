# Tests for add_total_daily_liters

test_that("add_total_daily_liters() — valid dataset without correction factor", {

  df <- tibble::tibble(
    container_size = c(10, 5, 15),
    num_journeys = c(2, 3, 4)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "container_size",
    wash_container_num_journeys_col = "num_journeys"
  ))
  expect_equal(nrow(out), 3)
  expect_true("wash_container_total_litres" %in% names(out))
})


test_that("add_total_daily_liters() — calculation without correction factor is correct", {

  df <- tibble::tibble(
    size = c(10, 20, 5),
    journeys = c(2, 3, 4)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  ))
  # 10*2=20, 20*3=60, 5*4=20
  expect_equal(out$wash_container_total_litres[1], 20)
  expect_equal(out$wash_container_total_litres[2], 60)
  expect_equal(out$wash_container_total_litres[3], 20)
})


test_that("add_total_daily_liters() — valid dataset with correction factor", {

  df <- tibble::tibble(
    container_size = c(10, 5, 15),
    num_journeys = c(2, 3, 4),
    correction = c(1, 0.8, 0.5)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "container_size",
    wash_container_num_journeys_col = "num_journeys",
    correction_factor_col = "correction"
  ))
  expect_equal(nrow(out), 3)
  expect_true("wash_container_total_litres" %in% names(out))
})


test_that("add_total_daily_liters() — calculation with correction factor is correct", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(1, 0.5)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  ))
  # (10*2)*1=20, (20*3)*0.5=30
  expect_equal(out$wash_container_total_litres[1], 20)
  expect_equal(out$wash_container_total_litres[2], 30)
})


test_that("add_total_daily_liters() — NA in size or journeys returns NA", {

  df <- tibble::tibble(
    size = c(10, NA, 15),
    journeys = c(2, 3, NA)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  ))
  expect_false(is.na(out$wash_container_total_litres[1]))
  expect_true(is.na(out$wash_container_total_litres[2]))
  expect_true(is.na(out$wash_container_total_litres[3]))
})


test_that("add_total_daily_liters() — NA in correction factor preserves base calculation", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(0.5, NA)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  ))
  # (10*2)*0.5=10, (20*3)*NA=60 (no correction applied)
  expect_equal(out$wash_container_total_litres[1], 10)
  expect_equal(out$wash_container_total_litres[2], 60)
})


test_that("add_total_daily_liters() — error on empty dataset", {

  df_empty <- tibble::tibble(
    size = numeric(0),
    journeys = numeric(0)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df_empty,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})


test_that("add_total_daily_liters() — error on missing required columns", {

  df <- tibble::tibble(
    size = c(10, 20)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})


test_that("add_total_daily_liters() — error when correction factor column missing", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys",
      correction_factor_col = "correction"
    )
  )
})


test_that("add_total_daily_liters() — warning when overwriting existing column", {

  df <- tibble::tibble(
    size = c(10),
    journeys = c(2),
    wash_container_total_litres = 99
  )

  expect_warning(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})


test_that("add_total_daily_liters() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    size = c(10, "twenty", 30),
    journeys = c(2, 3, 4)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})


test_that("add_total_daily_liters() — zero values handled correctly", {

  df <- tibble::tibble(
    size = c(10, 0, 20),
    journeys = c(0, 5, 2)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  )

  # 10*0=0, 0*5=0, 20*2=40
  expect_equal(out$wash_container_total_litres[1], 0)
  expect_equal(out$wash_container_total_litres[2], 0)
  expect_equal(out$wash_container_total_litres[3], 40)
})


test_that("add_total_daily_liters() — correction factor of 0 works correctly", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(0, 1)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  ))
  # (10*2)*0=0, (20*3)*1=60
  expect_equal(out$wash_container_total_litres[1], 0)
  expect_equal(out$wash_container_total_litres[2], 60)
})


test_that("add_total_daily_liters() — large values handled correctly", {

  df <- tibble::tibble(
    size = c(100, 500),
    journeys = c(10, 20),
    correction = c(1.5, 2)
  )

  out <- suppressMessages(add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  ))
  # (100*10)*1.5=1500, (500*20)*2=20000
  expect_equal(out$wash_container_total_litres[1], 1500)
  expect_equal(out$wash_container_total_litres[2], 20000)
})
