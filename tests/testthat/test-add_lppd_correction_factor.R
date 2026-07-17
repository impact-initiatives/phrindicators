# Tests for add_lppd_correction_factor

test_that("add_lppd_correction_factor() — valid dataset creates correction factor", {

  df <- tibble::tibble(
    num_days_collected = c(0, 3, 7)
  )

  out <- suppressMessages(add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days_collected"
  ))
  expect_equal(nrow(out), 3)
  expect_true("lppd_correction_factor" %in% names(out))
})


test_that("add_lppd_correction_factor() — calculation is correct", {

  df <- tibble::tibble(
    num_days = c(0, 7, 3, 5)
  )

  out <- suppressMessages(add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  ))
  # 0/7 = 0, 7/7 = 1, 3/7 = 0.429, 5/7 = 0.714
  expect_equal(out$lppd_correction_factor[1], 0)
  expect_equal(out$lppd_correction_factor[2], 1)
  expect_equal(round(out$lppd_correction_factor[3], 3), 0.429)
  expect_equal(round(out$lppd_correction_factor[4], 3), 0.714)
})


test_that("add_lppd_correction_factor() — values rounded to 3 decimal places", {

  df <- tibble::tibble(
    num_days = c(1, 2, 3, 4, 5, 6)
  )

  out <- suppressMessages(add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  ))
  # Check all values are rounded to 3 decimal places
  for (i in 1:6) {
    expect_equal(
      out$lppd_correction_factor[i],
      round(i / 7, 3)
    )
  }
})


test_that("add_lppd_correction_factor() — values outside 0-7 return NA", {

  df <- tibble::tibble(
    num_days = c(-1, 0, 3, 7, 8, 10)
  )

  out <- suppressMessages(add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  ))
  expect_true(is.na(out$lppd_correction_factor[1]))   # -1
  expect_false(is.na(out$lppd_correction_factor[2]))  # 0
  expect_false(is.na(out$lppd_correction_factor[3]))  # 3
  expect_false(is.na(out$lppd_correction_factor[4]))  # 7
  expect_true(is.na(out$lppd_correction_factor[5]))   # 8
  expect_true(is.na(out$lppd_correction_factor[6]))   # 10
})


test_that("add_lppd_correction_factor() — NA values remain NA", {

  df <- tibble::tibble(
    num_days = c(3, NA, 5)
  )

  out <- suppressMessages(add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  ))
  expect_false(is.na(out$lppd_correction_factor[1]))
  expect_true(is.na(out$lppd_correction_factor[2]))
  expect_false(is.na(out$lppd_correction_factor[3]))
})


test_that("add_lppd_correction_factor() — error on empty dataset", {

  df_empty <- tibble::tibble(
    num_days = numeric(0)
  )

  expect_error(
    add_lppd_correction_factor(
      .dataset = df_empty,
      num_days_collect_col = "num_days"
    )
  )
})


test_that("add_lppd_correction_factor() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(3, 5, 7)
  )

  expect_error(
    add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    )
  )
})


test_that("add_lppd_correction_factor() — warning when overwriting existing column", {

  df <- tibble::tibble(
    num_days = c(3, 5),
    lppd_correction_factor = c(99, 99)
  )

  suppressMessages(expect_warning(
    add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    ))
  )
})


test_that("add_lppd_correction_factor() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    num_days = c(3, "five", 7)
  )

  expect_error(
    suppressWarnings(add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    ))
  )
})


test_that("add_lppd_correction_factor() — boundary values at 0 and 7", {

  df <- tibble::tibble(
    num_days = c(0, 7)
  )

  suppressMessages(out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  ))

  expect_equal(out$lppd_correction_factor[1], 0)
  expect_equal(out$lppd_correction_factor[2], 1)
})
