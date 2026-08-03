# Tests for add_drinking_water_time_cat

test_that("add_drinking_water_time_cat() — both output columns are added", {

  df <- tibble::tibble(
    wash_drinking_water_source     = "borehole",
    wash_drinking_water_time_yn    = "number_minutes",
    wash_drinking_water_time_int   = 15,
    wash_drinking_water_time_sl    = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  expect_equal(nrow(out), 1)
  expect_true("wash_drinking_water_time_cat" %in% names(out))
  expect_true("wash_drinking_water_time" %in% names(out))
})


test_that("add_drinking_water_time_cat() — numeric minutes are bucketed correctly", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = c(15, 30, 59, 60, 600, 601),
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  # < 30 min  → first element of sl_under_30_min default
  expect_equal(out$wash_drinking_water_time_cat[1], "5min_or_less")
  # 30–59 min → sl_30min_1hr default
  expect_equal(out$wash_drinking_water_time_cat[2], "30min_1hr")
  expect_equal(out$wash_drinking_water_time_cat[3], "30min_1hr")
  # 60–600 min → sl_more_than_1hr default
  expect_equal(out$wash_drinking_water_time_cat[4], "more_than_1hr")
  expect_equal(out$wash_drinking_water_time_cat[5], "more_than_1hr")
  # > max (600) → NA
  expect_true(is.na(out$wash_drinking_water_time_cat[6]))
})


test_that("add_drinking_water_time_cat() — water_on_premises in time_yn yields 'premises'", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = c("water_in_dwelling", "water_in_plot"),
    wash_drinking_water_time_int = NA_real_,
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  expect_equal(out$wash_drinking_water_time_cat[1], "premises")
  expect_equal(out$wash_drinking_water_time_cat[2], "premises")
})


test_that("add_drinking_water_time_cat() — drinking_water_source short-circuits to 'premises'", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "piped_dwelling",
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = 60,
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  expect_equal(out$wash_drinking_water_time_cat[1], "premises")
  expect_equal(out$wash_drinking_water_time[1], "1")
})


test_that("add_drinking_water_time_cat() — drinking_water_source short-circuits to 'undefined'", {

  df <- tibble::tibble(
    wash_drinking_water_source   = c("dnk", "pnta"),
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = 15,
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  expect_equal(out$wash_drinking_water_time_cat[1], "undefined")
  expect_equal(out$wash_drinking_water_time_cat[2], "undefined")
  expect_true(is.na(out$wash_drinking_water_time[1]))
  expect_true(is.na(out$wash_drinking_water_time[2]))
})


test_that("add_drinking_water_time_cat() — source priority overrides time_yn", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "piped_dwelling",
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = 60,
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  # Source = piped_dwelling → "premises" regardless of the time values
  expect_equal(out$wash_drinking_water_time_cat[1], "premises")
})


test_that("add_drinking_water_time_cat() — pnta in time_yn yields 'undefined'", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "pnta",
    wash_drinking_water_time_int = NA_real_,
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  expect_equal(out$wash_drinking_water_time_cat[1], "undefined")
  expect_true(is.na(out$wash_drinking_water_time[1]))
})


test_that("add_drinking_water_time_cat() — dnk in time_yn resolved via skip-logic column", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "dnk",
    wash_drinking_water_time_int = NA_real_,
    wash_drinking_water_time_sl  = c("5min_or_less", "5min_15min", "15min_30min",
                                     "30min_1hr", "more_than_1hr", "dnk", "pnta")
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  # All three sl_under_30_min codes → canonical label "5min_or_less"
  expect_equal(out$wash_drinking_water_time_cat[1], "5min_or_less")
  expect_equal(out$wash_drinking_water_time_cat[2], "5min_or_less")
  expect_equal(out$wash_drinking_water_time_cat[3], "5min_or_less")
  # sl_30min_1hr
  expect_equal(out$wash_drinking_water_time_cat[4], "30min_1hr")
  # sl_more_than_1hr
  expect_equal(out$wash_drinking_water_time_cat[5], "more_than_1hr")
  # sl_undefined codes → "undefined"
  expect_equal(out$wash_drinking_water_time_cat[6], "undefined")
  expect_equal(out$wash_drinking_water_time_cat[7], "undefined")
})


test_that("add_drinking_water_time_cat() — binary column has correct string values", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = c("water_in_dwelling", "number_minutes", "number_minutes",
                                     "number_minutes", "pnta"),
    wash_drinking_water_time_int = c(NA, 15, 45, 120, NA),
    wash_drinking_water_time_sl  = NA_character_
  )

  out <- suppressMessages(add_drinking_water_time_cat(.dataset = df))

  # premises and under-30 → "1"
  expect_equal(out$wash_drinking_water_time[1], "1")  # premises
  expect_equal(out$wash_drinking_water_time[2], "1")  # 15 min
  # 30-60 and > 60 → "0"
  expect_equal(out$wash_drinking_water_time[3], "0")  # 45 min
  expect_equal(out$wash_drinking_water_time[4], "0")  # 120 min
  # undefined → NA
  expect_true(is.na(out$wash_drinking_water_time[5]))
})


test_that("add_drinking_water_time_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    wash_drinking_water_source   = character(0),
    wash_drinking_water_time_yn  = character(0),
    wash_drinking_water_time_int = numeric(0),
    wash_drinking_water_time_sl  = character(0)
  )

  expect_error(add_drinking_water_time_cat(.dataset = df_empty))
})


test_that("add_drinking_water_time_cat() — error when required columns are missing", {

  df <- tibble::tibble(other_col = c(1, 2, 3))

  expect_error(add_drinking_water_time_cat(.dataset = df))
})


test_that("add_drinking_water_time_cat() — warning when overwriting existing output columns", {

  df <- tibble::tibble(
    wash_drinking_water_source      = "borehole",
    wash_drinking_water_time_yn     = "number_minutes",
    wash_drinking_water_time_int    = 15,
    wash_drinking_water_time_sl     = NA_character_,
    wash_drinking_water_time_cat    = "old_value"
  )

  expect_warning(suppressMessages(add_drinking_water_time_cat(.dataset = df)))
})


test_that("add_drinking_water_time_cat() — warning when coercing character time_int to integer", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = c("15", "45"),
    wash_drinking_water_time_sl  = NA_character_
  )

  expect_warning(suppressMessages(add_drinking_water_time_cat(.dataset = df)))
})


test_that("add_drinking_water_time_cat() — error when time_int has non-positive values", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "number_minutes",
    wash_drinking_water_time_int = c(15, -5),
    wash_drinking_water_time_sl  = NA_character_
  )

  expect_error(add_drinking_water_time_cat(.dataset = df))
})


test_that("add_drinking_water_time_cat() — error when time_yn contains invalid values", {

  df <- tibble::tibble(
    wash_drinking_water_source   = "borehole",
    wash_drinking_water_time_yn  = "unknown_response",
    wash_drinking_water_time_int = NA_real_,
    wash_drinking_water_time_sl  = NA_character_
  )

  expect_error(add_drinking_water_time_cat(.dataset = df))
})
