# Tests for hh_check_consent_skip

test_that("hh_check_consent_skip() returns empty list when all skip-logic is correct", {
  df <- tibble::tibble(
    consent = c("yes", "yes", "no"),
    sensitive_info = c("value1", "value2", NA),
    health_data = c("data1", "data2", NA)
  )

  out <- hh_check_consent_skip(df, "consent", c("sensitive_info", "health_data"))

  expect_equal(length(out), 0)
})

test_that("hh_check_consent_skip() detects violations of skip-logic", {
  df <- tibble::tibble(
    consent = c("yes", "no", "yes"),
    sensitive_info = c("value1", "value2", "value3")
  )

  out <- suppressWarnings(hh_check_consent_skip(df, "consent", "sensitive_info"))

  expect_true("sensitive_info" %in% names(out))
  expect_equal(out$sensitive_info, 2)
})

test_that("hh_check_consent_skip() handles multiple violations", {
  df <- tibble::tibble(
    consent = c("yes", "no", "no"),
    sensitive_info = c("value1", "value2", "value3"),
    health_data = c("data1", NA, "data3")
  )

  out <- hh_check_consent_skip(df, "consent", c("sensitive_info", "health_data"))

  expect_equal(length(out), 2)
  expect_true("sensitive_info" %in% names(out))
  expect_true("health_data" %in% names(out))
})

test_that("hh_check_consent_skip() treats empty strings as NA", {
  df <- tibble::tibble(
    consent = c("yes", "no"),
    sensitive_info = c("value1", "")
  )

  out <- suppressWarnings(hh_check_consent_skip(df, "consent", "sensitive_info"))

  expect_equal(length(out), 0)
})

test_that("hh_check_consent_skip() is case-insensitive for 'no'", {
  df <- tibble::tibble(
    consent = c("yes", "NO", "No"),
    sensitive_info = c("value1", "value2", "value3")
  )

  out <- suppressWarnings(hh_check_consent_skip(df, "consent", "sensitive_info"))

  expect_equal(length(out$sensitive_info), 2)
})

test_that("hh_check_consent_skip() returns warning when column missing", {
  df <- tibble::tibble(
    consent_col = c("yes", "no"),
    sensitive_info = c("value1", "value2")
  )

  expect_warning(hh_check_consent_skip(df, "consent", "sensitive_info"))
})

test_that("hh_check_consent_skip() handles missing skip columns gracefully", {
  df <- tibble::tibble(
    consent = c("yes", "no"),
    sensitive_info = c("value1", "value2")
  )

  out <- hh_check_consent_skip(df, "consent", c("sensitive_info", "missing_col"))

  expect_true("sensitive_info" %in% names(out))
})

test_that("hh_check_consent_skip() returns empty list when all consent is yes", {
  df <- tibble::tibble(
    consent = c("yes", "yes", "yes"),
    sensitive_info = c("value1", "value2", "value3")
  )

  out <- suppressWarnings(hh_check_consent_skip(df, "consent", "sensitive_info"))

  expect_equal(length(out), 0)
})

test_that("hh_check_consent_skip() warns when violations detected", {
  df <- tibble::tibble(
    consent = c("yes", "no"),
    sensitive_info = c("value1", "value2")
  )

  expect_warning(hh_check_consent_skip(df, "consent", "sensitive_info"))
})
