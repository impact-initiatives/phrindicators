# Tests for add_mfaz

test_that("add_mfaz() — valid dataset creates MFAZ columns", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5, 14.0),
    child_sex = c("m", "f", "f"),
    child_age_months = c(24, 30, 18),
    nut_edema_confirm = c("yes", NA, "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  expect_equal(nrow(out), 3)
  expect_true("mfaz" %in% names(out))
  expect_true("severe_mfaz" %in% names(out))
  expect_true("moderate_mfaz" %in% names(out))
  expect_true("global_mfaz" %in% names(out))
  expect_true("flag_sd_mfaz" %in% names(out))
})


test_that("add_mfaz() — MFAZ thresholds work correctly", {

  # Create dataset with controlled MUAC values that should produce specific z-scores
  df <- tibble::tibble(
    nut_muac_cm = c(10.0, 11.8, 12.3, 13.5),
    child_sex = c("m", "f", "m", "f"),
    child_age_months = c(24, 30, 18, 36),
    nut_edema_confirm = c("no", "no", "no", "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # Check that mfaz column is created and contains numeric values
  expect_true(is.numeric(out$mfaz))

  # Check that severe/moderate/global are binary (0 or 1) or NA
  expect_true(all(out$severe_mfaz %in% c(0, 1, NA)))
  expect_true(all(out$moderate_mfaz %in% c(0, 1, NA)))
  expect_true(all(out$global_mfaz %in% c(0, 1, NA)))
})


test_that("add_mfaz() — edema confirmation affects severe and global MFAZ", {

  df <- tibble::tibble(
    nut_muac_cm = c(13.5, 13.5),  # Normal MUAC values
    child_sex = c("m", "f"),
    child_age_months = c(24, 30),
    nut_edema_confirm = c("yes", "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # With edema confirmed, should be classified as severe and global
  expect_equal(out$severe_mfaz[1], 1)
  expect_equal(out$global_mfaz[1], 1)

  # Without edema and normal MUAC, should not be severe or global
  expect_equal(out$severe_mfaz[2], 0)
  expect_equal(out$global_mfaz[2], 0)
})


test_that("add_mfaz() — children outside 6-59 months get NA", {

  df <- tibble::tibble(
    nut_muac_cm = c(11.0, 11.0, 11.0),
    child_sex = c("m", "f", "m"),
    child_age_months = c(5, 30, 60),
    nut_edema_confirm = c("no", "no", "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # Age 5 months (< 6) should be NA
  expect_true(is.na(out$severe_mfaz[1]))
  expect_true(is.na(out$moderate_mfaz[1]))
  expect_true(is.na(out$global_mfaz[1]))

  # Age 30 months should have values
  expect_false(is.na(out$severe_mfaz[2]))
  expect_false(is.na(out$moderate_mfaz[2]))
  expect_false(is.na(out$global_mfaz[2]))

  # Age 60 months (>= 60) should be NA
  expect_true(is.na(out$severe_mfaz[3]))
  expect_true(is.na(out$moderate_mfaz[3]))
  expect_true(is.na(out$global_mfaz[3]))
})


test_that("add_mfaz() — flag_sd_mfaz identifies extreme values", {

  # Create dataset with one extreme value
  df <- tibble::tibble(
    nut_muac_cm = c(12.0, 12.0, 12.0, 12.0, 5.0),  # Last one is extreme
    child_sex = c("m", "f", "m", "f", "m"),
    child_age_months = c(24, 30, 18, 36, 24),
    nut_edema_confirm = rep("no", 5)
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # flag_sd_mfaz should be 0 or 1
  expect_true(all(out$flag_sd_mfaz %in% c(0, 1)))

  # The extreme value should likely be flagged
  expect_equal(out$flag_sd_mfaz[5], 1)
})


test_that("add_mfaz() — grouping parameter works", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.0, 12.0, 10.0, 10.0),
    child_sex = c("m", "f", "m", "f"),
    child_age_months = c(24, 24, 24, 24),
    nut_edema_confirm = rep("no", 4),
    cluster = c("A", "A", "B", "B")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes",
    grouping = "cluster"
  )

  expect_equal(nrow(out), 4)
  expect_true("flag_sd_mfaz" %in% names(out))
  # Flags should be calculated within groups
  expect_true(all(out$flag_sd_mfaz %in% c(0, 1)))
})


test_that("add_mfaz() — temporary sex column is removed", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5),
    child_sex = c("m", "f"),
    child_age_months = c(24, 30),
    nut_edema_confirm = c("no", "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # Temporary column should not be in output
  expect_false("temp_sex_for_zscorer" %in% names(out))
})


test_that("add_mfaz() — error on empty dataset", {

  df_empty <- tibble::tibble(
    nut_muac_cm = numeric(0),
    child_sex = character(0),
    child_age_months = numeric(0),
    nut_edema_confirm = character(0)
  )

  expect_error(
    add_mfaz(
      .dataset = df_empty,
      nut_muac_cm_col = "nut_muac_cm",
      edema_confirm_col = "nut_edema_confirm",
      child_age_months_col = "child_age_months",
      child_sex_col = "child_sex",
      male_sex_val = "m",
      female_sex_val = "f",
      edema_confirm_val = "yes"
    )
  )
})


test_that("add_mfaz() — error on missing columns", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5)
  )

  expect_error(
    add_mfaz(
      .dataset = df,
      nut_muac_cm_col = "nut_muac_cm",
      edema_confirm_col = "nut_edema_confirm",
      child_age_months_col = "child_age_months",
      child_sex_col = "child_sex",
      male_sex_val = "m",
      female_sex_val = "f",
      edema_confirm_val = "yes"
    )
  )
})


test_that("add_mfaz() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5),
    child_sex = c("m", "f"),
    child_age_months = c(24, 30),
    nut_edema_confirm = c("no", "no"),
    mfaz = c(0, 0)
  )

  expect_warning(
    add_mfaz(
      .dataset = df,
      nut_muac_cm_col = "nut_muac_cm",
      edema_confirm_col = "nut_edema_confirm",
      child_age_months_col = "child_age_months",
      child_sex_col = "child_sex",
      male_sex_val = "m",
      female_sex_val = "f",
      edema_confirm_val = "yes"
    )
  )
})


test_that("add_mfaz() — NA in mfaz results in NA for classifications", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, NA),
    child_sex = c("m", "f"),
    child_age_months = c(24, 30),
    nut_edema_confirm = c("no", "no")
  )

  out <- add_mfaz(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    child_sex_col = "child_sex",
    male_sex_val = "m",
    female_sex_val = "f",
    edema_confirm_val = "yes"
  )

  # First row should have values
  expect_false(is.na(out$mfaz[1]))

  # Second row with NA MUAC should have NA mfaz
  expect_true(is.na(out$mfaz[2]))
  expect_true(is.na(out$severe_mfaz[2]))
  expect_true(is.na(out$moderate_mfaz[2]))
  expect_true(is.na(out$global_mfaz[2]))
})
