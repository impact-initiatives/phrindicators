# Tests for add_muac

test_that("add_muac() — valid dataset with cm values works", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5, 14.0),
    child_sex = c("m", "f", "f"),
    child_age_months = c(24, 30, 18),
    nut_edema_confirm = c("yes", NA, "no")
  )

  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_equal(nrow(out), 3)
  expect_true("sam_muac" %in% names(out))
  expect_true("mam_muac" %in% names(out))
  expect_true("gam_muac" %in% names(out))
  expect_true("flag_muac_extreme" %in% names(out))
})


test_that("add_muac() — SAM/MAM/GAM thresholds work correctly", {

  df <- tibble::tibble(
    nut_muac_cm = c(11.0, 12.0, 13.0),
    child_sex = c("m", "f", "m"),
    child_age_months = c(24, 30, 18),
    nut_edema_confirm = c("no", "no", "no")
  )


  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_equal(out$sam_muac[1], 1)  # < 11.5
  expect_equal(out$mam_muac[2], 1)  # >= 11.5 & < 12.5
  expect_equal(out$gam_muac[1], 1)  # < 12.5
  expect_equal(out$gam_muac[2], 1)  # < 12.5
  expect_equal(out$gam_muac[3], 0)  # >= 12.5
})


test_that("add_muac() — edema confirmation affects SAM/GAM", {

  df <- tibble::tibble(
    nut_muac_cm = c(13.0, 13.0),
    child_sex = c("m", "f"),
    child_age_months = c(24, 30),
    nut_edema_confirm = c("yes", "no")
  )

  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_equal(out$sam_muac[1], 1)  # edema confirmed
  expect_equal(out$sam_muac[2], 0)  # no edema
  expect_equal(out$gam_muac[1], 1)  # edema confirmed
  expect_equal(out$gam_muac[2], 0)  # no edema
})


test_that("add_muac() — children outside 6-59 months get NA", {

  df <- tibble::tibble(
    nut_muac_cm = c(11.0, 11.0, 11.0),
    child_sex = c("m", "f", "m"),
    child_age_months = c(5, 30, 60),
    nut_edema_confirm = c("no", "no", "no")
  )

  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_true(is.na(out$sam_muac[1]))   # age 5
  expect_false(is.na(out$sam_muac[2]))  # age 30
  expect_true(is.na(out$sam_muac[3]))   # age 60
})


test_that("add_muac() — extreme MUAC values are flagged", {

  df <- tibble::tibble(
    nut_muac_cm = c(4.5, 12.5, 21.0),
    child_sex = c("m", "f", "m"),
    child_age_months = c(24, 30, 18),
    nut_edema_confirm = c("no", "no", "no")
  )

  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_equal(out$flag_muac_extreme[1], 1)  # < 5
  expect_equal(out$flag_muac_extreme[2], 0)  # normal
  expect_equal(out$flag_muac_extreme[3], 1)  # > 20
})


test_that("add_muac() — detects mm and converts to cm", {

  df <- tibble::tibble(
    nut_muac_cm = c(125, 105, 140),  # actually in mm
    child_sex = c("m", "f", "m"),
    child_age_months = c(24, 30, 18),
    nut_edema_confirm = c("no", "no", "no")
  )

  out <- suppressMessages(add_muac(
    .dataset = df,
    nut_muac_cm_col = "nut_muac_cm",
    edema_confirm_col = "nut_edema_confirm",
    child_age_months_col = "child_age_months",
    edema_confirm_val = "yes"
  ))
  expect_true("nut_muac_cm" %in% names(out))
  expect_equal(out$nut_muac_cm[1], 12.5)
})


test_that("add_muac() — error on empty dataset", {

  df_empty <- tibble::tibble(
    nut_muac_cm = numeric(0),
    child_sex = character(0),
    child_age_months = numeric(0),
    nut_edema_confirm = character(0)
  )

  expect_error(
    add_muac(
      .dataset = df_empty,
      nut_muac_cm_col = "nut_muac_cm",
      edema_confirm_col = "nut_edema_confirm",
      child_age_months_col = "child_age_months",
      edema_confirm_yes = "yes"
    )
  )
})


test_that("add_muac() — error on missing columns", {

  df <- tibble::tibble(
    nut_muac_cm = c(12.5, 10.5)
  )

  expect_error(
    add_muac(
      .dataset = df,
      nut_muac_cm_col = "nut_muac_cm",
      edema_confirm_col = "nut_edema_confirm",
      child_age_months_col = "child_age_months",
      edema_confirm_yes = "yes"
    )
  )
})
