# Tests for add_handwashing_facility_cat

# Full set of required columns; helpers below override individual rows.
base_df <- function() {
  tibble::tibble(
    survey_modality = "in_person",
    wash_handwashing_facility = "none",
    wash_handwashing_facility_observed_water = NA_character_,
    wash_soap_observed_yn = NA_character_,
    wash_soap_observed_type = NA_character_,
    wash_handwashing_facility_reported = NA_character_,
    wash_handwashing_facility_water_reported_yn = NA_character_,
    wash_soap_reported_yn = NA_character_,
    wash_soap_reported_type = NA_character_
  )
}


test_that("add_handwashing_facility_cat() — observed path classes", {

  df <- tibble::tibble(
    survey_modality = rep("in_person", 4),
    wash_handwashing_facility = c(
      "available_fixed_in_dwelling", # basic
      "available_mobile",            # limited (no soap type)
      "available_fixed_in_plot",     # limited (water/soap absent)
      "none"                         # no_facility
    ),
    wash_handwashing_facility_observed_water = c("water_available", "water_available", "water_not_available", "water_not_available"),
    wash_soap_observed_yn = c("soap_available", "soap_available", "soap_not_available", "soap_not_available"),
    wash_soap_observed_type = c("soap", "ash_mud_sand", NA, NA),
    wash_handwashing_facility_reported = NA_character_,
    wash_handwashing_facility_water_reported_yn = NA_character_,
    wash_soap_reported_yn = NA_character_,
    wash_soap_reported_type = NA_character_
  )

  out <- suppressMessages(add_handwashing_facility_cat(df))

  expect_equal(
    out$wash_handwashing_facility_jmp_cat,
    c("basic", "limited", "limited", "no_facility")
  )
})


test_that("add_handwashing_facility_cat() — observed soap type downgrades basic to limited", {

  df <- tibble::tibble(
    survey_modality = rep("in_person", 3),
    wash_handwashing_facility = rep("available_fixed_in_dwelling", 3),
    wash_handwashing_facility_observed_water = rep("water_available", 3),
    wash_soap_observed_yn = rep("soap_available", 3),
    wash_soap_observed_type = c("soap", "ash_mud_sand", "dnk"),
    wash_handwashing_facility_reported = NA_character_,
    wash_handwashing_facility_water_reported_yn = NA_character_,
    wash_soap_reported_yn = NA_character_,
    wash_soap_reported_type = NA_character_
  )

  out <- suppressMessages(add_handwashing_facility_cat(df))

  expect_equal(out$wash_handwashing_facility_jmp_cat, c("basic", "limited", "limited"))
})


test_that("add_handwashing_facility_cat() — reported path used for remote and for no-permission", {

  df <- tibble::tibble(
    survey_modality = c("remote", "remote", "remote", "in_person"),
    wash_handwashing_facility = c(NA, NA, NA, "no_permission"),
    wash_handwashing_facility_observed_water = NA_character_,
    wash_soap_observed_yn = NA_character_,
    wash_soap_observed_type = NA_character_,
    wash_handwashing_facility_reported = c("fixed_dwelling", "mobile", "none", "fixed_yard"),
    wash_handwashing_facility_water_reported_yn = c("yes", "no", NA, "yes"),
    wash_soap_reported_yn = c("yes", "no", NA, "yes"),
    wash_soap_reported_type = c("soap", NA, NA, "ash_mud_sand")
  )

  out <- suppressMessages(add_handwashing_facility_cat(df))

  expect_equal(
    out$wash_handwashing_facility_jmp_cat,
    c("basic", "limited", "no_facility", "limited")
  )
})


test_that("add_handwashing_facility_cat() — reported undefined facility yields 'undefined'", {

  df <- dplyr::bind_rows(base_df(), base_df())
  df$survey_modality <- "remote"
  df$wash_handwashing_facility <- NA_character_
  df$wash_handwashing_facility_reported <- c("dnk", "other")

  out <- suppressMessages(add_handwashing_facility_cat(df))

  expect_equal(out$wash_handwashing_facility_jmp_cat, c("undefined", "undefined"))
})


test_that("add_handwashing_facility_cat() — intermediate helper columns are dropped", {

  df <- base_df()
  df <- dplyr::bind_rows(df, df)

  out <- suppressMessages(add_handwashing_facility_cat(df))

  expect_equal(names(out), c(names(df), "wash_handwashing_facility_jmp_cat"))
  expect_false(any(grepl("^\\.", names(out))))
})


test_that("add_handwashing_facility_cat() — error on empty dataset", {

  expect_error(suppressMessages(add_handwashing_facility_cat(base_df()[0, ])))
})


test_that("add_handwashing_facility_cat() — error when required columns missing", {

  df <- tibble::tibble(survey_modality = c("in_person", "remote"))

  expect_error(suppressMessages(add_handwashing_facility_cat(df)))
})


test_that("add_handwashing_facility_cat() — error when facility_no is not length 1", {

  df <- dplyr::bind_rows(base_df(), base_df())

  expect_error(
    suppressMessages(add_handwashing_facility_cat(df, facility_no = c("none", "other")))
  )
})


test_that("add_handwashing_facility_cat() — warning when overwriting existing column", {

  df <- dplyr::bind_rows(base_df(), base_df())
  df$wash_handwashing_facility_jmp_cat <- "old"

  expect_warning(suppressMessages(add_handwashing_facility_cat(df)))
})
