# Tests for add_sqm_per_person

test_that("add_sqm_per_person() — valid dataset creates all columns", {

  df <- tibble::tibble(
    shelter_shape = c("rectangle", "circle"),
    shelter_length_m = c(5, NA),
    shelter_width_m = c(4, NA),
    shelter_diameter_m = c(NA, 6),
    household_size = c(4, 3),
    shelter_measured = c("yes", "yes")
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shelter_shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "shelter_length_m",
    shelter_width_col = "shelter_width_m",
    shelter_diameter_col = "shelter_diameter_m",
    household_size_col = "household_size",
    measure_confirm_col = "shelter_measured",
    measure_confirm_yes_val = "yes"
  )

  expect_equal(nrow(out), 2)
  expect_true("area_sqm" %in% names(out))
  expect_true("sqm_per_person" %in% names(out))
  expect_true("sqm_per_person_cat" %in% names(out))
  expect_s3_class(out$sqm_per_person_cat, "factor")
})


test_that("add_sqm_per_person() — rectangle area calculation is correct", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "yes"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # 10 * 5 = 50 sqm, 50 / 10 = 5 sqm per person
  expect_equal(out$area_sqm[1], 50)
  expect_equal(out$sqm_per_person[1], 5)
})


test_that("add_sqm_per_person() — circle area calculation is correct", {

  df <- tibble::tibble(
    shape = "circle",
    length = NA,
    width = NA,
    diameter = 4,
    hh_size = 5,
    measured = "yes"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # pi * (4/2)^2 = pi * 4 = ~12.6 sqm
  expect_true(out$area_sqm[1] > 12 & out$area_sqm[1] < 13)
  expect_true(out$sqm_per_person[1] > 2 & out$sqm_per_person[1] < 3)
})


test_that("add_sqm_per_person() — categorization works correctly", {

  df <- tibble::tibble(
    shape = rep("rectangle", 4),
    length = c(10, 15, 20, 25),
    width = c(3, 4, 5, 6),
    diameter = rep(NA, 4),
    hh_size = rep(10, 4),
    measured = rep("yes", 4)
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # Row 1: 30/10 = 3 sqm per person (< 3.5)
  expect_true(grepl("<3.5", out$sqm_per_person_cat[1]))
  # Row 2: 60/10 = 6 sqm per person (>= 5.5)
  expect_true(grepl(">= 5.5", out$sqm_per_person_cat[2]))
})


test_that("add_sqm_per_person() — measurement not confirmed returns NA", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "no"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  expect_true(is.na(out$area_sqm[1]))
  expect_true(is.na(out$sqm_per_person[1]))
  expect_true(is.na(out$sqm_per_person_cat[1]))
})


test_that("add_sqm_per_person() — error on empty dataset", {

  df_empty <- tibble::tibble(
    shape = character(0),
    length = numeric(0),
    width = numeric(0),
    diameter = numeric(0),
    hh_size = numeric(0),
    measured = character(0)
  )

  expect_error(
    add_sqm_per_person(
      .dataset = df_empty,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})


test_that("add_sqm_per_person() — error on missing columns", {

  df <- tibble::tibble(
    shape = c("rectangle")
  )

  expect_error(
    add_sqm_per_person(
      .dataset = df,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})


test_that("add_sqm_per_person() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "yes",
    area_sqm = 99
  )

  expect_warning(
    add_sqm_per_person(
      .dataset = df,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})
