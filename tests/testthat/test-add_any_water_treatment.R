# Tests for add_any_water_treatment

test_that("add_any_water_treatment() — valid dataset creates treatment indicator", {

  df <- tibble::tibble(
    water_treatment = c("boiling", "none", "chlorine", "filter")
  )

  out <- suppressMessages(add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "water_treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
  ))
  expect_equal(nrow(out), 4)
  expect_true("wash_any_water_treatment" %in% names(out))
})


test_that("add_any_water_treatment() — categorization logic works", {

  df <- tibble::tibble(
    treatment = c("boiling", "chlorine", "none", "filter", "other")
  )

  out <- suppressMessages(add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
  ))
  expect_equal(out$wash_any_water_treatment[1], "yes")
  expect_equal(out$wash_any_water_treatment[2], "yes")
  expect_equal(out$wash_any_water_treatment[3], "no")
  expect_equal(out$wash_any_water_treatment[4], "yes")
  expect_true(is.na(out$wash_any_water_treatment[5]))
})


test_that("add_any_water_treatment() — NA and unknown values return NA", {

  df <- tibble::tibble(
    treatment = c(NA, "dnk", "pnta")
  )

  out <- suppressMessages(add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "treatment",
    yes_values = c("boiling"),
    no_values = c("none")
  ))
  expect_true(all(is.na(out$wash_any_water_treatment)))
})


test_that("add_any_water_treatment() — error on empty dataset", {

  df_empty <- tibble::tibble(
    treatment = character(0)
  )

  expect_error(
    add_any_water_treatment(
      .dataset = df_empty,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})


test_that("add_any_water_treatment() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("boiling", "none")
  )

  expect_error(
    add_any_water_treatment(
      .dataset = df,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})


test_that("add_any_water_treatment() — warning when overwriting existing column", {

  df <- tibble::tibble(
    treatment = c("boiling"),
    wash_any_water_treatment = "old"
  )

  expect_warning(
    add_any_water_treatment(
      .dataset = df,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})
