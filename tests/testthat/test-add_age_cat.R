# Tests for add_age_cat

test_that("add_age_cat() — valid dataset creates age categories", {

  df <- tibble::tibble(
    age_years = c(0, 3, 7, 10, 45, 87, 122)
  )

  out <- suppressMessages(add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  ))
  expect_equal(nrow(out), 7)
  expect_true("age_cat" %in% names(out))
  expect_s3_class(out$age_cat, "factor")

  # Check specific categories
  expect_equal(as.character(out$age_cat[1]), "0-4")
  expect_equal(as.character(out$age_cat[3]), "5-9")
  expect_equal(as.character(out$age_cat[5]), "45-49")
})


test_that("add_age_cat() — NA values are handled correctly", {

  df <- tibble::tibble(
    age_years = c(5, 10, NA, 25)
  )

  out <- suppressMessages(add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  ))
  expect_equal(nrow(out), 4)
  expect_true(is.na(out$age_cat[3]))
  expect_false(is.na(out$age_cat[1]))
})


test_that("add_age_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_years = numeric(0)
  )

  expect_error(
    add_age_cat(
      .dataset = df_empty,
      age_years_col = "age_years"
    )
  )
})


test_that("add_age_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(5, 10, 15)
  )

  expect_error(
    add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})


test_that("add_age_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    age_years = c(5, 10, 15),
    age_cat = c("old", "old", "old")
  )

  expect_warning(
    add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})


test_that("add_age_cat() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    age_years = c(5, 10, "fifteen", 20)
  )

  expect_error(
    a <- add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})


test_that("add_age_cat() — boundary values are categorized correctly", {

  df <- tibble::tibble(
    age_years = c(0, 4, 5, 9, 10, 14, 15, 124)
  )

  out <- add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(as.character(out$age_cat[1]), "0-4")
  expect_equal(as.character(out$age_cat[2]), "0-4")
  expect_equal(as.character(out$age_cat[3]), "5-9")
  expect_equal(as.character(out$age_cat[4]), "5-9")
  expect_equal(as.character(out$age_cat[5]), "10-14")
  expect_equal(as.character(out$age_cat[8]), "120-124")
})
