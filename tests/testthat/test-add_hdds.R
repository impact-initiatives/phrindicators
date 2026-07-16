# Tests for add_hdds

test_that("add_hdds() — standard use case works", {

  set.seed(123)

  df <- tibble::tibble(
    fsl_hdds_cereals    = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_tubers     = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_veg        = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_fruit      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_meat       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_eggs       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_fish       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_legumes    = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_dairy      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_oil        = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_sugar      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_condiments = sample(c("yes","no"), 30, TRUE)
  )

  out <- suppressMessages(add_hdds(df))

  expect_equal(nrow(out), 30)

  # Derived fields exist
  expect_true("fsl_hdds_score" %in% names(out))
  expect_true("fsl_hdds_cat"   %in% names(out))

  # Category values are valid (via grepl for translated labels)
  expect_true(all(grepl("Low|Medium|High", out$fsl_hdds_cat, ignore.case = TRUE)))
})


test_that("add_hdds() — missing required columns causes phr_error", {

  df_missing <- tibble::tibble(
    fsl_hdds_cereals = "yes",
    fsl_hdds_tubers  = "no"
    # Other 10 required columns missing
  )

  expect_error(
    add_hdds(df_missing),
    class = "phr_error"
  )
})


test_that("add_hdds() — invalid non-yes/no values cause phr_error", {

  df_bad <- tibble::tibble(
    fsl_hdds_cereals    = c("yes","INVALID","no"),
    fsl_hdds_tubers     = "yes",
    fsl_hdds_veg        = "yes",
    fsl_hdds_fruit      = "yes",
    fsl_hdds_meat       = "yes",
    fsl_hdds_eggs       = "yes",
    fsl_hdds_fish       = "yes",
    fsl_hdds_legumes    = "yes",
    fsl_hdds_dairy      = "yes",
    fsl_hdds_oil        = "yes",
    fsl_hdds_sugar      = "yes",
    fsl_hdds_condiments = "yes"
  )

  expect_warning(
    add_hdds(df_bad)
  )
})


test_that("add_hdds() — NA in any food group → HDDS score = NA", {

  df <- tibble::tibble(
    fsl_hdds_cereals    = c("yes", NA),
    fsl_hdds_tubers     = c("no",  "yes"),
    fsl_hdds_veg        = c("yes", "yes"),
    fsl_hdds_fruit      = c("yes", "no"),
    fsl_hdds_meat       = c("no",  "yes"),
    fsl_hdds_eggs       = c("yes", "yes"),
    fsl_hdds_fish       = c("no",  "yes"),
    fsl_hdds_legumes    = c("yes", "yes"),
    fsl_hdds_dairy      = c("no",  "yes"),
    fsl_hdds_oil        = c("yes", "no"),
    fsl_hdds_sugar      = c("no",  "yes"),
    fsl_hdds_condiments = c("yes", "yes")
  )

  out <- suppressMessages(add_hdds(df))

  expect_true(!is.na(out$fsl_hdds_score[1]))
  expect_true(is.na(out$fsl_hdds_score[2]))
})


test_that("add_hdds() — category assignment is correct", {

  df <- tibble::tibble(
    # Row 1: only 1 yes → Low
    fsl_hdds_cereals    = "yes",
    fsl_hdds_tubers     = "no",
    fsl_hdds_veg        = "no",
    fsl_hdds_fruit      = "no",
    fsl_hdds_meat       = "no",
    fsl_hdds_eggs       = "no",
    fsl_hdds_fish       = "no",
    fsl_hdds_legumes    = "no",
    fsl_hdds_dairy      = "no",
    fsl_hdds_oil        = "no",
    fsl_hdds_sugar      = "no",
    fsl_hdds_condiments = "no"
  )

  out <- suppressMessages(add_hdds(df))

  expect_true(grepl("Low", out$fsl_hdds_cat[1], ignore.case = TRUE))
})


test_that("add_hdds() — Medium and High category thresholds", {

  df <- tibble::tibble(
    fsl_hdds_cereals    = c("yes", "yes"),
    fsl_hdds_tubers     = c("yes", "yes"),
    fsl_hdds_veg        = c("no",  "yes"),
    fsl_hdds_fruit      = c("no",  "yes"),
    fsl_hdds_meat       = c("no",  "yes"),
    fsl_hdds_eggs       = c("no",  "yes"),
    fsl_hdds_fish       = c("no",  "no"),
    fsl_hdds_legumes    = c("no",  "yes"),
    fsl_hdds_dairy      = c("no",  "no"),
    fsl_hdds_oil        = c("no",  "yes"),
    fsl_hdds_sugar      = c("no",  "yes"),
    fsl_hdds_condiments = c("no",  "yes")
  )

  out <- suppressMessages(add_hdds(df))

  expect_true(grepl("Low", out$fsl_hdds_cat[1], ignore.case = TRUE))
  expect_true(grepl("High",   out$fsl_hdds_cat[2], ignore.case = TRUE))
})


test_that("add_hdds() — existing HDDS columns trigger a warning", {

  df <- tibble::tibble(
    fsl_hdds_cereals = "yes",
    fsl_hdds_tubers  = "no",
    fsl_hdds_veg     = "yes",
    fsl_hdds_fruit   = "no",
    fsl_hdds_meat    = "no",
    fsl_hdds_eggs    = "yes",
    fsl_hdds_fish    = "no",
    fsl_hdds_legumes = "yes",
    fsl_hdds_dairy   = "no",
    fsl_hdds_oil     = "yes",
    fsl_hdds_sugar   = "no",
    fsl_hdds_condiments = "yes",

    fsl_hdds_score = 99,   # existing var
    fsl_hdds_cat   = "Low" # existing var
  )

  expect_warning(
    add_hdds(df)
  )
})
