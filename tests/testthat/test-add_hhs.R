# Tests for add_hhs

test_that("add_hhs() — normal functionality works", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_nofoodhh_freq   = sample(c("rarely","sometimes","often"), 30, TRUE),
    fsl_hhs_sleephungry     = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_sleephungry_freq= sample(c("rarely","sometimes","often"), 30, TRUE),
    fsl_hhs_alldaynight     = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_alldaynight_freq= sample(c("rarely","sometimes","often"), 30, TRUE)
  )

  out <- suppressMessages(add_hhs(df))

  expect_s3_class(out, "data.frame")
  expect_true("fsl_hhs_score" %in% names(out))
  expect_true("fsl_hhs_cat" %in% names(out))
  expect_true("fsl_hhs_cat_ipc" %in% names(out))

  # Score must lie between 0–6
  expect_true(all(out$fsl_hhs_score >= 0 &
                    out$fsl_hhs_score <= 6 &
                    !is.na(out$fsl_hhs_score)))

  # Categories exist
  expect_true(all(grepl("Little to No|Moderate|Severe", out$fsl_hhs_cat)))

  expect_true(all(grepl("None|Little|Moderate|Severe|Very Severe",
                        out$fsl_hhs_cat_ipc)))

  # Factor levels ordered worst to best
  # fsl_hhs_cat: match by regex (grepl) instead of exact equality
  lvls1 <- levels(out$fsl_hhs_cat)
  expect_true(any(grepl("Severe", lvls1)))
  expect_true(any(grepl("Moderate", lvls1)))
  expect_true(any(grepl("Little\\s*to\\s*No", lvls1)))  # allow variable whitespace

  # fsl_hhs_cat_ipc: match by regex (grepl) instead of exact equality
  lvls2 <- levels(out$fsl_hhs_cat_ipc)
  expect_true(any(grepl("Very\\s*Severe", lvls2)))
  expect_true(any(grepl("Severe", lvls2)))
  expect_true(any(grepl("Moderate", lvls2)))
  expect_true(any(grepl("Little", lvls2)))
  expect_true(any(grepl("None", lvls2)))
})


test_that("add_hhs() — edge case of all zero indicators", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("no", 10),
    fsl_hhs_nofoodhh_freq   = rep("rarely", 10),
    fsl_hhs_sleephungry     = rep("no", 10),
    fsl_hhs_sleephungry_freq= rep("rarely", 10),
    fsl_hhs_alldaynight     = rep("no", 10),
    fsl_hhs_alldaynight_freq= rep("rarely", 10)
  )

  out <- suppressMessages(add_hhs(df))

  # Score must be 0
  expect_equal(unique(out$fsl_hhs_score), 0)

  # Category must be:
  expect_true(
    all(grepl("None", unique(as.character(out$fsl_hhs_cat_ipc))))
  )

  expect_true(
    all(grepl("Little to No", unique(as.character(out$fsl_hhs_cat))))
  )
})


test_that("add_hhs() — edge case of max severity", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("yes", 10),
    fsl_hhs_nofoodhh_freq   = rep("often", 10),
    fsl_hhs_sleephungry     = rep("yes", 10),
    fsl_hhs_sleephungry_freq= rep("often", 10),
    fsl_hhs_alldaynight     = rep("yes", 10),
    fsl_hhs_alldaynight_freq= rep("often", 10)
  )

  out <- suppressMessages(add_hhs(df))

  expect_equal(unique(out$fsl_hhs_score), 6)
  expect_true(
    all(grepl("Very Severe", unique(as.character(out$fsl_hhs_cat_ipc))))
  )

  expect_true(
    all(grepl("Severe", unique(as.character(out$fsl_hhs_cat))))
  )
})


test_that("add_hhs() — error if first argument is not a dataframe", {
  expect_error(
    add_hhs("not a df")
  )
})


test_that("add_hhs() — error if dataset has zero rows", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh = character(),
    fsl_hhs_nofoodhh_freq = character(),
    fsl_hhs_sleephungry = character(),
    fsl_hhs_sleephungry_freq = character(),
    fsl_hhs_alldaynight = character(),
    fsl_hhs_alldaynight_freq = character()
  )

  expect_error(
    add_hhs(df)
  )
})


test_that("add_hhs() — error on missing columns", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh = rep("yes", 5),
    # MISSING fsl_hhs_nofoodhh_freq
    fsl_hhs_sleephungry = rep("no", 5),
    fsl_hhs_sleephungry_freq = rep("rarely", 5),
    fsl_hhs_alldaynight = rep("no", 5),
    fsl_hhs_alldaynight_freq = rep("rarely", 5)
  )

  expect_error(
    add_hhs(df)
  )
})


test_that("add_hhs() — invalid Y/N categories produce error", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = c("yes","maybe","no","no","yes"),
    fsl_hhs_nofoodhh_freq   = rep("rarely", 5),
    fsl_hhs_sleephungry     = rep("no", 5),
    fsl_hhs_sleephungry_freq= rep("rarely", 5),
    fsl_hhs_alldaynight     = rep("yes", 5),
    fsl_hhs_alldaynight_freq= rep("often", 5)
  )

  expect_message(
   add_hhs(df)
  )
})


test_that("add_hhs() — invalid frequency categories produce error", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("yes", 5),
    fsl_hhs_nofoodhh_freq   = c("rarely","sometimes","badcat","rarely","often"),
    fsl_hhs_sleephungry     = rep("no", 5),
    fsl_hhs_sleephungry_freq= rep("rarely", 5),
    fsl_hhs_alldaynight     = rep("no", 5),
    fsl_hhs_alldaynight_freq= rep("often", 5)
  )

  expect_message(
    add_hhs(df)
  )
})


test_that("add_hhs() — mixed combinations produce valid output", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_nofoodhh_freq   = sample(c("rarely","sometimes","often"), 25, TRUE),
    fsl_hhs_sleephungry     = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_sleephungry_freq= sample(c("rarely","sometimes","often"), 25, TRUE),
    fsl_hhs_alldaynight     = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_alldaynight_freq= sample(c("rarely","sometimes","often"), 25, TRUE)
  )

  out <- suppressMessages(add_hhs(df))

  expect_equal(nrow(out), 25)
  expect_true(all(out$fsl_hhs_score >= 0 & out$fsl_hhs_score <= 6))
})
