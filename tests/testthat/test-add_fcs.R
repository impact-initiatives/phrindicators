# Tests for add_fcs

test_that("add_fcs() — valid dataset computes scores and categories", {

  set.seed(123)
  df_fcs_valid <- tibble::tibble(
    fsl_fcs_cereal  = sample(0:7, 30, replace = TRUE),
    fsl_fcs_legumes = sample(0:7, 30, replace = TRUE),
    fsl_fcs_veg     = sample(0:7, 30, replace = TRUE),
    fsl_fcs_fruit   = sample(0:3, 30, replace = TRUE),
    fsl_fcs_meat    = sample(0:4, 30, replace = TRUE),
    fsl_fcs_dairy   = sample(0:4, 30, replace = TRUE),
    fsl_fcs_sugar   = sample(0:7, 30, replace = TRUE),
    fsl_fcs_oil     = sample(0:7, 30, replace = TRUE)
  )

  out <- add_fcs(df_fcs_valid)

  expect_equal(nrow(out), 30)
  expect_true(all(!is.na(out$fsl_fcs_score)))
  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )
  expect_s3_class(out$fsl_fcs_cat, "factor")
})


test_that("add_fcs() — works across 500 diverse FCS combinations", {

  set.seed(456)

  # Create a diverse grid of 500 combinations
  # Strategy: sample uniformly across the valid range for each variable
  # to ensure we test low, medium, and high values
  n <- 500

  df_fcs_expanded <- tibble::tibble(
    # Cereals (0-7): include full range
    fsl_fcs_cereal  = sample(0:7, n, replace = TRUE),

    # Legumes (0-7): include full range
    fsl_fcs_legumes = sample(0:7, n, replace = TRUE),

    # Vegetables (0-7): include full range
    fsl_fcs_veg     = sample(0:7, n, replace = TRUE),

    # Fruit (0-3): smaller range, ensure good coverage
    fsl_fcs_fruit   = sample(0:3, n, replace = TRUE,
                             prob = c(0.3, 0.25, 0.25, 0.2)),

    # Meat (0-4): ensure we get extremes and middle values
    fsl_fcs_meat    = sample(0:4, n, replace = TRUE,
                             prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),

    # Dairy (0-4): similar to meat
    fsl_fcs_dairy   = sample(0:4, n, replace = TRUE,
                             prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),

    # Sugar (0-7): full range with emphasis on extremes
    fsl_fcs_sugar   = sample(0:7, n, replace = TRUE,
                             prob = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2)),

    # Oil (0-7): full range
    fsl_fcs_oil     = sample(0:7, n, replace = TRUE)
  )

  # Ensure we have some edge cases by replacing a few rows
  # All zeros (extreme poor)
  df_fcs_expanded[1, ] <- list(0, 0, 0, 0, 0, 0, 0, 0)

  # All max values (extreme acceptable)
  df_fcs_expanded[2, ] <- list(7, 7, 7, 7, 7, 7, 7, 7)

  # Borderline case: moderate values
  df_fcs_expanded[3, ] <- list(3, 2, 3, 1, 1, 1, 2, 3)

  # Mixed extreme: high staples, low protein
  df_fcs_expanded[4, ] <- list(7, 7, 7, 0, 0, 0, 5, 7)

  # Mixed extreme: low staples, high protein
  df_fcs_expanded[5, ] <- list(2, 1, 1, 3, 4, 4, 0, 2)

  out <- add_fcs(df_fcs_expanded)

  # Basic structure checks
  expect_equal(nrow(out), 500)
  expect_true(all(!is.na(out$fsl_fcs_score)))
  expect_s3_class(out$fsl_fcs_cat, "factor")

  # Check all categories are valid
  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )

  # Check score ranges are reasonable (FCS typically 0-112)
  expect_true(all(out$fsl_fcs_score >= 0))
  expect_true(all(out$fsl_fcs_score <= 112))

  # Verify we got diverse category distribution
  category_counts <- table(out$fsl_fcs_cat)
  expect_true(length(category_counts) >= 2,
              info = "Should have at least 2 different FCS categories")

  # Check edge cases produce expected ranges
  expect_equal(out$fsl_fcs_score[1], 0)  # All zeros = score 0
  expect_true(out$fsl_fcs_score[2] > 100)  # Max values = high score
  expect_equal(out$fsl_fcs_cat[1], factor("Poor", levels = c("Poor", "Borderline", "Acceptable")))
})


test_that("add_fcs() — dataset with NA produces NA scores appropriately", {

  set.seed(456)
  df_fcs_with_na <- tibble::tibble(
    fsl_fcs_cereal  = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_legumes = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_veg     = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_fruit   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_meat    = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_dairy   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_sugar   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_oil     = sample(c(0:7, NA), 30, replace = TRUE)
  )

  out <- add_fcs(df_fcs_with_na)

  expect_equal(nrow(out), 30)
  expect_true(any(is.na(out$fsl_fcs_score)))
})


test_that("add_fcs() — out-of-range values trigger phr_error", {

  set.seed(123)
  df_bad <- tibble::tibble(
    fsl_fcs_cereal  = sample(0:7, 30, TRUE),
    fsl_fcs_legumes = sample(0:7, 30, TRUE),
    fsl_fcs_veg     = sample(0:7, 30, TRUE),
    fsl_fcs_fruit   = sample(0:7, 30, TRUE),
    fsl_fcs_meat    = sample(0:7, 30, TRUE),
    fsl_fcs_dairy   = sample(0:7, 30, TRUE),
    fsl_fcs_sugar   = sample(0:7, 30, TRUE),
    fsl_fcs_oil     = sample(0:7, 30, TRUE)
  )

  df_bad$fsl_fcs_veg[1]  <- 10
  df_bad$fsl_fcs_meat[5] <- -3
  df_bad$fsl_fcs_oil[10] <- 14

  expect_warning(
    add_fcs(df_bad)
  )

})


test_that("add_fcs() — non-numeric values trigger phr_error", {

  df_nonnumeric <- tibble::tibble(
    fsl_fcs_cereal  = c(1,2,3,4,5,6,7,0,1,2, rep(3, 20)),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(2, 30),
    fsl_fcs_fruit   = rep(3, 30),
    fsl_fcs_meat    = rep(4, 30),
    fsl_fcs_dairy   = rep(5, 30),
    fsl_fcs_sugar   = rep(6, 30),
    fsl_fcs_oil     = rep(7, 30)
  )

  df_nonnumeric$fsl_fcs_sugar[2] <- "three"
  df_nonnumeric$fsl_fcs_meat[18] <- "x"
  df_nonnumeric$fsl_fcs_cereal[20] <- "7a"

  out <- add_fcs(df_nonnumeric)

  expect_warning(
    add_fcs(df_nonnumeric)
  )
})


test_that("add_fcs() — warns when overwriting existing score/category columns", {

  df_overwrite <- tibble::tibble(
    fsl_fcs_cereal  = rep(1, 30),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(1, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30),
    fsl_fcs_score   = rep(999, 30),
    fsl_fcs_cat     = rep("Old", 30)
  )

  expect_warning(
    add_fcs(df_overwrite)
  )
})


test_that("add_fcs() — alternative cutoffs applied correctly", {

  df <- tibble::tibble(
    fsl_fcs_cereal  = c(7, 0, 3, 7, 5, rep(2, 25)),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(1, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30)
  )

  out <- add_fcs(df, cutoffs = "alternative")

  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )

})


test_that("add_fcs() — invalid cutoff gives warning and defaults to normal", {

  df <- tibble::tibble(
    fsl_fcs_cereal  = rep(3, 30),
    fsl_fcs_legumes = rep(2, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(2, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30)
  )

  expect_warning(
    out <- add_fcs(df, cutoffs = "INVALID")
  )

  expect_true("fsl_fcs_score" %in% names(out))

})


test_that("add_fcs() — missing columns cause error", {

  df_missing <- tibble::tibble(
    fsl_fcs_cereal = 1:30,
    fsl_fcs_legumes = 1:30
    # missing 6 required vars
  )

  expect_error(
    add_fcs(df_missing)
  )
})


test_that("add_fcs() — empty dataframe rejected", {

  df_empty <- tibble::tibble(
    fsl_fcs_cereal = numeric(0),
    fsl_fcs_legumes = numeric(0),
    fsl_fcs_veg = numeric(0),
    fsl_fcs_fruit = numeric(0),
    fsl_fcs_meat = numeric(0),
    fsl_fcs_dairy = numeric(0),
    fsl_fcs_sugar = numeric(0),
    fsl_fcs_oil = numeric(0)
  )

  expect_error(add_fcs(df_empty))
})
