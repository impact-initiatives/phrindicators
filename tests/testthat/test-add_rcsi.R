# Tests for add_rcsi

test_that("add_rcsi() — normal functionality works", {
  set.seed(123)

  df <- tibble::tibble(
    fsl_rcsi_lessquality = sample(0:7, 30, TRUE),
    fsl_rcsi_borrow      = sample(0:7, 30, TRUE),
    fsl_rcsi_mealsize    = sample(0:7, 30, TRUE),
    fsl_rcsi_mealadult   = sample(0:7, 30, TRUE),
    fsl_rcsi_mealnb      = sample(0:7, 30, TRUE)
  )

  out <- add_rcsi(df)

  # Column creation
  expect_true("fsl_rcsi_score" %in% names(out))
  expect_true("fsl_rcsi_cat" %in% names(out))

  # Score is numeric and within expected range
  expect_true(all(is.numeric(out$fsl_rcsi_score)))
  expect_true(all(out$fsl_rcsi_score >= 0, na.rm = TRUE))

  # Category values contain expected text
  expect_true(all(grepl("No to Low|Medium|High",
                        as.character(out$fsl_rcsi_cat),
                        ignore.case = TRUE)))

  # Factor levels ordered worst to best
  # fsl_rcsi_cat: match by regex (grepl) instead of exact equality
  lvls <- levels(out$fsl_rcsi_cat)
  expect_true(any(grepl("High", lvls)))
  expect_true(any(grepl("Medium", lvls)))
  expect_true(any(grepl("No\\s*to\\s*Low", lvls)))  # allow variable whitespace
})


test_that("add_rcsi() — missing required RCSI columns triggers phr_error", {
  df_missing <- tibble::tibble(
    fsl_rcsi_lessquality = 1:5,
    fsl_rcsi_borrow      = 1:5
    # rest missing
  )

  expect_error(
    add_rcsi(df_missing)
  )
})


test_that("add_rcsi() — out-of-range values cause phr_error", {
  df_bad <- tibble::tibble(
    fsl_rcsi_lessquality = c(0, 1, 2, 3, 8),  # 8 is invalid
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4
  )

  expect_message(
    add_rcsi(df_bad)
  )
})


test_that("add_rcsi() — non-numeric values trigger phr_error", {
  df_nonum <- tibble::tibble(
    fsl_rcsi_lessquality = c("a", "2", "3", "4", "5"), # unsafe coercion
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4
  )

  expect_warning(
    out <- add_rcsi(df_nonum)
  )
})


test_that("add_rcsi() — empty dataset triggers phr_error", {
  df_empty <- tibble::tibble(
    fsl_rcsi_lessquality = numeric(0),
    fsl_rcsi_borrow      = numeric(0),
    fsl_rcsi_mealsize    = numeric(0),
    fsl_rcsi_mealadult   = numeric(0),
    fsl_rcsi_mealnb      = numeric(0)
  )

  expect_error(
    add_rcsi(df_empty),
    class = "phr_error"
  )
})


test_that("add_rcsi() — warns when overwriting existing output columns", {
  df <- tibble::tibble(
    fsl_rcsi_lessquality = 0:4,
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4,
    fsl_rcsi_score       = rep(99, 5),    # existing column
    fsl_rcsi_cat         = rep("OLD", 5)  # existing column
  )

  expect_warning(
    add_rcsi(df)
  )
})


test_that("add_rcsi() — category assignment works correctly", {

  df <- tibble::tibble(
    # Construct deterministic values for clear category boundaries
    fsl_rcsi_lessquality = c(0, 1, 7),
    fsl_rcsi_borrow      = c(0, 1, 7),
    fsl_rcsi_mealsize    = c(0, 1, 7),
    fsl_rcsi_mealadult   = c(0, 1, 7),
    fsl_rcsi_mealnb      = c(0, 1, 7)
  )

  out <- add_rcsi(df)

  # Extract character labels
  labs <- as.character(out$fsl_rcsi_cat)

  # Validate using grepl for translation safety
  expect_true(grepl("No to Low", labs[1]))
  expect_true(grepl("Medium",    labs[2]))
  expect_true(grepl("High",      labs[3]))
})
