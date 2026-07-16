# Tests for add_ecfies

test_that("add_ecfies() — valid dataset computes score and category", {

  df <- tibble::tibble(
    so1 = c("yes", "no", "yes", "dont_know", "yes"),
    so2 = c("no", "no", "yes", "prefer_not_to_answer", "yes"),
    so3 = c("yes", "yes", "yes", "no", "dont_know"),
    so4 = c("no", "yes", "no", "yes", "yes"),
    so5 = c("no", "no", "yes", "yes", "yes"),
    so6 = c("yes", "no", "dont_know", "prefer_not_to_answer", "yes"),
    so7 = c("no", "yes", "yes", "yes", "no"),
    so8 = c("yes", "yes", "yes", "no", "prefer_not_to_answer")
  )

  out <- add_ecfies(
    .dataset = df,
    nut_ecfies_so1_col = "so1",
    nut_ecfies_so2_col = "so2",
    nut_ecfies_so3_col = "so3",
    nut_ecfies_so4_col = "so4",
    nut_ecfies_so5_col = "so5",
    nut_ecfies_so6_col = "so6",
    nut_ecfies_so7_col = "so7",
    nut_ecfies_so8_col = "so8",
    yes_val = "yes",
    no_val = "no",
    dont_know_val = "dont_know",
    prefer_not_to_answer_val = "prefer_not_to_answer"
  )

  expect_equal(nrow(out), 5)
  expect_true("nut_ecfies_score" %in% names(out))
  expect_true("nut_ecfies_cat" %in% names(out))
  expect_s3_class(out$nut_ecfies_cat, "factor")
})


test_that("add_ecfies() — score calculation is correct", {

  df <- tibble::tibble(
    so1 = c("yes", "no"),
    so2 = c("yes", "no"),
    so3 = c("yes", "no"),
    so4 = c("yes", "no"),
    so5 = c("yes", "no"),
    so6 = c("yes", "no"),
    so7 = c("yes", "no"),
    so8 = c("yes", "no")
  )

  out <- add_ecfies(
    .dataset = df,
    nut_ecfies_so1_col = "so1",
    nut_ecfies_so2_col = "so2",
    nut_ecfies_so3_col = "so3",
    nut_ecfies_so4_col = "so4",
    nut_ecfies_so5_col = "so5",
    nut_ecfies_so6_col = "so6",
    nut_ecfies_so7_col = "so7",
    nut_ecfies_so8_col = "so8",
    yes_val = "yes",
    no_val = "no",
    dont_know_val = "dont_know",
    prefer_not_to_answer_val = "prefer_not_to_answer"
  )

  expect_equal(out$nut_ecfies_score[1], 8)  # all yes
  expect_equal(out$nut_ecfies_score[2], 0)  # all no
})


test_that("add_ecfies() — categorization thresholds work correctly", {

  df <- tibble::tibble(
    so1 = c("no", "yes", "yes", "yes"),
    so2 = c("no", "yes", "yes", "yes"),
    so3 = c("no", "yes", "yes", "yes"),
    so4 = c("no", "no", "yes", "yes"),
    so5 = c("no", "no", "yes", "yes"),
    so6 = c("no", "no", "yes", "yes"),
    so7 = c("no", "no", "no", "yes"),
    so8 = c("no", "no", "no", "yes")
  )

  out <- add_ecfies(
    .dataset = df,
    nut_ecfies_so1_col = "so1",
    nut_ecfies_so2_col = "so2",
    nut_ecfies_so3_col = "so3",
    nut_ecfies_so4_col = "so4",
    nut_ecfies_so5_col = "so5",
    nut_ecfies_so6_col = "so6",
    nut_ecfies_so7_col = "so7",
    nut_ecfies_so8_col = "so8",
    yes_val = "yes",
    no_val = "no",
    dont_know_val = "dont_know",
    prefer_not_to_answer_val = "prefer_not_to_answer"
  )

  expect_true(grepl("No Food Insecurity", out$nut_ecfies_cat[1]))      # score 0
  expect_true(grepl("Mild Food Insecurity", out$nut_ecfies_cat[2]))    # score 3
  expect_true(grepl("Moderate Food Insecurity", out$nut_ecfies_cat[3])) # score 6
  expect_true(grepl("Severe Food Insecurity", out$nut_ecfies_cat[4]))   # score 8
})


test_that("add_ecfies() — error on empty dataset", {

  df_empty <- tibble::tibble(
    so1 = character(0), so2 = character(0), so3 = character(0), so4 = character(0),
    so5 = character(0), so6 = character(0), so7 = character(0), so8 = character(0)
  )

  expect_error(
    add_ecfies(
      .dataset = df_empty,
      nut_ecfies_so1_col = "so1", nut_ecfies_so2_col = "so2",
      nut_ecfies_so3_col = "so3", nut_ecfies_so4_col = "so4",
      nut_ecfies_so5_col = "so5", nut_ecfies_so6_col = "so6",
      nut_ecfies_so7_col = "so7", nut_ecfies_so8_col = "so8",
      yes_val = "yes", no_val = "no",
      dont_know_val = "dont_know", prefer_not_to_answer_val = "prefer_not_to_answer"
    )
  )
})


test_that("add_ecfies() — error on missing columns", {

  df <- tibble::tibble(
    so1 = c("yes", "no"),
    so2 = c("yes", "no")
  )

  expect_error(
    add_ecfies(
      .dataset = df,
      nut_ecfies_so1_col = "so1", nut_ecfies_so2_col = "so2",
      nut_ecfies_so3_col = "so3", nut_ecfies_so4_col = "so4",
      nut_ecfies_so5_col = "so5", nut_ecfies_so6_col = "so6",
      nut_ecfies_so7_col = "so7", nut_ecfies_so8_col = "so8",
      yes_val = "yes", no_val = "no",
      dont_know_val = "dont_know", prefer_not_to_answer_val = "prefer_not_to_answer"
    )
  )
})


test_that("add_ecfies() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    so1 = "yes", so2 = "yes", so3 = "yes", so4 = "yes",
    so5 = "yes", so6 = "yes", so7 = "yes", so8 = "yes",
    nut_ecfies_score = 99
  )

  expect_warning(
    add_ecfies(
      .dataset = df,
      nut_ecfies_so1_col = "so1", nut_ecfies_so2_col = "so2",
      nut_ecfies_so3_col = "so3", nut_ecfies_so4_col = "so4",
      nut_ecfies_so5_col = "so5", nut_ecfies_so6_col = "so6",
      nut_ecfies_so7_col = "so7", nut_ecfies_so8_col = "so8",
      yes_val = "yes", no_val = "no",
      dont_know_val = "dont_know", prefer_not_to_answer_val = "prefer_not_to_answer"
    )
  )
})
