# Tests for add_hwise

test_that("add_hwise() — valid dataset creates HWISE-4 scores and categories", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "rarely", "sometimes"),
    wash_hwise_plans = c("never", "rarely", "sometimes"),
    wash_hwise_hands = c("never", "rarely", "sometimes"),
    wash_hwise_drink = c("never", "rarely", "sometimes")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink"
  )

  expect_equal(nrow(out), 3)
  expect_true("wash_hwise4_score" %in% names(out))
  expect_true("wash_hwise4_severity_cat" %in% names(out))
  expect_true("wash_hwise4_cat" %in% names(out))
})


test_that("add_hwise() — HWISE-4 score calculation is correct", {

  df <- tibble::tibble(
    wash_hwise_worry = c("always", "never"),
    wash_hwise_plans = c("always", "never"),
    wash_hwise_hands = c("always", "never"),
    wash_hwise_drink = c("always", "never")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink",
    never_val = "never",
    rarely_val = "rarely",
    sometimes_val = "sometimes",
    often_val = "often",
    always_val = "always"
  )

  expect_equal(out$wash_hwise4_score[1], 12)  # 4 * 3
  expect_equal(out$wash_hwise4_score[2], 0)
})


test_that("add_hwise() — HWISE-4 severity categories are correct", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "sometimes", "sometimes", "often", "always"),
    wash_hwise_plans = c("rarely", "never", "often", "sometimes", "always"),
    wash_hwise_hands = c("never", "sometimes", "sometimes", "always", "always"),
    wash_hwise_drink = c("never", "never", "never", "sometimes", "always")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink"
  )

  expect_true(grepl("No-to-marginal", out$wash_hwise4_severity_cat[1]))
  expect_true(grepl("Low", out$wash_hwise4_severity_cat[2]))
  expect_true(grepl("Moderate", out$wash_hwise4_severity_cat[3]))
  expect_true(grepl("High", out$wash_hwise4_severity_cat[4]))
  expect_true(grepl("Very High", out$wash_hwise4_severity_cat[5]))
})


test_that("add_hwise() — error on empty dataset", {

  df_empty <- tibble::tibble(
    wash_hwise_worry = character(0),
    wash_hwise_plans = character(0),
    wash_hwise_hands = character(0),
    wash_hwise_drink = character(0)
  )

  expect_error(
    add_hwise(
      .dataset = df_empty,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})


test_that("add_hwise() — error on missing columns", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "rarely")
  )

  expect_error(
    add_hwise(
      .dataset = df,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})


test_that("add_hwise() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never"),
    wash_hwise_plans = c("never"),
    wash_hwise_hands = c("never"),
    wash_hwise_drink = c("never"),
    wash_hwise4_score = 99
  )

  expect_warning(
    add_hwise(
      .dataset = df,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})
