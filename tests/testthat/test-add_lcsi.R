# Tests for add_lcsi

test_that("add_lcsi() — standard use case works", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress2    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress3    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress4    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),

    fsl_lcsi_crisis1    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_crisis2    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_crisis3    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),

    fsl_lcsi_emergency1 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_emergency2 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_emergency3 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE)
  )

  out <- suppressMessages(add_lcsi(df))

  expect_equal(nrow(out), 30)

  # Ensure all derived fields exist
  expect_true(all(c(
    "fsl_lcsi_stress_yes","fsl_lcsi_stress_exhaust","fsl_lcsi_stress",
    "fsl_lcsi_crisis_yes","fsl_lcsi_crisis_exhaust","fsl_lcsi_crisis",
    "fsl_lcsi_emergency_yes","fsl_lcsi_emergency_exhaust","fsl_lcsi_emergency",
    "fsl_lcsi_cat_yes","fsl_lcsi_cat_exhaust","fsl_lcsi_cat"
  ) %in% names(out)))

  # Check categories are translated values using grepl
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat_yes)))
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat_exhaust)))
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat)))

  # Factor levels ordered worst to best
  # Factor levels ordered worst to best (regex/grepl-based checks)
  pat_lcsi <- c(
    "Emergency",
    "Crisis",
    "Stress",
    "None"
  )

  lvls_lcsi <- levels(out$fsl_lcsi_cat)
  expect_true(all(vapply(pat_lcsi, \(p) any(grepl(p, lvls_lcsi)), logical(1))))

  lvls_lcsi_yes <- levels(out$fsl_lcsi_cat_yes)
  expect_true(all(vapply(pat_lcsi, \(p) any(grepl(p, lvls_lcsi_yes)), logical(1))))

  lvls_lcsi_exhaust <- levels(out$fsl_lcsi_cat_exhaust)
  expect_true(all(vapply(pat_lcsi, \(p) any(grepl(p, lvls_lcsi_exhaust)), logical(1))))
})


test_that("add_lcsi() — missing columns cause phr_error", {

  df_missing <- tibble::tibble(
    fsl_lcsi_stress1 = "yes",
    fsl_lcsi_stress2 = "no_had_no_need"
    # missing all others
  )

  expect_error(
    add_lcsi(df_missing),
    class = "phr_error"
  )
})


test_that("add_lcsi() — invalid values cause phr_error", {

  df_bad <- tibble::tibble(
    fsl_lcsi_stress1    = c("yes","INVALID","no_had_no_need"),
    fsl_lcsi_stress2    = "yes",
    fsl_lcsi_stress3    = "yes",
    fsl_lcsi_stress4    = "yes",
    fsl_lcsi_crisis1    = "yes",
    fsl_lcsi_crisis2    = "yes",
    fsl_lcsi_crisis3    = "yes",
    fsl_lcsi_emergency1 = "yes",
    fsl_lcsi_emergency2 = "yes",
    fsl_lcsi_emergency3 = "yes"
  )

  expect_warning(
    add_lcsi(df_bad)
  )
})


test_that("add_lcsi() — NA in any input forces NA in all derived fields", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = c("yes", NA),
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",
    fsl_lcsi_crisis1    = "no_had_no_need",
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",
    fsl_lcsi_emergency1 = "no_had_no_need",
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- suppressMessages(add_lcsi(df))

  # row with NA should have all derived fields NA
  derived_cols <- c(
    "fsl_lcsi_stress_yes","fsl_lcsi_stress_exhaust","fsl_lcsi_stress",
    "fsl_lcsi_crisis_yes","fsl_lcsi_crisis_exhaust","fsl_lcsi_crisis",
    "fsl_lcsi_emergency_yes","fsl_lcsi_emergency_exhaust","fsl_lcsi_emergency",
    "fsl_lcsi_cat_yes","fsl_lcsi_cat_exhaust","fsl_lcsi_cat"
  )

  expect_true(all(is.na(out[2, derived_cols])))
  expect_true(all(!is.na(out[1, derived_cols])))
})


test_that("add_lcsi() — correct category logic for controlled inputs", {

  df <- tibble::tibble(
    # Stress YES
    fsl_lcsi_stress1    = c("yes", "no_had_no_need", "no_had_no_need", "no_had_no_need"),
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",

    # Crisis YES (row 2)
    fsl_lcsi_crisis1    = c("no_had_no_need", "yes", "no_had_no_need", "no_had_no_need"),
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",

    # Emergency YES (row 3)
    fsl_lcsi_emergency1 = c("no_had_no_need", "no_had_no_need", "yes", "no_had_no_need"),
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- suppressMessages(add_lcsi(df))

  expect_true(grepl("Stress",    out$fsl_lcsi_cat[1]))
  expect_true(grepl("Crisis",    out$fsl_lcsi_cat[2]))
  expect_true(grepl("Emergency", out$fsl_lcsi_cat[3]))
  expect_true(grepl("None",      out$fsl_lcsi_cat[4]))
})


test_that("add_lcsi() — exhausted values correctly classify categories", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = "no_exhausted",
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",

    fsl_lcsi_crisis1    = "no_had_no_need",
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",

    fsl_lcsi_emergency1 = "no_had_no_need",
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- suppressMessages(add_lcsi(df))

  expect_true(grepl("Stress", out$fsl_lcsi_cat))
  expect_true(grepl("Stress", out$fsl_lcsi_cat_exhaust))
})
