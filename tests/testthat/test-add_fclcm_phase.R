# Tests for add_fclcm_phase

test_that("add_fclcm_phase works for standard 4-category LCSI", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3","P4","P5"),
    fsl_lcsi_cat = c("None","Stress","Crisis","Emergency","None")
  )

  out <- suppressMessages(add_fclcm_phase(
    .dataset = df,
    fc_phase_col = "fsl_fc_phase",
    lcsi_col = "fsl_lcsi_cat",
    p1_val = "P1",
    p2_val = "P2",
    p3_val = "P3",
    p4_val = "P4",
    p5_val = "P5",
    lcsi_none_val = "None",
    lcsi_stress_val = "Stress",
    lcsi_crisis_val = "Crisis",
    lcsi_emergency_val = "Emergency"
  ))
  expect_true(all(c("fsl_fclcm_cell", "fsl_fclcm_phase") %in% names(out)))
  expect_equal(nrow(out), 5)

  # fsl_fclcm_phase must be an ordered factor with levels P5 to P1 (worst to best)
  expect_s3_class(out$fsl_fclcm_phase, "factor")
  expect_true(is.ordered(out$fsl_fclcm_phase))
  expect_equal(levels(out$fsl_fclcm_phase), c("P5", "P4", "P3", "P2", "P1"))

  # Spot checks against lookup table
  expect_equal(out$fsl_fclcm_cell[1], 1)    # P1 + None
  expect_equal(as.character(out$fsl_fclcm_phase[1]), "P1")

  expect_equal(out$fsl_fclcm_cell[2], 7)    # P2 + Stress
  expect_equal(as.character(out$fsl_fclcm_phase[2]), "P2")
})


test_that("add_fclcm_phase works for 5-category LCSI (with Exhaustion)", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3","P4","P5"),
    fsl_lcsi_cat = c("Exhaustion","Exhaustion","None","Stress","Emergency")
  )

  out <- suppressMessages(add_fclcm_phase(df))

  expect_true(all(c("fsl_fclcm_cell", "fsl_fclcm_phase") %in% names(out)))
  expect_equal(nrow(out), 5)

  # Exhaustion should trigger the 5-category lookup
  expect_equal(out$fsl_fclcm_cell[1], 21)   # P1 + Exhaustion
  expect_equal(as.character(out$fsl_fclcm_phase[1]), "P1")

  expect_equal(out$fsl_fclcm_cell[2], 22)   # P2 + Exhaustion
})


test_that("add_fclcm_phase errors when fc_phase column is missing", {
  df <- tibble::tibble(
    x = c("P1","P2"),
    fsl_lcsi_cat = c("None","Stress")
  )

  expect_error(add_fclcm_phase(df), regexp = "Missing FC phase column")
})


test_that("add_fclcm_phase errors when lcsi column is missing", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2"),
    x = c("None","Stress")
  )

  expect_error(add_fclcm_phase(df), regexp = "Missing LCSI column")

})


test_that("add_fclcm_phase warns on invalid fc_phase values", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P9"),  # P9 is invalid
    fsl_lcsi_cat = c("None","Stress")
  )

  expect_warning(add_fclcm_phase(df))
})


test_that("add_fclcm_phase warns on invalid lcsi_cat values", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2"),
    fsl_lcsi_cat = c("Invalid","None")
  )

  expect_warning(add_fclcm_phase(df))
})


test_that("add_fclcm_phase works with NAs in lcsi_cat (ignored during lookup)", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3"),
    fsl_lcsi_cat = c("None", NA, "Stress")
  )

  out <- suppressMessages(add_fclcm_phase(df))

  expect_true(all(c("fsl_fclcm_cell","fsl_fclcm_phase") %in% names(out)))

  # First row should match P1 + None
  expect_equal(out$fsl_fclcm_cell[1], 1)

  # Second row should be NA for both fields
  expect_true(is.na(out$fsl_fclcm_cell[2]))
  expect_true(is.na(out$fsl_fclcm_phase[2]))
})


test_that("add_fclcm_phase preserves additional columns", {
  df <- tibble::tibble(
    id = 1:3,
    fsl_fc_phase = c("P1","P2","P3"),
    fsl_lcsi_cat = c("None","Stress","Crisis")
  )

  out <- suppressMessages(add_fclcm_phase(df))

  expect_true("id" %in% names(out))
  expect_equal(out$id, df$id)
})


test_that("add_fclcm_phase fails on non-data-frame input", {
  expect_error(add_fclcm_phase(NULL))
  expect_error(add_fclcm_phase(list()))
})


test_that("add_fclcm_phase() — factor lcsi_col column is preserved as factor after join", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1", "P2", "P3"),
    fsl_lcsi_cat = factor(c("None", "Stress", "Crisis"),
                          levels = c("None", "Stress", "Crisis", "Emergency"))
  )

  out <- suppressMessages(add_fclcm_phase(df))

  expect_s3_class(out$fsl_lcsi_cat, "factor")
  expect_equal(levels(out$fsl_lcsi_cat), levels(df$fsl_lcsi_cat))
})
