# Tests for add_fcm_phase

test_that("add_fcm_phase() — basic FCS + rCSI mapping works", {
  df <- tibble::tibble(
    fsl_fcs_cat  = factor(rep(c("Acceptable","Borderline","Poor"), each = 3)),
    fsl_rcsi_cat = factor(rep(c("Low","Medium","High"), times = 3))
  )

  out <- suppressMessages(add_fcm_phase(df))

  # output structure
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))

  # all rows must have cell/cat
  expect_equal(nrow(out), 9)
  expect_false(any(is.na(out$fsl_fc_cell)))

  # fsl_fc_phase must be an ordered factor with levels P5 to P1 (worst to best)
  expect_s3_class(out$fsl_fc_phase, "factor")
  expect_true(is.ordered(out$fsl_fc_phase))
  expect_equal(levels(out$fsl_fc_phase), c("P5", "P4", "P3", "P2", "P1"))

  # fsl_fc_cat must match P1–P4 patterns
  expect_true(all(grepl("^P[1-4]$", out$fsl_fc_phase)))
})


test_that("add_fcm_phase() — FCS + rCSI + HHS (3-indicator matrix) works", {

  df <- tibble::tibble(
    fsl_fcs_cat      = c("Acceptable","Borderline","Poor"),
    fsl_rcsi_cat     = c("Low","Medium","High"),
    fsl_hhs_cat_ipc  = c("None","Little","Moderate")
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)

  # expect P1–P5 only
  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_phase)))
})


test_that("add_fcm_phase() — HDDS + rCSI works when FCS/HHS absent", {

  df <- tibble::tibble(
    fsl_hdds_cat = c("Low","Medium","High"),
    fsl_rcsi_cat = c("Low","Medium","High")
  )

  out <- suppressMessages(add_fcm_phase(df))
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))

  expect_true(all(grepl("^P[1-4]$", out$fsl_fc_phase)))
})


test_that("add_fcm_phase() — FCS + HHS works when rCSI absent", {

  df <- tibble::tibble(
    fsl_fcs_cat     = c("Acceptable","Borderline","Poor"),
    fsl_hhs_cat_ipc = c("None","Little","Severe")
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_phase)))
})


test_that("add_fcm_phase() — HDDS + HHS works when FCS & rCSI missing", {

  df <- tibble::tibble(
    fsl_hdds_cat    = c("High","Medium","Low"),
    fsl_hhs_cat_ipc = c("None","Moderate","Severe")
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_phase)))
})


test_that("add_fcm_phase() — errors on non-dataframe input", {
  expect_error(
    add_fcm_phase(NULL),
    class = "phr_error"
  )
})


test_that("add_fcm_phase() — errors on empty dataset", {
  df <- tibble::tibble()
  expect_error(
    add_fcm_phase(df),
    class = "phr_error"
  )
})


test_that("add_fcm_phase() — invalid category values throw phr_error", {

  df <- tibble::tibble(
    fsl_fcs_cat  = "SomethingWrong",
    fsl_rcsi_cat = "Low"
  )

  expect_warning(
    add_fcm_phase(df)
  )
})


test_that("add_fcm_phase() — no valid combination throws phr_error", {

  df <- tibble::tibble(
    unrelated1 = "foo",
    unrelated2 = "bar"
  )

  expect_error(
    suppressMessages(add_fcm_phase(df)),
    class = "phr_error"
  )
})


test_that("add_fcm_phase() — output categories use P1–P5 patterns", {

  df <- tibble::tibble(
    fsl_fcs_cat  = "Acceptable",
    fsl_rcsi_cat = "Low"
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_true(grepl("^P[1-5]$", out$fsl_fc_phase))
})


test_that("add_fcm_phase() — custom value parameters work correctly", {
  # Test with custom (translated) values that differ from defaults
  df <- tibble::tibble(
    fsl_fcs_cat  = c("Aceptable", "Límite", "Pobre"),
    fsl_rcsi_cat = c("Bajo", "Medio", "Alto")
  )

  out <- suppressMessages(add_fcm_phase(
    df,
    fcs_acceptable_val = "Aceptable",
    fcs_borderline_val = "Límite",
    fcs_poor_val = "Pobre",
    rcsi_low_val = "Bajo",
    rcsi_medium_val = "Medio",
    rcsi_high_val = "Alto"
  ))
  # output structure
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))

  # Verify correct mapping
  expect_equal(out$fsl_fc_cell[1], 1)  # Aceptable + Bajo should map to cell 1
  expect_equal(as.character(out$fsl_fc_phase[1]), "P1")
})


test_that("add_fcm_phase() — custom HHS and HDDS values work", {
  # Test with custom HHS values
  df <- tibble::tibble(
    fsl_hdds_cat    = c("Faible", "Moyen", "Élevé"),
    fsl_hhs_cat_ipc = c("Aucun", "Peu", "Modéré")
  )

  out <- suppressMessages(add_fcm_phase(
    df,
    hdds_low_val = "Faible",
    hdds_medium_val = "Moyen",
    hdds_high_val = "Élevé",
    hhs_none_val = "Aucun",
    hhs_little_val = "Peu",
    hhs_moderate_val = "Modéré",
    hhs_severe_val = "Grave",
    hhs_very_severe_val = "Très Grave"
  ))
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))
})


test_that("add_fcm_phase() — custom value validation works", {
  # Test that validation uses custom values
  df <- tibble::tibble(
    fsl_fcs_cat  = c("Good", "Bad", "Okay"),
    fsl_rcsi_cat = c("Small", "Big", "Huge")
  )

  # This should work with custom values
  out <- suppressMessages(add_fcm_phase(
    df,
    fcs_acceptable_val = "Good",
    fcs_borderline_val = "Okay",
    fcs_poor_val = "Bad",
    rcsi_low_val = "Small",
    rcsi_medium_val = "Big",
    rcsi_high_val = "Huge"
  ))
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))
})


test_that("add_fcm_phase() — factor columns in dataset are preserved as factors after join", {
  df <- tibble::tibble(
    fsl_fcs_cat  = factor(c("Acceptable", "Borderline", "Poor"),
                          levels = c("Poor", "Borderline", "Acceptable")),
    fsl_rcsi_cat = factor(c("Low", "Medium", "High"),
                          levels = c("Low", "Medium", "High"))
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_s3_class(out$fsl_fcs_cat, "factor")
  expect_s3_class(out$fsl_rcsi_cat, "factor")
  expect_equal(levels(out$fsl_fcs_cat), levels(df$fsl_fcs_cat))
  expect_equal(levels(out$fsl_rcsi_cat), levels(df$fsl_rcsi_cat))
})


test_that("add_fcm_phase() — factor levels are preserved when all three indicators used", {
  df <- tibble::tibble(
    fsl_fcs_cat     = factor(c("Acceptable", "Borderline", "Poor"),
                             levels = c("Poor", "Borderline", "Acceptable")),
    fsl_rcsi_cat    = factor(c("Low", "Medium", "High"),
                             levels = c("Low", "Medium", "High")),
    fsl_hhs_cat_ipc = factor(c("None", "Little", "Moderate"),
                             levels = c("None", "Little", "Moderate", "Severe", "Very Severe"))
  )

  out <- suppressMessages(add_fcm_phase(df))

  expect_s3_class(out$fsl_fcs_cat, "factor")
  expect_s3_class(out$fsl_rcsi_cat, "factor")
  expect_s3_class(out$fsl_hhs_cat_ipc, "factor")
  expect_equal(levels(out$fsl_fcs_cat), levels(df$fsl_fcs_cat))
  expect_equal(levels(out$fsl_rcsi_cat), levels(df$fsl_rcsi_cat))
  expect_equal(levels(out$fsl_hhs_cat_ipc), levels(df$fsl_hhs_cat_ipc))
})

