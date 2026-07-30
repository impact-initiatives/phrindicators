# Tests for add_hhs() against a specialist-verified reference dataset.
#
# Unlike test-add_hhs.R (synthetic/generated data), this checks the
# function's output against real survey responses and the specialist's own
# computed HHS categories, keyed on uuid. fixtures/truth_hhs.csv is a
# faithful copy of the HHS sheet in tests/manual_validation/data/test data
# fsl.xlsx - it holds both the raw HHS component columns (used as
# add_hhs()'s input) and the specialist's own fsl_hhs_cat_ipc/fsl_hhs_cat
# (the truth we compare against). See helper-truth-fsl.R for
# fixture/comparison helpers.

test_that("add_hhs() matches the specialist's reference HHS categories", {

  fixture <- load_truth_fixture("hhs")

  out <- suppressMessages(suppressWarnings(add_hhs(fixture)))

  mismatches <- compare_to_truth(
    out, fixture,
    c(fsl_hhs_cat_ipc = "fsl_hhs_cat_ipc", fsl_hhs_cat = "fsl_hhs_cat")
  )

  expect_equal(
    nrow(mismatches), 0,
    info = paste(capture.output(print(mismatches)), collapse = "\n")
  )
})
