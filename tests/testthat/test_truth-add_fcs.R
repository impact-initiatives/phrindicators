# Tests for add_fcs() against a specialist-verified reference dataset.
#
# Unlike test-add_fcs.R (synthetic/generated data), this checks the function's
# output against real survey responses and the specialist's own computed FCS
# results, keyed on uuid. fixtures/truth_fcs.csv is a faithful copy of the
# FCS sheet in tests/manual_validation/data/test data fsl.xlsx - it holds
# both the raw FCS component columns (used as add_fcs()'s input) and the
# specialist's own fsl_fcs_score/fsl_fcs_cat (the truth we compare against).
# See helper-truth-fsl.R for fixture/comparison helpers.

test_that("add_fcs() matches the specialist's reference FCS scores/categories", {

  fixture <- load_truth_fixture("fcs")

  out <- suppressMessages(suppressWarnings(add_fcs(fixture)))

  mismatches <- compare_to_truth(
    out, fixture,
    c(fsl_fcs_score = "fsl_fcs_score", fsl_fcs_cat = "fsl_fcs_cat")
  )

  expect_equal(
    nrow(mismatches), 0,
    info = paste(capture.output(print(mismatches)), collapse = "\n")
  )
})
