# Tests for add_rcsi() against a specialist-verified reference dataset.
#
# Unlike test-add_rcsi.R (synthetic/generated data), this checks the
# function's output against real survey responses and the specialist's own
# computed rCSI results, keyed on uuid. fixtures/truth_rcsi.csv is a
# faithful copy of the rCSI sheet in tests/manual_validation/data/test data
# fsl.xlsx - it holds both the raw rCSI component columns (used as
# add_rcsi()'s input) and the specialist's own fsl_rcsi_score/fsl_rcsi_cat
# (the truth we compare against). See helper-truth-fsl.R for
# fixture/comparison helpers.

test_that("add_rcsi() matches the specialist's reference rCSI scores/categories", {

  fixture <- load_truth_fixture("rcsi")

  out <- suppressMessages(suppressWarnings(add_rcsi(fixture)))

  mismatches <- compare_to_truth(
    out, fixture,
    c(fsl_rcsi_score = "fsl_rcsi_score", fsl_rcsi_cat = "fsl_rcsi_cat")
  )

  expect_equal(
    nrow(mismatches), 0,
    info = paste(capture.output(print(mismatches)), collapse = "\n")
  )
})
