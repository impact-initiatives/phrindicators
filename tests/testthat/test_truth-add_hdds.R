# Tests for add_hdds() against a specialist-verified reference dataset.
#
# Unlike test-add_hdds.R (synthetic/generated data), this checks the
# function's output against real survey responses and the specialist's own
# computed HDDS score, keyed on uuid. fixtures/truth_hdds.csv is a faithful
# copy of the HDDS sheet in tests/manual_validation/data/test data fsl.xlsx -
# it holds both the raw HDDS component columns (used as add_hdds()'s input)
# and the specialist's own score (the truth we compare against, column
# named "hdds score" in the sheet). See helper-truth-fsl.R for
# fixture/comparison helpers.
#
# Note: the reference sheet only provides a score, not a category, so only
# fsl_hdds_score is checked here.

test_that("add_hdds() matches the specialist's reference HDDS score", {

  fixture <- load_truth_fixture("hdds")

  out <- suppressMessages(suppressWarnings(add_hdds(fixture)))

  mismatches <- compare_to_truth(
    out, fixture,
    c(fsl_hdds_score = "hdds score")
  )

  expect_equal(
    nrow(mismatches), 0,
    info = paste(capture.output(print(mismatches)), collapse = "\n")
  )
})
