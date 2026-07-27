# Tests for add_lcsi() against a specialist-verified reference dataset.
#
# Unlike test-add_lcsi.R (synthetic/generated data), this checks the
# function's output against real survey responses and the specialist's own
# computed LCSI category, keyed on uuid. fixtures/truth_lcsi.csv is a
# faithful copy of the LCS sheet in tests/manual_validation/data/test data
# fsl.xlsx, plus 4 coalesced columns appended at the end - see
# helper-truth-fsl.R for why (the sheet only has those 4 items split into
# _host/_camp versions, but add_lcsi()'s defaults expect one unsplit column
# per item). The fixture holds both the raw LCSI component columns (used as
# add_lcsi()'s input) and the specialist's own fsl_lcsi_cat (the truth we
# compare against).

test_that("add_lcsi() matches the specialist's reference LCSI category", {

  fixture <- load_truth_fixture("lcsi")

  out <- suppressMessages(suppressWarnings(add_lcsi(fixture)))

  mismatches <- compare_to_truth(
    out, fixture,
    c(fsl_lcsi_cat = "fsl_lcsi_cat")
  )

  expect_equal(
    nrow(mismatches), 0,
    info = paste(capture.output(print(mismatches)), collapse = "\n")
  )
})
