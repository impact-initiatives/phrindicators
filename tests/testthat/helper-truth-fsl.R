# Shared helpers for the "truth" tests (test_truth-*.R), which check
# phrindicators' FSL add_* functions against a specialist-verified reference
# dataset instead of synthetic data.
#
# Fixtures live in tests/testthat/fixtures/truth_*.csv - each one is a
# faithful, complete copy of one sheet from
# tests/manual_validation/data/test data fsl.xlsx (FCS/HDDS/rCSI/HHS/LCS),
# so each file holds BOTH the raw component columns needed to call the
# add_*() function AND the specialist's own computed score/category to
# compare against - no separate "raw inputs" file. All keyed on `uuid`.
#
# Exception: truth_lcsi.csv (from the LCS sheet) additionally carries
# coalesced fsl_lcsi_stress1/stress2/emergency2/emergency3 columns appended
# after the original sheet columns - the sheet only has those 4 items split
# into _host/_camp versions (different question wording per population
# group), but add_lcsi()'s defaults expect one unsplit column per item.
# Same fix as config_fsl.yml's add_lcsi block; see git history for why.

load_truth_fixture <- function(name) {
  utils::read.csv(
    test_path("fixtures", paste0("truth_", name, ".csv")),
    stringsAsFactors = FALSE, na.strings = c("NA", ""), check.names = FALSE
  )
}

#' Compare computed indicator columns against specialist reference values
#'
#' @param computed Data frame with a `uuid` column and the columns being tested.
#' @param truth Data frame with a `uuid` column and the reference columns.
#' @param col_map Named character vector: name = column in `computed`,
#'   value = matching column in `truth` (names can differ, e.g. HDDS's score
#'   column is called "hdds score" in the reference sheet).
#' @return A data frame of mismatching rows (0 rows if everything matches).
compare_to_truth <- function(computed, truth, col_map) {
  computed_cols <- names(col_map)
  truth_select <- truth[, c("uuid", unname(col_map)), drop = FALSE]
  names(truth_select) <- c("uuid", paste0("truth__", computed_cols))

  merged <- merge(
    computed[, c("uuid", computed_cols), drop = FALSE],
    truth_select,
    by = "uuid", all = TRUE
  )

  out <- list()
  for (col in computed_cols) {
    truth_col <- paste0("truth__", col)
    a <- as.character(merged[[col]])
    b <- as.character(merged[[truth_col]])

    same <- mapply(function(x, y) {
      if (is.na(x) && is.na(y)) return(TRUE)
      if (is.na(x) || is.na(y)) return(FALSE)
      xn <- suppressWarnings(as.numeric(x))
      yn <- suppressWarnings(as.numeric(y))
      if (!is.na(xn) && !is.na(yn)) return(isTRUE(all.equal(xn, yn, tolerance = 1e-6)))
      identical(trimws(x), trimws(y))
    }, a, b)

    if (any(!same)) {
      out[[col]] <- data.frame(
        column = col,
        uuid = merged$uuid[!same],
        computed = a[!same],
        truth = b[!same],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      column = character(), uuid = character(),
      computed = character(), truth = character()
    ))
  }
  do.call(rbind, out)
}
