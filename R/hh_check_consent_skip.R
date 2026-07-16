#' Enforce consent-based skip logic
#' @param df A data frame containing the consent and skip columns.
#' @param consent_col Character string specifying the column name for consent responses.
#' @param skip_cols Character vector of column names that should be skipped when consent is "no".
hh_check_consent_skip <- function(df, consent_col, skip_cols) {
  phrutils::phr_try({

    if (!consent_col %in% names(df)) {
      phrutils::phr_warning("HouseholdData", glue::glue("Consent column '{consent_col}' not found."))
      return(list())
    }

    issues <- list()
    deny <- which(tolower(df[[consent_col]]) == "no")

    for (col in skip_cols) {
      if (col %in% names(df)) {
        bad <- intersect(deny, which(!is.na(df[[col]]) & df[[col]] != ""))
        if (length(bad) > 0) issues[[col]] <- bad
      }
    }

    if (length(issues) > 0) {
      phrutils::phr_warning(
        "HouseholdData",
        glue::glue("Consent skip-logic violations detected in columns: {paste(names(issues), collapse=', ')}")
      )
    }

    issues

  }, on_error = "warn", origin = "hh_check_consent_skip")
}
