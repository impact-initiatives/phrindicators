#' Calculate IYCF Indicator 1: Ever Breastfed (EvBF)
#'
#' @description
#' Calculates whether a child was ever breastfed. Applicable to children 0-23 months.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_1 Character: Column name for ever breastfed question (1 = yes)
#' @param age_months Character: Column name for child's age in months
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_evbf (1 = ever breastfed, 0 = not, NA = ineligible)
#' @export
add_iycf_evbf <- function(.dataset,
                           iycf_1 = "iycf_1",
                           age_months = "age_months",
                           yes_val = "yes",
                           no_val = "no",
                           dnk_val = "dnk") {

  origin <- "add_iycf_evbf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_1, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_evbf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_evbf already exists and will be overwritten.")
      }

      # Recode categorical variables to numeric
      .dataset[[iycf_1]] <- iycf_recode_yesno(.dataset[[iycf_1]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_evbf = dplyr::case_when(
            .data[[age_months]] >= 24 | is.na(.data[[age_months]]) | is.na(.data[[iycf_1]]) ~ NA_real_,
            .data[[iycf_1]] == 1 ~ 1,
            .data[[iycf_1]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Ever Breastfed indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_1 and age_months variables are present and contain valid values."
  )
}
