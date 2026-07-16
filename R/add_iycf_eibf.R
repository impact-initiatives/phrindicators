#' Calculate IYCF Indicator 2: Early Initiation of Breastfeeding (EIBF)
#'
#' @description
#' Calculates whether breastfeeding was initiated within one hour of birth.
#' Applicable to children 0-23 months.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_2 Character: Column name for timing of first breastfeed (1 = immediately/within 1 hour, 2 = within 1 hour)
#' @param age_months Character: Column name for child's age in months
#'
#' @return Data frame with added column: iycf_eibf (1 = early initiation, 0 = not, NA = ineligible)
#' @export
add_iycf_eibf <- function(.dataset,
                           iycf_2 = "iycf_2",
                           age_months = "age_months") {

  origin <- "add_iycf_eibf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_2, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_eibf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_eibf already exists and will be overwritten.")
      }

      # Convert to numeric
      .dataset[[iycf_2]] <- as.numeric(.dataset[[iycf_2]])
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_eibf = dplyr::case_when(
            .data[[age_months]] >= 24 | is.na(.data[[age_months]]) | is.na(.data[[iycf_2]]) ~ NA_real_,
            .data[[iycf_2]] == 1 | .data[[iycf_2]] == 2 ~ 1,
            .data[[iycf_2]] != 1 & .data[[iycf_2]] != 2 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Early Initiation of Breastfeeding indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_2 and age_months variables are present and contain valid values."
  )
}
