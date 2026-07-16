#' Calculate IYCF Indicator 6: Continued Breastfeeding 12-23 Months (CBF)
#'
#' @description
#' Calculates whether a child aged 12-23 months is still being breastfed.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param age_months Character: Column name for child's age in months
#'
#' @return Data frame with added column: iycf_cbf (1 = continued breastfeeding, 0 = not, NA = ineligible)
#' @export
add_iycf_cbf <- function(.dataset,
                          iycf_4 = "iycf_4",
                          age_months = "age_months") {

  origin <- "add_iycf_cbf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_4, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_cbf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_cbf already exists and will be overwritten.")
      }

      # Convert to numeric
      .dataset[[iycf_4]] <- as.numeric(.dataset[[iycf_4]])
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_cbf = dplyr::case_when(
            .data[[age_months]] < 12 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) ~ NA_real_,
            .data[[iycf_4]] == 1 ~ 1,
            .data[[iycf_4]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Continued Breastfeeding indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_4 and age_months variables are present and contain valid values."
  )
}
