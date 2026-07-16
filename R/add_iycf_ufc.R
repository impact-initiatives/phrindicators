#' Calculate IYCF Indicator 14: Unhealthy Food Consumption (UFC)
#'
#' @description
#' Calculates whether a child aged 6-23 months consumed unhealthy foods
#' (sweet foods or fried/salty snacks) in the previous day.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_7p Character: Column name for sweet foods (1 = yes)
#' @param iycf_7q Character: Column name for fried/salty snacks (1 = yes)
#'
#' @return Data frame with added column: iycf_ufc (1 = consumed unhealthy foods, 0 = not, NA = ineligible)
#' @export
add_iycf_ufc <- function(.dataset,
                          age_months = "age_months",
                          iycf_7p = "iycf_7p",
                          iycf_7q = "iycf_7q") {

  origin <- "add_iycf_ufc"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(age_months, iycf_7p, iycf_7q)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_ufc" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_ufc already exists and will be overwritten.")
      }

      # Convert to numeric
      for (v in required_vars) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_ufc = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_7p]]) | is.na(.data[[iycf_7q]]) ~ NA_real_,
            .data[[iycf_7p]] == 1 | .data[[iycf_7q]] == 1 ~ 1,
            .data[[iycf_7p]] != 1 & .data[[iycf_7q]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Unhealthy Food Consumption indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months, iycf_7p, and iycf_7q are present."
  )
}
