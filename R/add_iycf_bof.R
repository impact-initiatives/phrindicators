#' Calculate IYCF Indicator 16: Bottle Feeding (BoF)
#'
#' @description
#' Calculates whether a child aged 0-23 months was fed using a bottle with
#' a nipple in the previous day.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_5 Character: Column name for bottle feeding question (1 = yes)
#' @param age_months Character: Column name for child's age in months
#'
#' @return Data frame with added column: iycf_bof (1 = bottle fed, 0 = not, NA = ineligible)
#' @export
add_iycf_bof <- function(.dataset,
                          iycf_5 = "iycf_5",
                          age_months = "age_months") {

  origin <- "add_iycf_bof"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_5, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_bof" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_bof already exists and will be overwritten.")
      }

      # Convert to numeric
      .dataset[[iycf_5]] <- as.numeric(.dataset[[iycf_5]])
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_bof = dplyr::case_when(
            .data[[age_months]] >= 24 | is.na(.data[[age_months]]) | is.na(.data[[iycf_5]]) ~ NA_real_,
            .data[[iycf_5]] == 1 ~ 1,
            .data[[iycf_5]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Bottle Feeding indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_5 and age_months variables are present and contain valid values."
  )
}
