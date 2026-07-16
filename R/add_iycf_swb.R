#' Calculate IYCF Indicator 13: Sweet Beverage Consumption (SWB)
#'
#' @description
#' Calculates whether a child aged 6-23 months consumed any sweet beverages
#' in the previous day.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_6c_swt Character: Column name for sweetened milk (1 = yes)
#' @param iycf_6d_swt Character: Column name for sweetened yogurt drink (1 = yes)
#' @param iycf_6e Character: Column name for fruit juice (1 = yes)
#' @param iycf_6f Character: Column name for soda/sugary drinks (1 = yes)
#' @param iycf_6g Character: Column name for tea/coffee with sugar (1 = yes)
#' @param iycf_6h_swt Character: Column name for other sweetened liquids (1 = yes)
#' @param iycf_6j_swt Character: Column name for other sweetened drinks (1 = yes)
#'
#' @return Data frame with added column: iycf_swb (1 = consumed sweet beverages, 0 = not, NA = ineligible)
#' @export
add_iycf_swb <- function(.dataset,
                          age_months = "age_months",
                          iycf_6c_swt = "iycf_6c_swt",
                          iycf_6d_swt = "iycf_6d_swt",
                          iycf_6e = "iycf_6e",
                          iycf_6f = "iycf_6f",
                          iycf_6g = "iycf_6g",
                          iycf_6h_swt = "iycf_6h_swt",
                          iycf_6j_swt = "iycf_6j_swt") {

  origin <- "add_iycf_swb"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(age_months, iycf_6c_swt, iycf_6d_swt, iycf_6e, iycf_6f,
                         iycf_6g, iycf_6h_swt, iycf_6j_swt)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_swb" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_swb already exists and will be overwritten.")
      }

      # Convert to numeric
      for (v in required_vars) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_swb = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_6c_swt]]) |
              is.na(.data[[iycf_6d_swt]]) | is.na(.data[[iycf_6e]]) |
              is.na(.data[[iycf_6f]]) | is.na(.data[[iycf_6g]]) |
              is.na(.data[[iycf_6h_swt]]) | is.na(.data[[iycf_6j_swt]]) ~ NA_real_,
            .data[[iycf_6c_swt]] == 1 | .data[[iycf_6d_swt]] == 1 |
              .data[[iycf_6e]] == 1 | .data[[iycf_6f]] == 1 |
              .data[[iycf_6g]] == 1 | .data[[iycf_6h_swt]] == 1 |
              .data[[iycf_6j_swt]] == 1 ~ 1,
            .data[[iycf_6c_swt]] != 1 & .data[[iycf_6d_swt]] != 1 &
              .data[[iycf_6e]] != 1 & .data[[iycf_6f]] != 1 &
              .data[[iycf_6g]] != 1 & .data[[iycf_6h_swt]] != 1 &
              .data[[iycf_6j_swt]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Sweet Beverage Consumption indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months and sweet beverage variables are present."
  )
}
