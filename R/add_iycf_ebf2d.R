#' Calculate IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth (EBF2D)
#'
#' @description
#' Calculates whether the child was exclusively breastfed during the first two days
#' after birth. Applicable to children 0-23 months.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_3 Character: Column name for prelacteal feeding question (2 = only breastmilk)
#' @param age_months Character: Column name for child's age in months
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_ebf2d (1 = exclusive BF first 2 days, 0 = not, NA = ineligible)
#' @export
add_iycf_ebf2d <- function(.dataset,
                            iycf_3 = "iycf_3",
                            age_months = "age_months",
                            yes_val = "yes",
                            no_val = "no",
                            dnk_val = "dnk") {

  origin <- "add_iycf_ebf2d"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_3, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_ebf2d" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_ebf2d already exists and will be overwritten.")
      }

      # Recode categorical variables to numeric
      .dataset[[iycf_3]] <- iycf_recode_yesno(.dataset[[iycf_3]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_ebf2d = dplyr::case_when(
            .data[[age_months]] >= 24 | is.na(.data[[age_months]]) | is.na(.data[[iycf_3]]) ~ NA_real_,
            .data[[iycf_3]] == 2 ~ 1,
            .data[[iycf_3]] != 2 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Exclusive Breastfeeding First 2 Days indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_3 and age_months variables are present and contain valid values."
  )
}
