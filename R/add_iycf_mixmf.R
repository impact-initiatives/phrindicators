#' Calculate IYCF Indicator 5: Mixed Milk Feeding (MixMF)
#'
#' @description
#' Calculates whether a breastfed child aged 0-5 months is also receiving
#' other milk (formula or animal milk). Applicable to children 0-5 months.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param iycf_6b Character: Column name for infant formula (1 = yes)
#' @param iycf_6c Character: Column name for milk from animals (1 = yes)
#' @param age_months Character: Column name for child's age in months
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_mixmf (1 = mixed milk feeding, 0 = not, NA = ineligible)
#' @export
add_iycf_mixmf <- function(.dataset,
                            iycf_4 = "iycf_4",
                            iycf_6b = "iycf_6b",
                            iycf_6c = "iycf_6c",
                            age_months = "age_months",
                            yes_val = "yes",
                            no_val = "no",
                            dnk_val = "dnk") {

  origin <- "add_iycf_mixmf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(iycf_4, iycf_6b, iycf_6c, age_months)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_mixmf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_mixmf already exists and will be overwritten.")
      }

      # Recode categorical variables to numeric
      .dataset[[iycf_4]] <- iycf_recode_yesno(.dataset[[iycf_4]], yes_val, no_val, dnk_val)
      .dataset[[iycf_6b]] <- iycf_recode_yesno(.dataset[[iycf_6b]], yes_val, no_val, dnk_val)
      .dataset[[iycf_6c]] <- iycf_recode_yesno(.dataset[[iycf_6c]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_mixmf = dplyr::case_when(
            .data[[age_months]] >= 6 | is.na(.data[[age_months]]) |
              is.na(.data[[iycf_4]]) | is.na(.data[[iycf_6b]]) | is.na(.data[[iycf_6c]]) ~ NA_real_,
            .data[[iycf_4]] == 1 & (.data[[iycf_6b]] == 1 | .data[[iycf_6c]] == 1) ~ 1,
            .data[[iycf_4]] != 1 | (.data[[iycf_6b]] != 1 & .data[[iycf_6c]] != 1) ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Mixed Milk Feeding indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_4, iycf_6b, iycf_6c, and age_months variables are present."
  )
}
