#' Calculate IYCF Indicator 11: Minimum Acceptable Diet (MAD)
#'
#' @description
#' Calculates the Minimum Acceptable Diet indicator for children 6-23 months.
#' A child meets MAD if they meet both MDD and MMF, and for non-breastfed
#' children, also MMFF. This function requires that iycf_mdd_cat, iycf_mmf,
#' and iycf_mmff have already been calculated (e.g., via add_iycf_mdd,
#' add_iycf_mmf, and add_iycf_mmff).
#'
#' @param .dataset Data frame containing pre-calculated IYCF indicators
#' @param age_months Character: Column name for child's age in months
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param iycf_mdd_cat Character: Column name for MDD category (1 = meets MDD)
#' @param iycf_mmf Character: Column name for MMF indicator (1 = meets MMF)
#' @param iycf_mmff Character: Column name for MMFF indicator (1 = meets MMFF)
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_mad (1 = meets MAD, 0 = not, NA = ineligible)
#' @export
add_iycf_mad <- function(.dataset,
                          age_months = "age_months",
                          iycf_4 = "iycf_4",
                          iycf_mdd_cat = "iycf_mdd_cat",
                          iycf_mmf = "iycf_mmf",
                          iycf_mmff = "iycf_mmff",
                          yes_val = "yes",
                          no_val = "no",
                          dnk_val = "dnk") {

  origin <- "add_iycf_mad"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(age_months, iycf_4, iycf_mdd_cat, iycf_mmf, iycf_mmff)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_mad" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_mad already exists and will be overwritten.")
      }

      # Recode iycf_4 (categorical yes/no); other columns are pre-computed numeric indicators
      .dataset[[iycf_4]] <- iycf_recode_yesno(.dataset[[iycf_4]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])
      .dataset[[iycf_mdd_cat]] <- as.numeric(.dataset[[iycf_mdd_cat]])
      .dataset[[iycf_mmf]] <- as.numeric(.dataset[[iycf_mmf]])
      .dataset[[iycf_mmff]] <- as.numeric(.dataset[[iycf_mmff]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_mad = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_mdd_cat]]) |
              is.na(.data[[iycf_mmf]]) | is.na(.data[[iycf_mmff]]) |
              is.na(.data[[iycf_4]]) ~ NA_real_,
            .data[[iycf_mdd_cat]] == 1 & .data[[iycf_mmf]] == 1 &
              (.data[[iycf_4]] == 1 | .data[[iycf_mmff]] == 1) ~ 1,
            .data[[iycf_mdd_cat]] != 1 | .data[[iycf_mmf]] != 1 |
              (.data[[iycf_4]] != 1 & .data[[iycf_mmff]] != 1) ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Minimum Acceptable Diet indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_mdd_cat, iycf_mmf, iycf_mmff, iycf_4, and age_months are present. Run add_iycf_mdd, add_iycf_mmf, and add_iycf_mmff first."
  )
}
