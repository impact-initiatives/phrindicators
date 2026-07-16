#' Calculate IYCF Indicator 10: Minimum Milk Feeding Frequency for Non-Breastfed Children (MMFF)
#'
#' @description
#' Calculates whether a non-breastfed child aged 6-23 months receives at least
#' 2 milk feeds per day (formula, animal milk, yogurt, or dairy products).
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param iycf_6b_num Character: Column name for number of times infant formula was given
#' @param iycf_6c_num Character: Column name for number of times milk was given
#' @param iycf_6d_num Character: Column name for number of times yogurt drink was given
#' @param iycf_7a_num Character: Column name for number of times yogurt food was given
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_mmff (1 = meets MMFF, 0 = not, NA = ineligible)
#' @export
add_iycf_mmff <- function(.dataset,
                           age_months = "age_months",
                           iycf_4 = "iycf_4",
                           iycf_6b_num = "iycf_6b_num",
                           iycf_6c_num = "iycf_6c_num",
                           iycf_6d_num = "iycf_6d_num",
                           iycf_7a_num = "iycf_7a_num",
                           yes_val = "yes",
                           no_val = "no",
                           dnk_val = "dnk") {

  origin <- "add_iycf_mmff"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      mmff_vars <- c(age_months, iycf_4, iycf_6b_num, iycf_6c_num, iycf_6d_num, iycf_7a_num)
      phrutils::phr_validate_columns(.dataset, mmff_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_mmff" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_mmff already exists and will be overwritten.")
      }

      # Recode iycf_4 (categorical yes/no) and convert numeric columns
      .dataset[[iycf_4]] <- iycf_recode_yesno(.dataset[[iycf_4]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])
      .dataset[[iycf_6b_num]] <- as.numeric(.dataset[[iycf_6b_num]])
      .dataset[[iycf_6c_num]] <- as.numeric(.dataset[[iycf_6c_num]])
      .dataset[[iycf_6d_num]] <- as.numeric(.dataset[[iycf_6d_num]])
      .dataset[[iycf_7a_num]] <- as.numeric(.dataset[[iycf_7a_num]])

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::rowwise() |>
        dplyr::mutate(
          count_dairy = sum(c(.data[[iycf_6b_num]], .data[[iycf_6c_num]],
                              .data[[iycf_6d_num]], .data[[iycf_7a_num]]), na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          iycf_mmff = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_6b_num]]) |
              is.na(.data[[iycf_6c_num]]) | is.na(.data[[iycf_6d_num]]) |
              is.na(.data[[iycf_7a_num]]) ~ NA_real_,
            .data[[iycf_4]] != 1 & .data$count_dairy >= 2 ~ 1,
            .data[[iycf_4]] == 1 | .data$count_dairy < 2 ~ 0
          )
        ) |>
        dplyr::select(-"count_dairy")

      phrutils::phr_message(origin, "IYCF Minimum Milk Feeding Frequency indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months, iycf_4, iycf_6b_num, iycf_6c_num, iycf_6d_num, and iycf_7a_num are present."
  )
}
