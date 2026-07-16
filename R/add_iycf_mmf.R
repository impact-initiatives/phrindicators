#' Calculate IYCF Indicator 9: Minimum Meal Frequency (MMF)
#'
#' @description
#' Calculates the Minimum Meal Frequency indicator for children 6-23 months.
#' Criteria differ for breastfed vs non-breastfed children and by age group.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param iycf_6b_num Character: Column name for number of times infant formula was given
#' @param iycf_6c_num Character: Column name for number of times milk was given
#' @param iycf_6d_num Character: Column name for number of times yogurt was given
#' @param iycf_8 Character: Column name for number of times solid/semi-solid foods were given
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_mmf (1 = meets MMF, 0 = not, NA = ineligible)
#' @export
add_iycf_mmf <- function(.dataset,
                          age_months = "age_months",
                          iycf_4 = "iycf_4",
                          iycf_6b_num = "iycf_6b_num",
                          iycf_6c_num = "iycf_6c_num",
                          iycf_6d_num = "iycf_6d_num",
                          iycf_8 = "iycf_8",
                          yes_val = "yes",
                          no_val = "no",
                          dnk_val = "dnk") {

  origin <- "add_iycf_mmf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      mmf_vars <- c(age_months, iycf_4, iycf_6b_num, iycf_6c_num, iycf_6d_num, iycf_8)
      phrutils::phr_validate_columns(.dataset, mmf_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_mmf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_mmf already exists and will be overwritten.")
      }

      # Recode iycf_4 (categorical yes/no) and convert numeric columns
      .dataset[[iycf_4]] <- iycf_recode_yesno(.dataset[[iycf_4]], yes_val, no_val, dnk_val)
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])
      .dataset[[iycf_6b_num]] <- as.numeric(.dataset[[iycf_6b_num]])
      .dataset[[iycf_6c_num]] <- as.numeric(.dataset[[iycf_6c_num]])
      .dataset[[iycf_6d_num]] <- as.numeric(.dataset[[iycf_6d_num]])
      .dataset[[iycf_8]] <- as.numeric(.dataset[[iycf_8]])

      # Calculate indicator components
      .dataset <- .dataset |>
        dplyr::mutate(
          # Breastfed children 6-8 months: >= 2 times solid foods
          mmf_bf_6to8months = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 9 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) | is.na(.data[[iycf_8]]) ~ NA_real_,
            .data[[iycf_4]] == 1 & .data[[iycf_8]] >= 2 ~ 1,
            .data[[iycf_4]] != 1 | .data[[iycf_8]] < 2 ~ 0
          ),
          # Breastfed children 9-23 months: >= 3 times solid foods
          mmf_bf_9to23months = dplyr::case_when(
            .data[[age_months]] < 9 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) | is.na(.data[[iycf_8]]) ~ NA_real_,
            .data[[iycf_4]] == 1 & .data[[iycf_8]] >= 3 ~ 1,
            .data[[iycf_4]] != 1 | .data[[iycf_8]] < 3 ~ 0
          )
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          count_6b_6c_6d_8 = sum(c(.data[[iycf_6b_num]], .data[[iycf_6c_num]],
                                    .data[[iycf_6d_num]], .data[[iycf_8]]), na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          # Non-breastfed children 6-23 months: >= 4 milk feeds + solid foods, with at least 1 solid
          mmf_nonbf_6to23months = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) |
              is.na(.data[[iycf_6b_num]]) | is.na(.data[[iycf_6c_num]]) |
              is.na(.data[[iycf_6d_num]]) | is.na(.data[[iycf_8]]) ~ NA_real_,
            .data[[iycf_4]] != 1 & .data$count_6b_6c_6d_8 >= 4 & .data[[iycf_8]] >= 1 ~ 1,
            .data[[iycf_4]] == 1 | .data$count_6b_6c_6d_8 < 4 | .data[[iycf_8]] < 1 ~ 0
          ),
          # Combined MMF indicator
          iycf_mmf = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) |
              is.na(.data[[iycf_6b_num]]) | is.na(.data[[iycf_6c_num]]) |
              is.na(.data[[iycf_6d_num]]) | is.na(.data[[iycf_8]]) ~ NA_real_,

            dplyr::coalesce(.data$mmf_bf_6to8months, 0) == 1 |
              dplyr::coalesce(.data$mmf_bf_9to23months, 0) == 1 |
              dplyr::coalesce(.data$mmf_nonbf_6to23months, 0) == 1 ~ 1,

            TRUE ~ 0
          )
        ) |>
        dplyr::select(-"mmf_bf_6to8months", -"mmf_bf_9to23months",
                      -"mmf_nonbf_6to23months", -"count_6b_6c_6d_8")

      phrutils::phr_message(origin, "IYCF Minimum Meal Frequency indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months, iycf_4, iycf_6b_num, iycf_6c_num, iycf_6d_num, and iycf_8 are present."
  )
}
