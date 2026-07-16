#' Calculate IYCF Indicator 15: Zero Vegetable or Fruit Consumption (ZVF)
#'
#' @description
#' Calculates whether a child aged 6-23 months consumed zero vegetables or
#' fruits in the previous day. A child is flagged if none of the five
#' vegetable/fruit food groups were consumed.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_7c Character: Column name for vitamin A rich vegetables (1 = yes, 2 = no)
#' @param iycf_7e Character: Column name for dark green leafy vegetables (1 = yes, 2 = no)
#' @param iycf_7f Character: Column name for other vegetables (1 = yes, 2 = no)
#' @param iycf_7g Character: Column name for vitamin A rich fruits (1 = yes, 2 = no)
#' @param iycf_7h Character: Column name for other fruits (1 = yes, 2 = no)
#' @param yes_val Character: Value representing "yes" in categorical columns
#' @param no_val Character: Value representing "no" in categorical columns
#' @param dnk_val Character: Value representing "don't know" in categorical columns (recoded to NA)
#'
#' @return Data frame with added column: iycf_zvf (1 = zero veg/fruit, 0 = consumed veg/fruit, NA = ineligible)
#' @export
add_iycf_zvf <- function(.dataset,
                          age_months = "age_months",
                          iycf_7c = "iycf_7c",
                          iycf_7e = "iycf_7e",
                          iycf_7f = "iycf_7f",
                          iycf_7g = "iycf_7g",
                          iycf_7h = "iycf_7h",
                          yes_val = "yes",
                          no_val = "no",
                          dnk_val = "dnk") {

  origin <- "add_iycf_zvf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(age_months, iycf_7c, iycf_7e, iycf_7f, iycf_7g, iycf_7h)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_zvf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_zvf already exists and will be overwritten.")
      }

      # Recode categorical variables to numeric
      categorical_vars <- c(iycf_7c, iycf_7e, iycf_7f, iycf_7g, iycf_7h)
      for (v in categorical_vars) {
        .dataset[[v]] <- iycf_recode_yesno(.dataset[[v]], yes_val, no_val, dnk_val)
      }
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Calculate indicator - zvf components check if each food was NOT consumed (value == 2)
      .dataset <- .dataset |>
        dplyr::mutate(
          zvf1 = dplyr::case_when(.data[[iycf_7c]] == 2 ~ 1, .data[[iycf_7c]] != 2 ~ 0, TRUE ~ NA_real_),
          zvf2 = dplyr::case_when(.data[[iycf_7e]] == 2 ~ 1, .data[[iycf_7e]] != 2 ~ 0, TRUE ~ NA_real_),
          zvf3 = dplyr::case_when(.data[[iycf_7f]] == 2 ~ 1, .data[[iycf_7f]] != 2 ~ 0, TRUE ~ NA_real_),
          zvf4 = dplyr::case_when(.data[[iycf_7g]] == 2 ~ 1, .data[[iycf_7g]] != 2 ~ 0, TRUE ~ NA_real_),
          zvf5 = dplyr::case_when(.data[[iycf_7h]] == 2 ~ 1, .data[[iycf_7h]] != 2 ~ 0, TRUE ~ NA_real_)
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          zvf_sum = sum(c(.data$zvf1, .data$zvf2, .data$zvf3, .data$zvf4, .data$zvf5), na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          iycf_zvf = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 |
              is.na(.data[[age_months]]) | is.na(.data$zvf1) | is.na(.data$zvf2) |
              is.na(.data$zvf3) | is.na(.data$zvf4) | is.na(.data$zvf5) ~ NA_real_,
            .data$zvf_sum == 5 ~ 1,
            .data$zvf_sum < 5 ~ 0
          )
        ) |>
        dplyr::select(-"zvf1", -"zvf2", -"zvf3", -"zvf4", -"zvf5", -"zvf_sum")

      phrutils::phr_message(origin, "IYCF Zero Vegetable or Fruit Consumption indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months, iycf_7c, iycf_7e, iycf_7f, iycf_7g, and iycf_7h are present."
  )
}
