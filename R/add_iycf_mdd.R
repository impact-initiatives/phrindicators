#' Calculate IYCF Indicator 8: Minimum Dietary Diversity (MDD)
#'
#' @description
#' Calculates the Minimum Dietary Diversity indicator for children 6-23 months.
#' MDD is met when a child consumes foods from 5 or more of 8 defined food groups.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param iycf_6b Character: Column name for infant formula
#' @param iycf_6c Character: Column name for milk from animals
#' @param iycf_6d Character: Column name for yogurt drink
#' @param iycf_7a Character: Column name for yogurt food
#' @param iycf_7b Character: Column name for grains/roots/tubers
#' @param iycf_7c Character: Column name for vitamin A rich vegetables
#' @param iycf_7d Character: Column name for white potatoes/tubers
#' @param iycf_7e Character: Column name for dark green leafy vegetables
#' @param iycf_7f Character: Column name for other vegetables
#' @param iycf_7g Character: Column name for vitamin A rich fruits
#' @param iycf_7h Character: Column name for other fruits
#' @param iycf_7i Character: Column name for organ meat
#' @param iycf_7j Character: Column name for flesh meat
#' @param iycf_7k Character: Column name for fish/seafood
#' @param iycf_7l Character: Column name for eggs
#' @param iycf_7m Character: Column name for insects/grubs
#' @param iycf_7n Character: Column name for legumes/nuts/seeds
#' @param iycf_7o Character: Column name for cheese/other dairy
#'
#' @return Data frame with added columns: iycf_mdd_score, iycf_mdd_cat
#' @export
add_iycf_mdd <- function(.dataset,
                          age_months = "age_months",
                          iycf_4 = "iycf_4",
                          iycf_6b = "iycf_6b",
                          iycf_6c = "iycf_6c",
                          iycf_6d = "iycf_6d",
                          iycf_7a = "iycf_7a",
                          iycf_7b = "iycf_7b",
                          iycf_7c = "iycf_7c",
                          iycf_7d = "iycf_7d",
                          iycf_7e = "iycf_7e",
                          iycf_7f = "iycf_7f",
                          iycf_7g = "iycf_7g",
                          iycf_7h = "iycf_7h",
                          iycf_7i = "iycf_7i",
                          iycf_7j = "iycf_7j",
                          iycf_7k = "iycf_7k",
                          iycf_7l = "iycf_7l",
                          iycf_7m = "iycf_7m",
                          iycf_7n = "iycf_7n",
                          iycf_7o = "iycf_7o") {

  origin <- "add_iycf_mdd"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      mdd_vars <- c(age_months, iycf_4, iycf_6b, iycf_6c, iycf_6d,
                    iycf_7a, iycf_7b, iycf_7c, iycf_7d, iycf_7e, iycf_7f,
                    iycf_7g, iycf_7h, iycf_7i, iycf_7j, iycf_7k, iycf_7l,
                    iycf_7m, iycf_7n, iycf_7o)
      phrutils::phr_validate_columns(.dataset, mdd_vars, origin, soft = FALSE)

      # Overwrite warnings
      for (var in c("iycf_mdd_score", "iycf_mdd_cat")) {
        if (var %in% names(.dataset)) {
          phrutils::phr_warning(origin, glue::glue("Variable {var} already exists and will be overwritten."))
        }
      }

      # Convert to numeric
      for (v in mdd_vars) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }

      # Calculate 8 food groups
      .dataset <- .dataset |>
        dplyr::mutate(
          # Group 1: Breastmilk
          mdd1 = dplyr::case_when(
            .data[[iycf_4]] == 1 ~ 1,
            .data[[iycf_4]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 2: Grains, roots, tubers
          mdd2 = dplyr::case_when(
            .data[[iycf_7b]] == 1 | .data[[iycf_7d]] == 1 ~ 1,
            .data[[iycf_7b]] != 1 & .data[[iycf_7d]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 3: Legumes, nuts, seeds
          mdd3 = dplyr::case_when(
            .data[[iycf_7n]] == 1 ~ 1,
            .data[[iycf_7n]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 4: Dairy (formula, milk, yogurt, cheese)
          mdd4 = dplyr::case_when(
            .data[[iycf_6b]] == 1 | .data[[iycf_6c]] == 1 | .data[[iycf_6d]] == 1 |
              .data[[iycf_7a]] == 1 | .data[[iycf_7o]] == 1 ~ 1,
            .data[[iycf_6b]] != 1 & .data[[iycf_6c]] != 1 & .data[[iycf_6d]] != 1 &
              .data[[iycf_7a]] != 1 & .data[[iycf_7o]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 5: Flesh foods (meat, fish, organ meat, insects)
          mdd5 = dplyr::case_when(
            .data[[iycf_7i]] == 1 | .data[[iycf_7j]] == 1 | .data[[iycf_7k]] == 1 | .data[[iycf_7m]] == 1 ~ 1,
            .data[[iycf_7i]] != 1 & .data[[iycf_7j]] != 1 & .data[[iycf_7k]] != 1 & .data[[iycf_7m]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 6: Eggs
          mdd6 = dplyr::case_when(
            .data[[iycf_7l]] == 1 ~ 1,
            .data[[iycf_7l]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 7: Vitamin A rich fruits and vegetables
          mdd7 = dplyr::case_when(
            .data[[iycf_7c]] == 1 | .data[[iycf_7e]] == 1 | .data[[iycf_7g]] == 1 ~ 1,
            .data[[iycf_7c]] != 1 & .data[[iycf_7e]] != 1 & .data[[iycf_7g]] != 1 ~ 0,
            TRUE ~ NA_real_),
          # Group 8: Other fruits and vegetables
          mdd8 = dplyr::case_when(
            .data[[iycf_7f]] == 1 | .data[[iycf_7h]] == 1 ~ 1,
            .data[[iycf_7f]] != 1 & .data[[iycf_7h]] != 1 ~ 0,
            TRUE ~ NA_real_)
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          iycf_mdd_score = sum(c(.data$mdd1, .data$mdd2, .data$mdd3, .data$mdd4,
                                  .data$mdd5, .data$mdd6, .data$mdd7, .data$mdd8), na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          iycf_mdd_cat = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 | is.na(.data$iycf_mdd_score) ~ NA_real_,
            .data[[age_months]] >= 6 & .data[[age_months]] < 24 & .data$iycf_mdd_score >= 5 ~ 1,
            .data[[age_months]] >= 6 & .data[[age_months]] < 24 & .data$iycf_mdd_score < 5 ~ 0
          )
        ) |>
        dplyr::select(-"mdd1", -"mdd2", -"mdd3", -"mdd4", -"mdd5", -"mdd6", -"mdd7", -"mdd8")

      phrutils::phr_message(origin, "IYCF Minimum Dietary Diversity indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure all MDD component variables are present."
  )
}
