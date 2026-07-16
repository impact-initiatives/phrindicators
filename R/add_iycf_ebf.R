#' Calculate IYCF Indicator 4: Exclusive Breastfeeding (EBF)
#'
#' @description
#' Calculates whether a child aged 0-5 months is exclusively breastfed (no other
#' foods or liquids). Checks breastfeeding status, food consumption, and liquid consumption.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param iycf_4 Character: Column name for currently breastfeeding (1 = yes)
#' @param age_months Character: Column name for child's age in months
#' @param iycf_6a Character: Column name for plain water (optional, checked if present)
#' @param iycf_6b Character: Column name for infant formula (optional, checked if present)
#' @param iycf_6c Character: Column name for milk (optional, checked if present)
#' @param iycf_6d Character: Column name for juice (optional, checked if present)
#' @param iycf_6e Character: Column name for clear broth (optional, checked if present)
#' @param iycf_6f Character: Column name for yogurt (optional, checked if present)
#' @param iycf_6g Character: Column name for thin porridge (optional, checked if present)
#' @param iycf_6h Character: Column name for other liquids (optional, checked if present)
#' @param iycf_6i Character: Column name for other liquids 2 (optional, checked if present)
#' @param iycf_6j Character: Column name for other liquids 3 (optional, checked if present)
#' @param iycf_7a Character: Column name for food group a (optional, checked if present)
#' @param iycf_7b Character: Column name for food group b (optional, checked if present)
#' @param iycf_7c Character: Column name for food group c (optional, checked if present)
#' @param iycf_7d Character: Column name for food group d (optional, checked if present)
#' @param iycf_7e Character: Column name for food group e (optional, checked if present)
#' @param iycf_7f Character: Column name for food group f (optional, checked if present)
#' @param iycf_7g Character: Column name for food group g (optional, checked if present)
#' @param iycf_7h Character: Column name for food group h (optional, checked if present)
#' @param iycf_7i Character: Column name for food group i (optional, checked if present)
#' @param iycf_7j Character: Column name for food group j (optional, checked if present)
#' @param iycf_7k Character: Column name for food group k (optional, checked if present)
#' @param iycf_7l Character: Column name for food group l (optional, checked if present)
#' @param iycf_7m Character: Column name for food group m (optional, checked if present)
#' @param iycf_7n Character: Column name for food group n (optional, checked if present)
#' @param iycf_7o Character: Column name for food group o (optional, checked if present)
#' @param iycf_7p Character: Column name for food group p (optional, checked if present)
#' @param iycf_7q Character: Column name for food group q (optional, checked if present)
#' @param iycf_7r Character: Column name for food group r (optional, checked if present)
#'
#' @return Data frame with added column: iycf_ebf (1 = exclusively breastfed, 0 = not, NA = ineligible)
#' @export
add_iycf_ebf <- function(.dataset,
                          iycf_4 = "iycf_4",
                          age_months = "age_months",
                          iycf_6a = "iycf_6a",
                          iycf_6b = "iycf_6b",
                          iycf_6c = "iycf_6c",
                          iycf_6d = "iycf_6d",
                          iycf_6e = "iycf_6e",
                          iycf_6f = "iycf_6f",
                          iycf_6g = "iycf_6g",
                          iycf_6h = "iycf_6h",
                          iycf_6i = "iycf_6i",
                          iycf_6j = "iycf_6j",
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
                          iycf_7o = "iycf_7o",
                          iycf_7p = "iycf_7p",
                          iycf_7q = "iycf_7q",
                          iycf_7r = "iycf_7r") {

  origin <- "add_iycf_ebf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check essential columns
      essential_vars <- c(iycf_4, age_months)
      phrutils::phr_validate_columns(.dataset, essential_vars, origin, soft = FALSE)

      # Define food and liquid variable sets
      ebf_food_params <- c(iycf_7a, iycf_7b, iycf_7c, iycf_7d, iycf_7e, iycf_7f,
                           iycf_7g, iycf_7h, iycf_7i, iycf_7j, iycf_7k, iycf_7l,
                           iycf_7m, iycf_7n, iycf_7o, iycf_7p, iycf_7q, iycf_7r)
      ebf_liquid_params <- c(iycf_6a, iycf_6b, iycf_6c, iycf_6d, iycf_6e, iycf_6f,
                             iycf_6g, iycf_6h, iycf_6i, iycf_6j)

      # Find which food and liquid columns are present
      ebf_foods_present <- intersect(ebf_food_params, names(.dataset))
      ebf_liquids_present <- intersect(ebf_liquid_params, names(.dataset))

      if (length(ebf_foods_present) == 0 || length(ebf_liquids_present) == 0) {
        phrutils::phr_error(
          "Need at least one food variable and one liquid variable present to calculate EBF.",
          origin = origin
        )
      }

      # Warnings for missing variables
      missing_foods <- setdiff(ebf_food_params, names(.dataset))
      if (length(missing_foods) > 0) {
        phrutils::phr_warning(
          origin,
          paste0("Dataset does not have all food variables. Missing: ",
                 paste(missing_foods, collapse = ", "),
                 ". Risk of overestimating EBF.")
        )
      }

      missing_liquids <- setdiff(ebf_liquid_params, names(.dataset))
      if (length(missing_liquids) > 0) {
        phrutils::phr_warning(
          origin,
          paste0("Dataset does not have all liquid variables. Missing: ",
                 paste(missing_liquids, collapse = ", "),
                 ". Risk of overestimating EBF.")
        )
      }

      # Overwrite warning
      if ("iycf_ebf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_ebf already exists and will be overwritten.")
      }

      # Convert to numeric
      all_vars <- c(iycf_4, ebf_foods_present, ebf_liquids_present)
      for (v in all_vars) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])

      # Count foods consumed (value != 2 means consumed)
      num_foods <- length(ebf_foods_present)
      .dataset$count_no_foods <- apply(.dataset[ebf_foods_present], 1, function(x) sum(x == 2, na.rm = TRUE))

      # Count liquids consumed (value != 2 means consumed)
      num_liquids <- length(ebf_liquids_present)
      .dataset$count_no_liquids <- apply(.dataset[ebf_liquids_present], 1, function(x) sum(x == 2, na.rm = TRUE))

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          count_foods = num_foods - .data$count_no_foods,
          count_liquids = num_liquids - .data$count_no_liquids,
          iycf_ebf = dplyr::case_when(
            .data[[age_months]] >= 6 | is.na(.data[[age_months]]) | is.na(.data[[iycf_4]]) ~ NA_real_,
            .data[[iycf_4]] == 1 & .data$count_foods == 0 & .data$count_liquids == 0 ~ 1,
            .data[[iycf_4]] != 1 | .data$count_foods > 0 | .data$count_liquids > 0 ~ 0
          )
        ) |>
        dplyr::select(-"count_no_foods", -"count_no_liquids", -"count_foods", -"count_liquids")

      phrutils::phr_message(origin, "IYCF Exclusive Breastfeeding indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure iycf_4, age_months, and food/liquid variables are present."
  )
}
