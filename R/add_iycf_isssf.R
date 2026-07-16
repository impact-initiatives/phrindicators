#' Calculate IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods (ISSSF)
#'
#' @description
#' Calculates whether a child aged 6-8 months received any solid, semi-solid,
#' or soft foods in the previous day.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
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
#' @return Data frame with added column: iycf_isssf (1 = received solid foods, 0 = not, NA = ineligible)
#' @export
add_iycf_isssf <- function(.dataset,
                            age_months = "age_months",
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

  origin <- "add_iycf_isssf"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check age_months is present
      phrutils::phr_validate_columns(.dataset, age_months, origin, soft = FALSE)

      # Define all food variables
      food_params <- c(iycf_7a, iycf_7b, iycf_7c, iycf_7d, iycf_7e, iycf_7f,
                       iycf_7g, iycf_7h, iycf_7i, iycf_7j, iycf_7k, iycf_7l,
                       iycf_7m, iycf_7n, iycf_7o, iycf_7p, iycf_7q, iycf_7r)

      # Find which food columns are present
      foods_present <- intersect(food_params, names(.dataset))

      if (length(foods_present) == 0) {
        phrutils::phr_error(
          "Need at least one food variable present to calculate ISSSF.",
          origin = origin
        )
      }

      # Warning for missing variables
      missing_foods <- setdiff(food_params, names(.dataset))
      if (length(missing_foods) > 0) {
        phrutils::phr_warning(
          origin,
          paste0("Dataset does not have all food variables. Missing: ",
                 paste(missing_foods, collapse = ", "),
                 ". Risk of underestimating ISSSF.")
        )
      }

      # Overwrite warning
      if ("iycf_isssf" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_isssf already exists and will be overwritten.")
      }

      # Convert to numeric
      .dataset[[age_months]] <- as.numeric(.dataset[[age_months]])
      for (v in foods_present) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }

      # Count foods consumed (1 = yes)
      .dataset$count_foods_isssf <- apply(.dataset[foods_present], 1, function(x) sum(x == 1, na.rm = TRUE))

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_isssf = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] > 8 | is.na(.data[[age_months]]) ~ NA_real_,
            .data$count_foods_isssf > 0 ~ 1,
            .data$count_foods_isssf == 0 ~ 0
          )
        ) |>
        dplyr::select(-"count_foods_isssf")

      phrutils::phr_message(origin, "IYCF Introduction of Solid/Semi-Solid/Soft Foods indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months and food variables (iycf_7a-iycf_7r) are present."
  )
}
