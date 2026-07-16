#' Calculate IYCF Indicator 12: Egg and Flesh Food Consumption (EFF)
#'
#' @description
#' Calculates whether a child aged 6-23 months consumed eggs or flesh foods
#' (organ meat, flesh meat, fish, or insects) in the previous day.
#'
#' @param .dataset Data frame containing IYCF component variables
#' @param age_months Character: Column name for child's age in months
#' @param iycf_7i Character: Column name for organ meat (1 = yes)
#' @param iycf_7j Character: Column name for flesh meat (1 = yes)
#' @param iycf_7k Character: Column name for fish/seafood (1 = yes)
#' @param iycf_7l Character: Column name for eggs (1 = yes)
#' @param iycf_7m Character: Column name for insects/grubs (1 = yes)
#'
#' @return Data frame with added column: iycf_eff (1 = consumed eggs/flesh foods, 0 = not, NA = ineligible)
#' @export
add_iycf_eff <- function(.dataset,
                          age_months = "age_months",
                          iycf_7i = "iycf_7i",
                          iycf_7j = "iycf_7j",
                          iycf_7k = "iycf_7k",
                          iycf_7l = "iycf_7l",
                          iycf_7m = "iycf_7m") {

  origin <- "add_iycf_eff"

  phrutils::phr_try(
    expr = {

      # Validate dataset
      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset is empty.")

      # Check required columns
      required_vars <- c(age_months, iycf_7i, iycf_7j, iycf_7k, iycf_7l, iycf_7m)
      phrutils::phr_validate_columns(.dataset, required_vars, origin, soft = FALSE)

      # Overwrite warning
      if ("iycf_eff" %in% names(.dataset)) {
        phrutils::phr_warning(origin, "Variable iycf_eff already exists and will be overwritten.")
      }

      # Convert to numeric
      for (v in required_vars) {
        .dataset[[v]] <- as.numeric(.dataset[[v]])
      }

      # Calculate indicator
      .dataset <- .dataset |>
        dplyr::mutate(
          iycf_eff = dplyr::case_when(
            .data[[age_months]] < 6 | .data[[age_months]] >= 24 | is.na(.data[[age_months]]) ~ NA_real_,
            .data[[iycf_7i]] == 1 | .data[[iycf_7j]] == 1 | .data[[iycf_7k]] == 1 |
              .data[[iycf_7l]] == 1 | .data[[iycf_7m]] == 1 ~ 1,
            .data[[iycf_7i]] != 1 & .data[[iycf_7j]] != 1 & .data[[iycf_7k]] != 1 &
              .data[[iycf_7l]] != 1 & .data[[iycf_7m]] != 1 ~ 0
          )
        )

      phrutils::phr_message(origin, "IYCF Egg and Flesh Food Consumption indicator computed successfully.")
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure age_months, iycf_7i, iycf_7j, iycf_7k, iycf_7l, and iycf_7m are present."
  )
}
