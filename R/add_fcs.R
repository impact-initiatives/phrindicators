#' Calculate Food Consumption Score (FCS)
#'
#' @description
#' Calculates the FCS score and category based on weighted food group frequencies.
#' Applies standard WFP methodology with configurable cutoffs.
#'
#' @param .dataset Data frame containing FCS component variables
#' @param cutoffs Character: "normal" or "alternative" FCS categorization cutoffs
#' @param fsl_fcs_cereal Character: Column name for cereals frequency (0-7 days)
#' @param fsl_fcs_legumes Character: Column name for legumes frequency (0-7 days)
#' @param fsl_fcs_veg Character: Column name for vegetables frequency (0-7 days)
#' @param fsl_fcs_fruit Character: Column name for fruit frequency (0-7 days)
#' @param fsl_fcs_meat Character: Column name for meat frequency (0-7 days)
#' @param fsl_fcs_dairy Character: Column name for dairy frequency (0-7 days)
#' @param fsl_fcs_sugar Character: Column name for sugar frequency (0-7 days)
#' @param fsl_fcs_oil Character: Column name for oil frequency (0-7 days)
#'
#' @return Data frame with added columns: fsl_fcs_score, fsl_fcs_cat
#' @export
add_fcs <- function(.dataset,
                    cutoffs = "normal",
                    fsl_fcs_cereal = "fsl_fcs_cereal",
                    fsl_fcs_legumes = "fsl_fcs_legumes",
                    fsl_fcs_veg = "fsl_fcs_veg",
                    fsl_fcs_fruit = "fsl_fcs_fruit",
                    fsl_fcs_meat = "fsl_fcs_meat",
                    fsl_fcs_dairy = "fsl_fcs_dairy",
                    fsl_fcs_sugar = "fsl_fcs_sugar",
                    fsl_fcs_oil = "fsl_fcs_oil") {

  origin <- "add_fcs"

  phrutils::phr_try(
    expr = {


      # Basic dataset validation

      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)

      phrutils::phr_assert(
        nrow(.dataset) > 0,
        origin,
        "Dataset is empty."
      )

      fcs_vars <- c(
        fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat,
        fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar
      )

      phrutils::phr_validate_columns(
        .dataset,
        fcs_vars,
        origin,
        hint = "Ensure all required FCS component variables are present.",
        soft = FALSE
      )


      # Overwrite warnings

      if ("fsl_fcs_score" %in% names(.dataset)) {
        var <- "fsl_fcs_score"
        phrutils::phr_warning(
          origin,
          glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }

      if ("fsl_fcs_cat" %in% names(.dataset)) {
        var <- "fsl_fcs_cat"
        phrutils::phr_warning(
          origin,
          glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }


      # Validation (warning only - do NOT modify original values)

      for (v in fcs_vars) {

        # must be numeric
        phrutils::phr_validate_all_numeric(.dataset[[v]], origin, soft = TRUE)

        # call validator (warning only)
        phrutils::phr_validate_range(
          df = .dataset,
          col = v,
          min = 0,
          max = 7,
          soft = TRUE,
          origin = origin,
          hint = "FCS frequency must be between 0 and 7."
        )
      }


      # Compute weighted components
      # Out-of-range values are treated as NA during calculation ONLY
      # Original values are preserved for quality checks

      .dataset <- .dataset |>
        dplyr::mutate(dplyr::across(dplyr::all_of(fcs_vars), as.numeric)) |>
        dplyr::mutate(
          # Create "safe" versions that treat out-of-range as NA for calculation
          # but don't modify the original columns
          fcs_weight_cereal1 = ifelse(.data[[fsl_fcs_cereal]] < 0 | .data[[fsl_fcs_cereal]] > 7, NA, .data[[fsl_fcs_cereal]]) * 2,
          fcs_weight_legume2 = ifelse(.data[[fsl_fcs_legumes]] < 0 | .data[[fsl_fcs_legumes]] > 7, NA, .data[[fsl_fcs_legumes]]) * 3,
          fcs_weight_dairy3  = ifelse(.data[[fsl_fcs_dairy]] < 0 | .data[[fsl_fcs_dairy]] > 7, NA, .data[[fsl_fcs_dairy]]) * 4,
          fcs_weight_meat4   = ifelse(.data[[fsl_fcs_meat]] < 0 | .data[[fsl_fcs_meat]] > 7, NA, .data[[fsl_fcs_meat]]) * 4,
          fcs_weight_veg5    = ifelse(.data[[fsl_fcs_veg]] < 0 | .data[[fsl_fcs_veg]] > 7, NA, .data[[fsl_fcs_veg]]) * 1,
          fcs_weight_fruit6  = ifelse(.data[[fsl_fcs_fruit]] < 0 | .data[[fsl_fcs_fruit]] > 7, NA, .data[[fsl_fcs_fruit]]) * 1,
          fcs_weight_oil7    = ifelse(.data[[fsl_fcs_oil]] < 0 | .data[[fsl_fcs_oil]] > 7, NA, .data[[fsl_fcs_oil]]) * 0.5,
          fcs_weight_sugar8  = ifelse(.data[[fsl_fcs_sugar]] < 0 | .data[[fsl_fcs_sugar]] > 7, NA, .data[[fsl_fcs_sugar]]) * 0.5
        ) |>
        dplyr::mutate(
          fsl_fcs_score = rowSums(
            dplyr::across(c(
              fcs_weight_cereal1, fcs_weight_legume2, fcs_weight_dairy3,
              fcs_weight_meat4, fcs_weight_veg5, fcs_weight_fruit6,
              fcs_weight_oil7, fcs_weight_sugar8
            )),
            na.rm = FALSE
          ),
          # Calculate standard deviation of days across all food groups
          sd_foods = apply(
            dplyr::across(c(
              fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat,
              fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar
            )), 1, stats::sd, na.rm = TRUE
          )
        )


      # Handle cutoffs

      if (!cutoffs %in% c("normal", "alternative")) {
        phrutils::phr_warning(
          origin,
          "Invalid cutoff type '{cutoffs}'. Defaulting to 'normal' thresholds."
        )
        cutoffs <- "normal"
      }


      # Categorization

      if (cutoffs == "normal") {

        .dataset <- .dataset |>
          dplyr::mutate(
            fsl_fcs_cat = dplyr::case_when(
              fsl_fcs_score < 21.5 ~ "Poor",
              fsl_fcs_score <= 35  ~ "Borderline",
              fsl_fcs_score > 35   ~ "Acceptable",
              TRUE ~ NA_character_
            )
          )

      } else {

        .dataset <- .dataset |>
          dplyr::mutate(
            fsl_fcs_cat = dplyr::case_when(
              fsl_fcs_score <= 28 ~ "Poor",
              fsl_fcs_score <= 42 ~ "Borderline",
              fsl_fcs_score > 42  ~ "Acceptable",
              TRUE ~ NA_character_
            )
          )
      }

      # factorization
      .dataset <- .dataset |>
        dplyr::mutate(
          fsl_fcs_cat = factor(
            fsl_fcs_cat,
            levels = c(
              "Poor",
              "Borderline",
              "Acceptable"
            )
          )
        )


      # Completion

      phrutils::phr_message(origin, "FCS score and category computed successfully.")

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure FCS component variables contain numeric values 0\u20137."
  )
}
