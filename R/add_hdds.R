#' Calculate Household Dietary Diversity Score (HDDS)
#'
#' @description
#' Calculates HDDS count and category based on yes/no responses to 12 food
#' group consumption questions.
#'
#' @param .dataset Data frame containing HDDS component variables
#' @param fsl_hdds_cereals Character: Column for cereals consumption (yes/no)
#' @param fsl_hdds_tubers Character: Column for tubers consumption (yes/no)
#' @param fsl_hdds_veg Character: Column for vegetables consumption (yes/no)
#' @param fsl_hdds_fruit Character: Column for fruit consumption (yes/no)
#' @param fsl_hdds_meat Character: Column for meat consumption (yes/no)
#' @param fsl_hdds_eggs Character: Column for eggs consumption (yes/no)
#' @param fsl_hdds_fish Character: Column for fish consumption (yes/no)
#' @param fsl_hdds_legumes Character: Column for legumes consumption (yes/no)
#' @param fsl_hdds_dairy Character: Column for dairy consumption (yes/no)
#' @param fsl_hdds_oil Character: Column for oil consumption (yes/no)
#' @param fsl_hdds_sugar Character: Column for sugar consumption (yes/no)
#' @param fsl_hdds_condiments Character: Column for condiments consumption (yes/no)
#' @param yes_val Character: Value representing "yes" response
#' @param no_val Character: Value representing "no" response
#'
#' @return Data frame with added columns: fsl_hdds_count, fsl_hdds_cat
#' @export
add_hdds <- function(.dataset,
                     fsl_hdds_cereals = "fsl_hdds_cereals",
                     fsl_hdds_tubers = "fsl_hdds_tubers",
                     fsl_hdds_veg = "fsl_hdds_veg",
                     fsl_hdds_fruit = "fsl_hdds_fruit",
                     fsl_hdds_meat = "fsl_hdds_meat",
                     fsl_hdds_eggs = "fsl_hdds_eggs",
                     fsl_hdds_fish = "fsl_hdds_fish",
                     fsl_hdds_legumes = "fsl_hdds_legumes",
                     fsl_hdds_dairy = "fsl_hdds_dairy",
                     fsl_hdds_oil = "fsl_hdds_oil",
                     fsl_hdds_sugar = "fsl_hdds_sugar",
                     fsl_hdds_condiments = "fsl_hdds_condiments",
                     yes_val = "yes",
                     no_val = "no") {

  origin <- "add_hdds"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    yes_val <- phrutils::ensure_value(yes_val, "yes")
    no_val <- phrutils::ensure_value(no_val, "no")


    # Validate dataset

    phrutils::phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = "Confirm that you passed a valid data.frame from your cleaning pipeline.",
      soft = FALSE
    )

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin,
      "Dataset is empty.",
      hint = "Check that your input data has rows after filtering or import."
    )




    # Required columns

    hdds_vars <- c(
      fsl_hdds_cereals, fsl_hdds_tubers, fsl_hdds_veg, fsl_hdds_fruit,
      fsl_hdds_meat, fsl_hdds_eggs, fsl_hdds_fish, fsl_hdds_legumes,
      fsl_hdds_dairy, fsl_hdds_oil, fsl_hdds_sugar, fsl_hdds_condiments
    )

    phrutils::phr_validate_columns(
      .dataset,
      hdds_vars,
      origin = origin,
      hint = "Check that all 12 HDDS food group variables are present and correctly named.",
      soft = FALSE
    )


    # Validate text choices

    allowed <- c(yes_val, no_val)

    for (v in hdds_vars) {
      # Allow NA as valid input
      phrutils::phr_validate_choice(
        x       = .dataset[[v]],
        choices = c(allowed, NA_character_),
        origin  = origin,
        soft    = FALSE
      )
    }


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_hdds_score",
      "fsl_hdds_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }
    }


    # Compute HDDS Score
    # Treat invalid values (not yes/no) as NA
    # Apply strict NA rule: if any HDDS variable is NA or invalid \u2192 score = NA

    .dataset <- .dataset |>
      dplyr::rowwise() |>
      dplyr::mutate(
        fsl_hdds_score = {
          # Check for any NA or invalid values
          hdds_values <- dplyr::c_across(all_of(hdds_vars))
          has_na <- any(is.na(hdds_values))
          has_invalid <- any(!hdds_values %in% c(yes_val, no_val, NA_character_))

          if (has_na || has_invalid) {
            NA_real_
          } else {
            sum(hdds_values == yes_val)
          }
        },

        fsl_hdds_cat = dplyr::case_when(
          is.na(fsl_hdds_score) ~ NA_character_,
          fsl_hdds_score <= 2 ~ "Low",
          fsl_hdds_score <= 4 ~ "Medium",
          fsl_hdds_score > 4 ~ "High"
        )
      ) |>
      dplyr::ungroup()


    # Standardize factor levels

    .dataset <- .dataset |>
      dplyr::mutate(
        fsl_hdds_cat = factor(
          fsl_hdds_cat,
          levels = c(
            "Low",
            "Medium",
            "High"
          )
        )
      )

    phrutils::phr_message(
      origin,
      "HDDS score and category computed successfully."
    )

    return(.dataset)

  },
  on_error = "abort",
  origin = origin)
}
