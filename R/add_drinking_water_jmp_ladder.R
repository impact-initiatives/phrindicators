#' @title Add JMP Ladder Categories for Drinking Water
#'
#' @description This function categorizes drinking water access based on water source and collection time into JMP (Joint Monitoring Programme) ladder categories.
#'
#' @details The resulting categories are based on ensuring safe and reliable drinking water sources:
#' * **Basic**: Improved water source with water collection time ≤ 30 minutes per round trip.
#' * **Limited**: Improved water source with water collection time > 30 minutes per round trip.
#' * **Unimproved**: Water source categorized as unimproved, regardless of collection time.
#' * **Surface Water**: Surface water source, regardless of collection time.
#'
#' The categories are ordered from "Basic" (best) to "Surface Water" (worst) for analysis or visualization needs.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param drinking_water_source_cat_col Column name (as a string) for the drinking water source category.
#' @param drinking_water_source_cat_improved_val Value indicating improved water sources (defaults to `"Improved"` if NULL or empty).
#' @param drinking_water_source_cat_unimproved_val Value indicating unimproved water sources (defaults to `"Unimproved"` if NULL or empty).
#' @param drinking_water_source_cat_surface_water_val Value indicating surface water sources (defaults to `"Surface Water"` if NULL or empty).
#' @param drinking_water_time_col Column name (as a string) for drinking water time category.
#' @param drinking_water_time_under_30min_val Value indicating water collection time ≤ 30 minutes (defaults to `"1"` if NULL or empty).
#' @param drinking_water_time_over_30min_val Value indicating water collection time > 30 minutes (defaults to `"0"` if NULL or empty).
#'
#' @return A data frame with a newly added column named `wash_jmp_ladder_drinking_water_cat`, which categorizes each record into one of the following JMP ladder categories:
#' * **Basic**
#' * **Limited**
#' * **Unimproved**
#' * **Surface Water**
#'
#' The new column is an ordered factor, in the JMP ladder order of "Basic", "Limited", "Unimproved", and "Surface Water".
#'
#' @export
add_drinking_water_jmp_ladder <- function(
    .dataset,
    drinking_water_source_cat_col = "wash_drinking_water_source_cat",
    drinking_water_source_cat_improved_val = NULL,
    drinking_water_source_cat_unimproved_val = NULL,
    drinking_water_source_cat_surface_water_val = NULL,
    drinking_water_time_col = "wash_drinking_water_time",
    drinking_water_time_under_30min_val = NULL,
    drinking_water_time_over_30min_val = NULL
) {
  origin <- "add_drinking_water_jmp_ladder"

  phrutils::phr_try({

    # Validate dataset
    phrutils::phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = ("Ensure you pass a valid data frame or tibble to `.dataset`."),
      soft = FALSE
    )

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin = origin,
      ("Dataset is empty.")
    )

    # Validate required columns
    required_columns <- c(drinking_water_source_cat_col, drinking_water_time_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the drinking water source and collection time columns are present."),
      soft = FALSE
    )

    # Overwrite warnings for any existing output column
    output_column <- "wash_jmp_ladder_drinking_water_cat"
    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Use ensure_value for all the variables
    drinking_water_source_cat_improved_val <- phrutils::ensure_value(drinking_water_source_cat_improved_val, "Improved")
    drinking_water_source_cat_unimproved_val <- phrutils::ensure_value(drinking_water_source_cat_unimproved_val, "Unimproved")
    drinking_water_source_cat_surface_water_val <- phrutils::ensure_value(drinking_water_source_cat_surface_water_val, "Surface Water")
    drinking_water_time_under_30min_val <- phrutils::ensure_value(drinking_water_time_under_30min_val, "1")
    drinking_water_time_over_30min_val <- phrutils::ensure_value(drinking_water_time_over_30min_val, "0")

    # JMP ladder categorization
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_drinking_water_cat = dplyr::case_when(
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_surface_water_val ~ ("Surface Water"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_unimproved_val ~ ("Unimproved"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_improved_val &
            as.character(.data[[drinking_water_time_col]]) == drinking_water_time_over_30min_val ~ ("Limited"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_improved_val &
            as.character(.data[[drinking_water_time_col]]) == drinking_water_time_under_30min_val ~ ("Basic"),
          TRUE ~ NA_character_
        )
      )

    # Convert to an ordered factor
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_drinking_water_cat = factor(
          wash_jmp_ladder_drinking_water_cat,
          levels = c(
            ("Basic"),
            ("Limited"),
            ("Unimproved"),
            ("Surface Water")
          ),
          ordered = TRUE
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("JMP ladder categories for drinking water computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist and provide valid categorical values."))
}
