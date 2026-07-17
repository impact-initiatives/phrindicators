#' @title Add Standardized Nutrition Demographics
#'
#' @description This function adds standardized demographic indicator columns to nutrition data.
#' Similar to how `add_standardized_deaths` works for death data, this creates binary/numeric
#' columns that can be easily aggregated to household level.
#'
#' @details
#' The function creates the following canonical columns for nutrition data:
#' - **nutrition_child_under2**: `1` if age < 2 years, `0` otherwise
#' - **nutrition_child_2to5**: `1` if age >= 2 and < 5 years, `0` otherwise
#' - **nutrition_child_under5**: `1` if age < 5 years, `0` otherwise
#'
#' Note: This function expects `calc_age_years` to exist (created by `add_standardized_age`).
#'
#' @param .dataset A data frame or tibble containing nutrition data
#' @param age_years_col Column name (character) for age in years. Defaults to "calc_age_years"
#'
#' @return A data frame with added nutrition demographic columns
#'
#' @examples
#' df <- data.frame(
#'   child_id = 1:5,
#'   calc_age_years = c(0.8, 1.5, 2.5, 4, 6)
#' )
#'
#' result <- add_standardized_nutrition_demographics(
#'   .dataset = df,
#'   age_years_col = "calc_age_years"
#' )
#'
#' @export
add_standardized_nutrition_demographics <- function(
    .dataset,
    age_years_col = "calc_age_years"
) {

  origin <- "add_standardized_nutrition_demographics"

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

    # Validate age column
    phrutils::phr_validate_columns(
      .dataset,
      age_years_col,
      origin = origin,
      hint = ("Ensure age_years_col exists. Run add_standardized_age first."),
      soft = FALSE
    )

    # Warn about overwriting existing columns
    output_cols <- c(
      "nutrition_child_under2", "nutrition_child_2to5", "nutrition_child_under5"
    )

    for (col in output_cols) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Column `{col}` already exists and will be overwritten."))
        )
      }
    }

    # Add age-based columns
    .dataset <- .dataset |>
      dplyr::mutate(
        nutrition_child_under2 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 2,
          1, 0
        ),
        nutrition_child_2to5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) &
            .data[[age_years_col]] >= 2 &
            .data[[age_years_col]] < 5,
          1, 0
        ),
        nutrition_child_under5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 5,
          1, 0
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Standardized nutrition demographic columns added successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure age column is valid."))
}
