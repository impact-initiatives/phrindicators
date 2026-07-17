#' Add Drinking Water Collection Time Category to Dataset
#'
#' Categorizes households based on the time taken to fetch drinking water. Produces a numeric output column
#' (`wash_drinking_water_time_cat`) with the following codes:
#' - `1`: Less than or equal to 30 minutes (on premises or quick access).
#' - `0`: More than 30 minutes.
#' - `NA`: Undefined or missing information.
#'
#' The function works flexibly based on the arguments provided:
#' - If a numeric column for minutes (`number_minutes_col`) is provided, it is preferred for categorization.
#' - If a categorical column (`categorical_time_col`) is provided, it is used when numeric minutes are not available.
#'
#' @param .dataset Input data frame or tibble.
#' @param number_minutes_col Column name for the reported number of minutes. Defaults to `"number_minutes"`.
#' @param categorical_time_col Column name for categorical responses indicating time in categories (e.g., "under_30min_val", "more_than_30min_val"). Defaults to `"categorical_time"`.
#' @param under_30min_val Value in `categorical_time_col` indicating less than 30 minutes. Defaults to `"under_30min"`.
#' @param more_than_30min_val Value in `categorical_time_col` indicating more than 30 minutes. Defaults to `"more_than_30min"`.
#' @param undefined_val Values in either column indicating undefined responses. Defaults to `c("dnk", "pnta")`.
#' @param premises_val Value in `categorical_time_col` indicating water is fetched on premises. Defaults to `"on_premises"`.
#'
#' @details
#' If both `number_minutes_col` and `categorical_time_col` are provided, the function prioritizes `number_minutes_col` for calculations.
#' The function creates a new column `wash_drinking_water_time_cat` coded as:
#' - `1`: Fetching water takes less than or equal to 30 minutes (or is on premises).
#' - `0`: Fetching water takes more than 30 minutes.
#' - `NA`: Undefined or missing information.
#'
#' @return A modified dataset with an additional column `wash_drinking_water_time_cat`.
#'
#' @examples
#' example_data <- data.frame(
#'   number_minutes = c(15, 45, NA, 25),
#'   categorical_time = c("under_30min", "more_than_30min", "pnta", "under_30min")
#' )
#' add_drinking_water_time_cat(example_data)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data abort sym
#' @export
add_drinking_water_time_cat <- function(
    .dataset,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min_val = "under_30min",
    more_than_30min_val = "more_than_30min",
    undefined_val = c("dnk", "pnta"),
    premises_val = "on_premises"
) {
  origin <- "add_drinking_water_time_cat"

  phrutils::phr_try(
    expr = {
      # Use ensure_value for all *_val parameters
      under_30min_val <- phrutils::ensure_value(under_30min_val, "under_30min")
      more_than_30min_val <- phrutils::ensure_value(more_than_30min_val, "more_than_30min")
      undefined_val <- phrutils::ensure_value(undefined_val, c("dnk", "pnta"))
      premises_val <- phrutils::ensure_value(premises_val, "on_premises")

      # Validate dataset
      phrutils::phr_validate_dataframe(
        .dataset,
        origin = origin,
        hint = ("Ensure you pass a valid data frame or tibble to `.dataset`.")
      )

      phrutils::phr_assert(
        nrow(.dataset) > 0,
        origin = origin,
        ("Dataset is empty.")
      )

      # Validate input columns
      has_numeric_col <- number_minutes_col %in% names(.dataset)
      has_categorical_col <- categorical_time_col %in% names(.dataset)

      if (has_numeric_col) {
        phrutils::phr_assert(
          is.numeric(.dataset[[number_minutes_col]]) | all(is.na(.dataset[[number_minutes_col]])),
          origin = origin,
          (glue::glue("Column {number_minutes_col} must be numeric."))
        )
      }

      if (has_categorical_col) {
        phrutils::phr_validate_columns(
          .dataset,
          categorical_time_col,
          origin = origin,
          hint = ("Ensure the categorical time column exists if minutes are not provided.")
        )

        phrutils::phr_validate_choice(
          x = .dataset[[categorical_time_col]],
          choices = c(under_30min_val, more_than_30min_val, undefined_val, premises_val, NA_character_),
          origin = origin,
          soft = FALSE
        )
      }

      if (!has_numeric_col && !has_categorical_col) {
        phrutils::phr_error(
          message = "Both number_minutes_col and categorical_time_col are missing. At least one is required.",
          origin = origin,
          hint = ("Ensure one of the valid input columns exists in the dataset.")
        )
      }

      # Overwrite warnings for output column
      output_col <- "wash_drinking_water_time_cat"
      if (output_col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Column {output_col} already exists and will be overwritten."))
        )
      }

      # Categorize drinking water time based on 4 scenarios
      if (has_numeric_col && has_categorical_col) {
        # Scenario 1: Both columns available - prefer numeric when not NA, fallback to categorical
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            # Use numeric if available and not NA
            !is.na(.data[[number_minutes_col]]) & .data[[number_minutes_col]] <= 30 ~ 1,
            !is.na(.data[[number_minutes_col]]) & .data[[number_minutes_col]] > 30 ~ 0,
            # Fallback to categorical if numeric is NA
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% c(under_30min_val, premises_val) ~ 1,
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% more_than_30min_val ~ 0,
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% undefined_val ~ NA_real_,
            TRUE ~ NA_real_
          )
        )
      } else if (has_numeric_col && !has_categorical_col) {
        # Scenario 2: Only numeric column available
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            .data[[number_minutes_col]] <= 30 ~ 1,
            .data[[number_minutes_col]] > 30 ~ 0,
            TRUE ~ NA_real_
          )
        )
      } else if (!has_numeric_col && has_categorical_col) {
        # Scenario 3: Only categorical column available
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            .data[[categorical_time_col]] %in% c(under_30min_val, premises_val) ~ 1,
            .data[[categorical_time_col]] %in% more_than_30min_val ~ 0,
            .data[[categorical_time_col]] %in% undefined_val ~ NA_real_,
            TRUE ~ NA_real_
          )
        )
      } else {
        # Scenario 4: Neither column available (fallback error)
        phrutils::phr_error(
          message = "Neither number_minutes_col nor categorical_time_col are available.",
          origin = origin,
          hint = ("This should not happen if validation passed. Check column availability.")
        )
      }

      phrutils::phr_message(
        origin = origin,
        message = ("Drinking water time category successfully added as wash_drinking_water_time_cat.")
      )
      return(.dataset)
    },
    on_error = "abort",
    origin = origin
  )
}
