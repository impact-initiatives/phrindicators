#' @title Add Healthcare Access Within One Hour
#'
#' @description This function calculates whether healthcare access is available within one hour based on reported travel time, including both numeric and range-based inputs.
#'
#' @details The function adds a new column, `health_healthcare_access_one_hour`, to the dataset. The determination of healthcare access is based on the following logic:
#' 1. **Numeric Minutes**: If travel time in minutes is available, values below 60 minutes are categorized as `"yes"` and 60 minutes or more as `"no"`.
#' 2. **Range-Based Minutes**: If numeric minutes are unavailable, responses from a range-based travel time column are used. Acceptable responses for less than one-hour travel time and one hour or more travel time must be specified.
#' 3. **Unknown or Unavailable Responses**: If neither numeric nor range-based responses are determinable, the result is marked as `"dont_know"`.
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param health_care_travel_time_col Column name (character) indicating reported travel time type (e.g., numeric, range, or dont_know).
#' @param num_minutes_val Character value indicating "numeric minutes" in the travel time type column.
#' @param range_val Character value indicating "range-based travel time" in the travel time type column.
#' @param health_care_travel_time_minutes_col Column name (character) for numeric travel time in minutes.
#' @param health_care_travel_time_range_col Column name (character) for range-based travel time.
#' @param less_than_one_hour_range_val Character vector of acceptable values indicating less than one hour of travel time.
#' @param one_hour_or_more_range_val Character vector of acceptable values indicating one hour or more of travel time.
#'
#' @return A data frame with a new column:
#' * **health_healthcare_access_one_hour**:
#'   - `"yes"`: Healthcare access is available within one hour.
#'   - `"no"`: Healthcare access requires one hour or more of travel time.
#'   - `"dont_know"`: Unable to determine travel time.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   health_care_travel_time = c("num_minutes", "num_minutes", "range", "dont_know"),
#'   health_care_travel_time_minutes = c(45, 65, NA, NA),
#'   health_care_travel_time_range = c(NA, NA, "<1_hour", ">=1_hour")
#' )
#'
#' # Call the function
#' df_result <- add_healthcare_access_one_hour(
#'   .dataset = df,
#'   health_care_travel_time_col = "health_care_travel_time",
#'   num_minutes_val = "num_minutes",
#'   range_val = "range",
#'   health_care_travel_time_minutes_col = "health_care_travel_time_minutes",
#'   health_care_travel_time_range_col = "health_care_travel_time_range",
#'   less_than_one_hour_range_val = c("<1_hour"),
#'   one_hour_or_more_range_val = c(">=1_hour")
#' )
#'
#' @export
add_healthcare_access_one_hour <- function(
    .dataset,
    health_care_travel_time_col,
    num_minutes_val,
    range_val,
    health_care_travel_time_minutes_col,
    health_care_travel_time_range_col,
    less_than_one_hour_range_val,
    one_hour_or_more_range_val
) {

  origin <- "add_healthcare_access_one_hour"

  phrutils::phr_try({

    # Use ensure_value for *_val parameters
    num_minutes_val <- phrutils::ensure_value(num_minutes_val, "num_minutes")
    range_val <- phrutils::ensure_value(range_val, "range")
    less_than_one_hour_range_val <- phrutils::ensure_value(less_than_one_hour_range_val, c("<1_hour", "less_than_1_hour"))
    one_hour_or_more_range_val <- phrutils::ensure_value(one_hour_or_more_range_val, c(">=1_hour", "1_hour_or_more"))

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

    # Validate input columns

    required_columns <- c(
      health_care_travel_time_col,
      health_care_travel_time_minutes_col,
      health_care_travel_time_range_col
    )
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the required travel time columns exist in the dataset."),
      soft = FALSE
    )

    # Validate health_care_travel_time column

    phrutils::phr_validate_choice(
      x = .dataset[[health_care_travel_time_col]],
      choices = c(num_minutes_val, range_val, NA_character_),
      origin = origin,
      soft = TRUE
    )

    # Validate health_care_travel_time_minutes column

    phrutils::phr_validate_all_numeric(
      .dataset[[health_care_travel_time_minutes_col]],
      origin = origin,
      hint = ("The `health_care_travel_time_minutes_col` column must contain numeric values."),
      soft = TRUE
    )

    # Validate ranges from health_care_travel_time_range_col

    valid_ranges <- c(less_than_one_hour_range_val, one_hour_or_more_range_val, NA_character_)
    phrutils::phr_validate_choice(
      x = .dataset[[health_care_travel_time_range_col]],
      choices = valid_ranges,
      origin = origin,
      soft = TRUE
    )

    # Overwrite warnings for any existing output column

    output_column <- "health_healthcare_access_one_hour"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable `{output_column}` already exists and will be overwritten."))
      )
    }

    # Calculate healthcare access within one hour

    .dataset <- .dataset |>
      dplyr::mutate(
        health_healthcare_access_one_hour = dplyr::case_when(
          # Use numeric minutes if available
          .data[[health_care_travel_time_col]] == num_minutes_val &
            !is.na(.data[[health_care_travel_time_minutes_col]]) &
            .data[[health_care_travel_time_minutes_col]] < 60 ~ ("yes"),
          .data[[health_care_travel_time_col]] == num_minutes_val &
            !is.na(.data[[health_care_travel_time_minutes_col]]) &
            .data[[health_care_travel_time_minutes_col]] >= 60 ~ ("no"),

          # Use range-based travel time if numeric minutes are unavailable
          .data[[health_care_travel_time_col]] == range_val &
            .data[[health_care_travel_time_range_col]] %in% less_than_one_hour_range_val ~ ("yes"),
          .data[[health_care_travel_time_col]] == range_val &
            .data[[health_care_travel_time_range_col]] %in% one_hour_or_more_range_val ~ ("no"),

          # Mark as `dont_know` if unable to determine
          is.na(.data[[health_care_travel_time_minutes_col]]) &
            is.na(.data[[health_care_travel_time_range_col]]) ~ ("dont_know"),

          # Default for undefined cases
          TRUE ~ NA_character_
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Healthcare access within one hour successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns have valid data, and values align with the ranges and expected inputs provided."))
}
