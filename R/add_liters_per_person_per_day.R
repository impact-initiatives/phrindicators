#' Add Liters Per Person Per Day and Related Metrics to a Dataset
#'
#' This function calculates the liters per person per day (`liters_pppd`) and other related metrics
#' for a dataset. The function assumes proper data validation and calculation based on input column names.
#'
#' @param .dataset A data frame or tibble containing input data.
#' @param total_liters_col Column name for total liters of water available.
#' @param household_size_col Column name for household size.
#' @param num_days_col Column name for the number of days of water supply.
#'
#' @details
#' The function calculates and appends the following columns:
#' - `liters_pppd`: Total liters per person per day.
#' - `liters_z_score`: Z-score (standardized value) of the `total_liters`.
#' - `liters_pppd_z_score`: Z-score of the `liters_pppd`.
#' - `liters_log`: Natural logarithm of `total_liters` (log transformation).
#' - `liters_pppd_log`: Natural logarithm of `liters_pppd`.
#' - `liters_log_z_score`: Z-score of the `liters_log`.
#' - `liters_pppd_log_z_score`: Z-score of the `liters_pppd_log`.
#'
#' Uses robust validation to ensure input columns exist, dataset is valid, and calculations align with expectations.
#'
#' @return A data frame or tibble with additional columns for the calculated metrics.
#' @examples
#' example_data <- data.frame(
#'   total_liters = c(100, 200, 300),
#'   household_size = c(4, 5, 6),
#'   num_days = c(1, 2, 3)
#' )
#' result <- add_liters_per_person_per_day(
#'   .dataset = example_data,
#'   total_liters_col = "total_liters",
#'   household_size_col = "household_size",
#'   num_days_col = "num_days"
#' )
#'
#' @importFrom dplyr mutate rowwise ungroup across ends_with
#' @export
add_liters_per_person_per_day <- function(
    .dataset,
    total_liters_col,
    household_size_col,
    num_days_col = NULL  # num_days_col is now optional and can be NULL
) {

  origin <- "add_liters_per_person_per_day"

  phrutils::phr_try(
    expr = {
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

      # Adjust num_days_column if NULL
      if (is.null(num_days_col)) {
        .dataset <- .dataset |>
          dplyr::mutate(temp_num_days_col = 1)  # Add a temporary column with default value 1
        num_days_col <- "temp_num_days_col"  # Use this temporary column as num_days_col
      }

      # Required columns
      required_cols <- c(total_liters_col, household_size_col, num_days_col)
      phrutils::phr_validate_columns(
        .dataset,
        required_cols,
        origin = origin,
        hint = ("Ensure all required columns are present in the dataset."),
        soft = FALSE
      )

      # Validate column contents
      for (col in required_cols) {
        phrutils::phr_assert(
          is.numeric(.dataset[[col]]) & all(.dataset[[col]] >= 0, na.rm = TRUE),
          origin = origin,
          (glue::glue("Column `{col}` must be numeric and contain non-negative values."))
        )
      }

      # Calculations
      .dataset <- .dataset |>
        dplyr::mutate(
          # Calculate liters per person per day
          liters_pppd = .data[[total_liters_col]] /
            (.data[[household_size_col]] * .data[[num_days_col]]),

          # Z-score of total liters
          liters_z_score = (.data[[total_liters_col]] - mean(.data[[total_liters_col]], na.rm = TRUE)) /
            sd(.data[[total_liters_col]], na.rm = TRUE),

          # Z-score of liters per person per day
          liters_pppd_z_score = (liters_pppd - mean(liters_pppd, na.rm = TRUE)) /
            sd(liters_pppd, na.rm = TRUE),

          # Log-transformed values
          liters_log = log(.data[[total_liters_col]]),
          liters_pppd_log = log(liters_pppd),

          # Z-score of log-transformed values
          liters_log_z_score = (liters_log - mean(liters_log, na.rm = TRUE)) /
            sd(liters_log, na.rm = TRUE),
          liters_pppd_log_z_score = (liters_pppd_log - mean(liters_pppd_log, na.rm = TRUE)) /
            sd(liters_pppd_log, na.rm = TRUE),

          # Categorize liters per person per day
          wash_lppd_cat = dplyr::case_when(
            liters_pppd < 3 ~ ("Less than 3 LPPD"),
            liters_pppd >= 3 & liters_pppd < 7.5 ~ ("3-7.5 LPPD"),
            liters_pppd >= 7.5 & liters_pppd < 15 ~ ("7.5- <15 LPPD"),
            liters_pppd >= 15 ~ ("Greater than 15 LPPD"),
            TRUE ~ NA_character_
          )
        )

      # Remove temporary column if created
      if ("temp_num_days_col" %in% names(.dataset)) {
        .dataset <- .dataset |>
          dplyr::select(-temp_num_days_col)
      }

      phrutils::phr_message(
        origin = origin,
        message = ("Liters per person per day and related metrics added successfully.")
      )

      return(.dataset)
    },

    on_error = "abort",
    origin = origin,
    hint = ("Ensure all input columns exist, are numeric, and do not contain negative values.")
  )
}
