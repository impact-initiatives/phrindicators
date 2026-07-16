#' @title Add Person-Time Calculations to Dataset
#'
#' @description This function calculates person-time in days for each row based on specified entry and exit dates.
#' It also adds additional columns flagging age- and sex-based person-time where possible. The function gracefully handles
#' cases where optional columns are not provided or contain invalid values.
#'
#' @details
#' The function determines entry and exit dates as follows:
#' - **Entry Date**: Defaults to `recall_date_col`. If provided, uses `dob_col` or `date_joined_col` if they occur after
#' the recall date, prioritizing the more recent one.
#' - **Exit Date**: Defaults to `survey_date_col`. If provided, uses `date_of_death_col` (if death occurs before the survey date)
#' or `date_left_col` (if the person left before the survey date).
#'
#' If optional columns (`dob_col`, `date_of_death_col`, `date_joined_col`, `date_left_col`) are not provided or contain invalid
#' values, the function falls back to calculating person-time based solely on `recall_date_col` and `survey_date_col`.
#'
#' ## Outputs
#' - **person_time**: Person-time in days between calculated entry and exit dates. Negative values are set to `0`.
#' - **entry_date**: The calculated entry date based on available data.
#' - **exit_date**: The calculated exit date based on available data.
#' - **flag_negative_persontime**: `1` for records with negative person-time, `0` otherwise.
#' - **person_time_under5**: Person-time for individuals under 5 years old (if `age_years_col` is provided).
#' - **person_time_male**: Person-time for males (if `sex_col` is provided).
#' - **person_time_female**: Person-time for females (if `sex_col` is provided).
#'
#' @param .dataset A data frame or tibble containing records for individuals.
#' @param recall_date_col Column name (character) for the recall date. **Required.**
#' @param survey_date_col Column name (character) for the survey date. **Required.**
#' @param dob_col (Optional) Column name (character) for the date of birth. Defaults to `NULL`.
#' @param date_of_death_col (Optional) Column name (character) for the date of death. Defaults to `NULL`.
#' @param sex_col (Optional) Column name (character) for the sex of the individual. Defaults to `NULL`.
#' @param age_years_col (Optional) Column name (character) for the age of the individual (in years). Defaults to `NULL`.
#' @param date_left_col (Optional) Column name (character) for the date the individual left. Defaults to `NULL`.
#' @param date_joined_col (Optional) Column name (character) for the date the individual joined. Defaults to `NULL`.
#' @param male_val Character value representing males in the `sex_col`. Defaults to `"Male"`.
#' @param female_val Character value representing females in the `sex_col`. Defaults to `"Female"`.
#'
#' @return A data frame or tibble with calculated person-time columns:
#' - `person_time`: Person-time in days between entry and exit dates.
#' - `entry_date`: Calculated entry date.
#' - `exit_date`: Calculated exit date.
#' - `flag_negative_persontime`: Flag indicating negative person-time.
#' - Optional columns: `person_time_under5`, `person_time_male`, `person_time_female`.
#'
#' @examples
#' df <- data.frame(
#'   recall_date = as.Date(c("2023-01-01", "2023-01-01")),
#'   survey_date = as.Date(c("2023-12-31", "2023-11-30")),
#'   dob = as.Date(c("2022-06-01", "2021-01-01")),
#'   date_of_death = as.Date(NA),
#'   sex = c("Male", "Female"),
#'   age_years = c(1, 2)
#' )
#'
#' result <- add_persontime(
#'   df,
#'   recall_date_col = "recall_date",
#'   survey_date_col = "survey_date",
#'   dob_col = "dob",
#'   sex_col = "sex",
#'   age_years_col = "age_years",
#'   male_val = "Male",
#'   female_val = "Female"
#' )
#'
#' print(result)
#'
#' @export
add_persontime <- function(
    .dataset,
    recall_date_col,
    survey_date_col,
    dob_col = NULL,
    date_of_death_col = NULL,
    sex_col = NULL,
    age_years_col = NULL,
    date_left_col = NULL,
    date_joined_col = NULL,
    male_val = "Male",
    female_val = "Female"
) {
  origin <- "add_persontime"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    male_val <- phrutils::ensure_value(male_val, "Male")
    female_val <- phrutils::ensure_value(female_val, "Female")

    # Step 1: Validate Dataset
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

    # Step 2: Validate and Convert Input Columns
    required_columns <- c(recall_date_col, survey_date_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure mandatory columns for recall date and survey date exist."),
      soft = FALSE
    )

    # Handle optional columns
    optional_columns <- c(dob_col, date_of_death_col, date_left_col, date_joined_col)
    valid_optional_columns <- optional_columns[!is.null(optional_columns)]
    available_columns <- c(required_columns, valid_optional_columns)

    # Convert valid columns to Date
    .dataset <- .dataset |>
      dplyr::mutate(
        across(
          all_of(available_columns),
          ~ as.Date(.x, na.rm = TRUE)
        )
      )

    # Step 3: Calculate Entry Date
    .dataset <- .dataset |>
      dplyr::mutate(
        entry_date = pmax(
          as.Date(.data[[recall_date_col]]),
          if (!is.null(dob_col) && dob_col %in% colnames(.dataset)) as.Date(.data[[dob_col]]) else as.Date(NA),
          if (!is.null(date_joined_col) && date_joined_col %in% colnames(.dataset)) as.Date(.data[[date_joined_col]]) else as.Date(NA),
          na.rm = TRUE
        )
      )

    # Step 4: Calculate Exit Date
    .dataset <- .dataset |>
      dplyr::mutate(
        exit_date = pmin(
          as.Date(.data[[survey_date_col]]),
          if (!is.null(date_of_death_col) && date_of_death_col %in% colnames(.dataset)) as.Date(.data[[date_of_death_col]]) else as.Date(NA),
          if (!is.null(date_left_col) && date_left_col %in% colnames(.dataset)) as.Date(.data[[date_left_col]]) else as.Date(NA),
          na.rm = TRUE
        )
      )

    # Ensure entry_date and exit_date are valid
    phrutils::phr_assert(
      all(!is.na(.dataset$entry_date)),  # Ensure there are valid entry dates
      origin = origin,
      ("No valid entry dates found. Ensure recall_date_col, date_joined_col, or dob_col is available and valid.")
    )

    phrutils::phr_assert(
      all(!is.na(.dataset$exit_date)),  # Ensure there are valid exit dates
      origin = origin,
      ("No valid exit dates found. Ensure survey_date_col, date_of_death_col, or date_left_col is available and valid.")
    )

    # Step 5: Calculate Person Time
    .dataset <- .dataset |>
      dplyr::mutate(
        person_time = as.numeric(exit_date - entry_date),  # Calculate person time in days
        person_time = dplyr::if_else(person_time < 0, 0, person_time),  # Ensure non-negative values
        flag_negative_persontime = as.integer(person_time < 0)  # Flag negative person time
      )

    # Ensure person time is valid
    phrutils::phr_assert(
      all(!is.na(.dataset$person_time)),
      origin = origin,
      ("No valid person time could be calculated. Check date columns and their values.")
    )

    # Step 6: Add Optional Columns

    # Add person_time_under5 if age_years_col is provided and exists in dataset
    if (!is.null(age_years_col) && age_years_col %in% colnames(.dataset)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          person_time_under5 = dplyr::case_when(
            .data[[age_years_col]] < 5 ~ person_time,
            TRUE ~ 0
          )
        )
    }

    # Add person_time_male if sex_col is provided and exists in dataset
    if (!is.null(sex_col) && sex_col %in% colnames(.dataset)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          person_time_male = dplyr::case_when(
            .data[[sex_col]] == male_val ~ person_time,
            TRUE ~ 0
          )
        )
    }

    # Add person_time_female if sex_col is provided and exists in dataset
    if (!is.null(sex_col) && sex_col %in% colnames(.dataset)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          person_time_female = dplyr::case_when(
            .data[[sex_col]] == female_val ~ person_time,
            TRUE ~ 0
          )
        )
    }

    phrutils::phr_message(
      origin = origin,
      message = ("Person-time calculations successfully added to the dataset.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure required columns are present and contain valid data to calculate person-time."))
}
