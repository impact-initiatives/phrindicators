#' @title Add Standardized Roster Demographics
#'
#' @description This function adds standardized demographic indicator columns to roster data.
#' Similar to how `add_standardized_deaths` works for death data, this creates binary/numeric
#' columns that can be easily aggregated to household level.
#'
#' @details
#' The function creates the following canonical columns for roster data:
#' - **roster_child_under2**: `1` if age < 2 years, `0` otherwise
#' - **roster_child_under5**: `1` if age < 5 years, `0` otherwise
#' - **roster_2to5**: `1` if 2 <= age < 5 years, `0` otherwise
#' - **roster_5plus**: `1` if age >= 5 years, `0` otherwise
#' - **roster_5_10**: `1` if 5 <= age < 10 years, `0` if age < 5 years, `NA` if age >= 10 years or age is NA
#' - **roster_male**: `1` if sex matches male value, `0` otherwise
#' - **roster_female**: `1` if sex matches female value, `0` otherwise
#' - **roster_woman_15to49**: `1` if female aged 15-49, `0` otherwise
#'
#' Note: This function expects `calc_age_years` to exist (created by `add_standardized_age`).
#'
#' @param .dataset A data frame or tibble containing roster data
#' @param age_years_col Column name (character) for age in years. Defaults to "calc_age_years"
#' @param sex_col Column name (character) for sex/gender
#' @param male_val Value(s) representing male in sex_col (character vector)
#' @param female_val Value(s) representing female in sex_col (character vector)
#'
#' @return A data frame with added roster demographic columns
#'
#' @examples
#' df <- data.frame(
#'   person_id = 1:5,
#'   calc_age_years = c(1, 4, 8, 25, 40),
#'   sex = c("M", "F", "M", "F", "F")
#' )
#'
#' result <- add_standardized_roster_demographics(
#'   .dataset = df,
#'   age_years_col = "calc_age_years",
#'   sex_col = "sex",
#'   male_val = "M",
#'   female_val = "F"
#' )
#'
#' @export
add_standardized_roster_demographics <- function(
    .dataset,
    age_years_col = "calc_age_years",
    sex_col = NULL,
    male_val = NULL,
    female_val = NULL
) {

  origin <- "add_standardized_roster_demographics"

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

    # Use ensure_value for defaults
    male_val <- phrutils::ensure_value(male_val, c("m", "male", "1", "M", "Male"))
    female_val <- phrutils::ensure_value(female_val, c("f", "female", "2", "F", "Female"))

    # Warn about overwriting existing columns
    output_cols <- c(
      "roster_child_under2", "roster_child_under5",
      "roster_2to5", "roster_5plus", "roster_5_10",
      "roster_male", "roster_female", "roster_woman_15to49"
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
        roster_child_under2 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 2,
          1, 0
        ),
        roster_child_under5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 5,
          1, 0
        ),
        roster_2to5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] >= 2 & .data[[age_years_col]] < 5,
          1, 0
        ),
        roster_5plus = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] >= 5,
          1, 0
        ),
        roster_5_10 = dplyr::case_when(
          is.na(.data[[age_years_col]]) ~ NA_real_,
          .data[[age_years_col]] < 5 ~ 0,
          .data[[age_years_col]] < 10 ~ 1,
          TRUE ~ NA_real_
        )
      )

    # Add sex-based columns if sex_col is provided
    if (!is.null(sex_col) && sex_col %in% names(.dataset)) {

      # Standardize sex values for comparison
      .dataset <- .dataset |>
        dplyr::mutate(
          .sex_std = tolower(trimws(as.character(.data[[sex_col]])))
        )

      # Create sex columns
      .dataset <- .dataset |>
        dplyr::mutate(
          roster_male = dplyr::if_else(
            .sex_std %in% tolower(male_val),
            1, 0
          ),
          roster_female = dplyr::if_else(
            .sex_std %in% tolower(female_val),
            1, 0
          ),
          roster_woman_15to49 = dplyr::if_else(
            .sex_std %in% tolower(female_val) &
              !is.na(.data[[age_years_col]]) &
              .data[[age_years_col]] >= 15 &
              .data[[age_years_col]] <= 49,
            1, 0
          )
        ) |>
        dplyr::select(-.sex_std)

    } else {
      # If no sex column, create columns with 0
      phrutils::phr_warning(
        origin = origin,
        message = ("Sex column not provided or not found. Sex-based columns will be set to 0.")
      )

      .dataset <- .dataset |>
        dplyr::mutate(
          roster_male = 0,
          roster_female = 0,
          roster_woman_15to49 = 0
        )
    }

    phrutils::phr_message(
      origin = origin,
      message = ("Standardized roster demographic columns added successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure age and sex columns are valid."))
}
