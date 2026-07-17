#' @title Add Standardized Deaths and Related Columns
#'
#' @description This function processes a dataset where each row represents a deceased individual. It performs categorizations to standardize the dataset by adding columns for deaths based on age, sex, cause of death, and location-based classifications.
#'
#' The function is flexible and works even if key columns like `date_of_death` or `recall_date` are missing, by defaulting the `death` column to 1. Additionally, age-based, sex-based, and other classifications are added only if their respective columns are explicitly provided.
#'
#' @details
#' Depending on available columns and arguments, the function adds the following columns:
#' - **death**: `1` if death is identified (based on `date_of_death` and `recall_date` when available; defaults to `1` if those columns are absent).
#' - **death_under5**: `1` for deceased individuals younger than 5 years, otherwise `0` (requires `age_years_col`).
#' - **death_male**: `1` for individuals with `male_val` in the `sex_col` column (requires `sex_col`, `male_val`, and `female_val`).
#' - **death_female**: `1` for individuals with `female_val` in the `sex_col` column (requires `sex_col`, `male_val`, and `female_val`).
#' - **death_non_trauma**: `1` if the cause of death is in `non_trauma_vals` (requires `cause_of_death_col` and `non_trauma_vals`).
#' - **death_trauma**: `1` if the cause of death is in `trauma_vals` (requires `cause_of_death_col` and `trauma_vals`).
#' - **death_other**: `1` if the cause of death is in `other_vals` (requires `cause_of_death_col` and `other_vals`).
#' - **death_current_location**: `1` if the location of death matches `current_location_residence_vals` (requires `location_of_death_col` and `current_location_residence_vals`).
#' - **death_migration**: `1` if the location of death matches `migration_vals` (requires `location_of_death_col` and `migration_vals`).
#' - **death_last_location**: `1` if the location of death matches `last_location_residence_vals` (requires `location_of_death_col` and `last_location_residence_vals`).
#' - **death_birth**: `1` if the individual was born after the `recall_date_col` value, `0` otherwise (requires `date_of_birth_col` and `recall_date_col`).
#'
#' This function ensures minimal output by providing a `death` column as `1` even if `date_of_death` or `recall_date` is unavailable. It will process age-based, sex-based, or cause-of-death classifications only if their respective inputs are provided.
#'
#' @param .dataset A data frame or tibble containing rows for deceased individuals.
#' @param age_years_col Column name (character, optional) indicating the age of individuals in years. If NULL, `death_under5` will not be created.
#' @param sex_col Column name (character, optional) for the sex of deceased individuals. If NULL, `death_male` and `death_female` will not be created.
#' @param male_val Character value representing males in the `sex_col` column (required if `sex_col` is provided).
#' @param female_val Character value representing females in the `sex_col` column (required if `sex_col` is provided).
#' @param date_of_death_col Column name (character, optional) indicating the date of death. If NULL, `death` is defaulted to `1`.
#' @param recall_date_col Column name (character, optional) indicating the recall date. If NULL, `death` is defaulted to `1`.
#' @param date_of_birth_col Column name (character, optional) indicating the date of birth. If provided with `recall_date_col`, `death_birth` will be created.
#' @param cause_of_death_col (Optional) Column name (character) for the cause of death.
#' @param non_trauma_vals (Optional) Character vector of acceptable values for non-trauma causes of death.
#' @param trauma_vals (Optional) Character vector of acceptable values for trauma-related causes of death.
#' @param other_vals (Optional) Character vector of acceptable values for other causes of death.
#' @param location_of_death_col (Optional) Column name (character) for reported location of death.
#' @param current_location_residence_vals (Optional) Character vector of acceptable values for death at current location of residence.
#' @param migration_vals (Optional) Character vector of acceptable values for death during migration.
#' @param last_location_residence_vals (Optional) Character vector of acceptable values for death at the last location of residence.
#'
#' @return A data frame or tibble with the added columns as described above. At a minimum, the `death` column is always added, defaulting to `1` if no `date_of_death_col` and `recall_date_col` are provided.
#'
#' @examples
#' df <- data.frame(
#'   age_years = c(2, 30, 50),
#'   sex = c("M", "F", "M"),
#'   date_of_death = as.Date(c("2023-01-01", "2023-02-01", NA)),
#'   recall_date = as.Date(c("2022-01-01", "2023-01-01", "2022-12-31")),
#'   cause_of_death = c("malaria", "trauma", "old age"),
#'   location_of_death = c("home", "road", "last residence")
#' )
#'
#' result <- add_standardized_deaths(
#'   .dataset = df,
#'   age_years_col = "age_years",
#'   sex_col = "sex",
#'   male_val = "M",
#'   female_val = "F",
#'   date_of_death_col = "date_of_death",
#'   recall_date_col = "recall_date",
#'   cause_of_death_col = "cause_of_death",
#'   non_trauma_vals = c("malaria", "diarrhea"),
#'   trauma_vals = c("trauma", "accident"),
#'   other_vals = c("old age", "unknown"),
#'   location_of_death_col = "location_of_death",
#'   current_location_residence_vals = c("home"),
#'   migration_vals = c("road"),
#'   last_location_residence_vals = c("last residence")
#' )
#' print(result)
#'
#' @export
add_standardized_deaths <- function(
    .dataset,
    age_years_col = NULL,
    sex_col = NULL,
    male_val = NULL,
    female_val = NULL,
    date_of_death_col = NULL,
    recall_date_col = NULL,
    date_of_birth_col = NULL,
    cause_of_death_col = NULL,
    non_trauma_vals = NULL,
    trauma_vals = NULL,
    other_vals = NULL,
    location_of_death_col = NULL,
    current_location_residence_vals = NULL,
    migration_vals = NULL,
    last_location_residence_vals = NULL
) {
  origin <- "add_standardized_deaths"

  phrutils::phr_try({

    # Use ensure_value for all *_val and *_vals parameters
    male_val <- phrutils::ensure_value(male_val, "Male")
    female_val <- phrutils::ensure_value(female_val, "Female")
    non_trauma_vals <- phrutils::ensure_value(non_trauma_vals, "non_trauma")
    trauma_vals <- phrutils::ensure_value(trauma_vals, "trauma")
    other_vals <- phrutils::ensure_value(other_vals, "other")
    current_location_residence_vals <- phrutils::ensure_value(current_location_residence_vals, "current")
    migration_vals <- phrutils::ensure_value(migration_vals, "migration")
    last_location_residence_vals <- phrutils::ensure_value(last_location_residence_vals, "last")

    # Validate the dataset
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

    # Validate mandatory input columns
    required_columns <- NULL
    if (!is.null(age_years_col)) { required_columns <- c(required_columns, age_years_col) }
    if (!is.null(sex_col)) { required_columns <- c(required_columns, sex_col) }
    if (!is.null(date_of_death_col) && !is.null(recall_date_col)) {
      required_columns <- c(required_columns, date_of_death_col, recall_date_col)
    }
    if (!is.null(date_of_birth_col)) { required_columns <- c(required_columns, date_of_birth_col) }

    if (length(required_columns) > 0) {
      phrutils::phr_validate_columns(
        .dataset,
        required_columns,
        origin = origin,
        hint = ("Ensure that the necessary columns exist in the dataset."),
        soft = FALSE
      )
    }

    # Convert date columns (if applicable)
    date_cols_to_convert <- c()
    if (!is.null(date_of_death_col)) date_cols_to_convert <- c(date_cols_to_convert, date_of_death_col)
    if (!is.null(recall_date_col)) date_cols_to_convert <- c(date_cols_to_convert, recall_date_col)
    if (!is.null(date_of_birth_col)) date_cols_to_convert <- c(date_cols_to_convert, date_of_birth_col)
    
    if (length(date_cols_to_convert) > 0) {
      .dataset <- .dataset |>
        dplyr::mutate(
          across(
            all_of(date_cols_to_convert),
            ~ as.Date(.x)
          )
        )
    }

    # Predefine columns to create
    columns_to_create <- c(
      "death", "death_under5", "death_male", "death_female",
      "death_non_trauma", "death_trauma", "death_other",
      "death_current_location", "death_migration", "death_last_location",
      "death_birth"
    )

    for (col in columns_to_create) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("The column `{col}` already exists and will be overwritten."))
        )
      }
    }

    # Create the `death` column based on date_of_death and recall_date availability
    if (!is.null(date_of_death_col) && !is.null(recall_date_col)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death = dplyr::case_when(
            !is.na(.data[[date_of_death_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_death_col]] > .data[[recall_date_col]] ~ 1,
            is.na(.data[[date_of_death_col]]) | is.na(.data[[recall_date_col]]) ~ 1,  # Default to valid death
            TRUE ~ 0
          )
        )
    } else {
      .dataset <- .dataset |>
        dplyr::mutate(
          death = 1
        )
    }

    # Age-based death classification (optional)
    if (!is.null(age_years_col)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_under5 = dplyr::case_when(
            .data[[age_years_col]] < 5 ~ 1,
            .data[[age_years_col]] >= 5 ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    # Sex-based death classification (optional)
    if (!is.null(sex_col) && !is.null(male_val) && !is.null(female_val)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_male = dplyr::case_when(
            .data[[sex_col]] == male_val ~ 1,
            .data[[sex_col]] == female_val ~ 0,
            TRUE ~ NA_real_
          ),
          death_female = dplyr::case_when(
            .data[[sex_col]] == female_val ~ 1,
            .data[[sex_col]] == male_val ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    # Cause of death columns (optional)
    if (!is.null(cause_of_death_col) && !is.null(non_trauma_vals)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_non_trauma = dplyr::case_when(
            .data[[cause_of_death_col]] %in% non_trauma_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    if (!is.null(cause_of_death_col) && !is.null(trauma_vals)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_trauma = dplyr::case_when(
            .data[[cause_of_death_col]] %in% trauma_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    if (!is.null(cause_of_death_col) && !is.null(other_vals)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_other = dplyr::case_when(
            .data[[cause_of_death_col]] %in% other_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    # Location of death columns (optional)
    if (!is.null(location_of_death_col)) {
      if (!is.null(current_location_residence_vals)) {
        .dataset <- .dataset |>
          dplyr::mutate(
            death_current_location = dplyr::case_when(
              .data[[location_of_death_col]] %in% current_location_residence_vals ~ 1,
              TRUE ~ 0
            )
          )
      }

      if (!is.null(migration_vals)) {
        .dataset <- .dataset |>
          dplyr::mutate(
            death_migration = dplyr::case_when(
              .data[[location_of_death_col]] %in% migration_vals ~ 1,
              TRUE ~ 0
            )
          )
      }

      if (!is.null(last_location_residence_vals)) {
        .dataset <- .dataset |>
          dplyr::mutate(
            death_last_location = dplyr::case_when(
              .data[[location_of_death_col]] %in% last_location_residence_vals ~ 1,
              TRUE ~ 0
            )
          )
      }
    }

    # Birth classification (optional)
    # death_birth: 1 if individual was born after recall_date, 0 otherwise
    if (!is.null(date_of_birth_col) && !is.null(recall_date_col)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          death_birth = dplyr::case_when(
            !is.na(.data[[date_of_birth_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_birth_col]] > .data[[recall_date_col]] ~ 1,
            !is.na(.data[[date_of_birth_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_birth_col]] <= .data[[recall_date_col]] ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    phrutils::phr_message(
      origin = origin,
      message = ("Standardized death calculations successfully added to the dataset.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input column names, values, and data validity for the dataset."))
}
