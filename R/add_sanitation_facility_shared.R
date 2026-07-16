#' @title Add Shared Sanitation Facility Indicator
#'
#' @description Adds a new column to the dataset indicating whether the sanitation facility is shared. If provided with both numerical and categorical inputs, the function prioritizes numerical evaluation, falling back to categorical evaluation for records without numerical data.
#'
#' @details The new column, `wash_sanitation_facility_shared`, is derived using one or both of the following methods:
#' 1. **Numerical Evaluation**: Based on the number of households sharing the sanitation facility. Facilities shared by `shared_threshold` or more households are categorized as "yes" (shared); otherwise, as "no" (not shared).
#' 2. **Categorical Evaluation (Fallback)**: Based on specific character values indicating shared or not shared facilities.
#'
#' If both options are provided, the function computes the indicator using numerical data if available for a given record. If the numerical value is missing or undefined, the categorical data is used as a fallback.
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param num_households_col (Optional) Column name (as a string) for the number of households sharing the sanitation facility. Facilities with `shared_threshold` or more households are considered "yes" (shared).
#' @param shared_threshold (Numeric, Optional) Threshold for determining if a sanitation facility is shared based on the number of households (default is 2).
#' @param shared_response_col (Optional) Column name (as a string) for categorical responses indicating shared/not shared sanitation facilities.
#' @param shared_values (Optional) A character vector containing values indicating shared sanitation facilities.
#' @param not_shared_values (Optional) A character vector containing values indicating not shared sanitation facilities.
#'
#' @return A data frame with a new column `wash_sanitation_facility_shared`, containing:
#' * **"yes"**: Shared sanitation.
#' * **"no"**: Not shared sanitation.
#'
#' @examples
#' # Example 1: Using both numerical and categorical inputs (numerical prioritized)
#' df <- data.frame(
#'   num_households = c(1, 3, NA, NA),
#'   shared_response = c(NA, NA, "shared", "not_shared")
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   num_households_col = "num_households",
#'   shared_response_col = "shared_response",
#'   shared_values = c("shared"),
#'   not_shared_values = c("not_shared")
#' )
#'
#' # Example 2: Using only numerical input
#' df <- data.frame(
#'   num_households = c(1, 2, 3, NA)
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   num_households_col = "num_households"
#' )
#'
#' # Example 3: Using only categorical input
#' df <- data.frame(
#'   shared_response = c("shared", "not_shared", "shared", NA)
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   shared_response_col = "shared_response",
#'   shared_values = c("shared"),
#'   not_shared_values = c("not_shared")
#' )
#'
#' @export
add_sanitation_facility_shared <- function(
    .dataset,
    num_households_col = NULL,
    shared_threshold = 2,
    shared_response_col = NULL,
    shared_values = NULL,
    not_shared_values = NULL
) {

  origin <- "add_sanitation_facility_shared"

  phrutils::phr_try({

    # Use ensure_value for *_values parameters
    shared_values <- phrutils::ensure_value(shared_values, "shared")
    not_shared_values <- phrutils::ensure_value(not_shared_values, "not_shared")

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

    # Validation of inputs
    phrutils::phr_assert(
      !is.null(num_households_col) || (!is.null(shared_response_col) && !is.null(shared_values) && !is.null(not_shared_values)),
      origin = origin,
      ("You must provide either `num_households_col` for numerical evaluation OR `shared_response_col` with `shared_values` and `not_shared_values` for categorical evaluation.")
    )

    # Validate `num_households_col` if provided
    if (!is.null(num_households_col)) {
      phrutils::phr_validate_columns(
        .dataset,
        num_households_col,
        origin = origin,
        hint = ("Ensure the column for number of households exists in the dataset."),
        soft = FALSE
      )

      phrutils::phr_validate_all_numeric(
        .dataset[[num_households_col]],
        origin = origin,
        hint = ("The `num_households_col` must contain numeric values."),
        soft = TRUE
      )
    }

    # Validate `shared_response_col` if provided
    if (!is.null(shared_response_col)) {
      phrutils::phr_validate_columns(
        .dataset,
        shared_response_col,
        origin = origin,
        hint = ("Ensure the column for shared sanitation responses exists in the dataset."),
        soft = FALSE
      )

      phrutils::phr_validate_choice(
        x = .dataset[[shared_response_col]],
        choices = c(shared_values, not_shared_values, NA_character_),
        origin = origin,
        soft = FALSE
      )
    }

    # Overwrite warnings for any existing output column
    output_column <- "wash_sanitation_facility_shared_cat"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Determine which columns are present and write the corresponding logic
    if (!is.null(num_households_col) && !is.null(shared_response_col)) {

      # Both `num_households_col` and `shared_response_col` are present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] >= shared_threshold ~ "yes",
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] < shared_threshold ~ "no",
            is.na(.data[[num_households_col]]) & !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% shared_values ~ "yes",
            is.na(.data[[num_households_col]]) & !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% not_shared_values ~ "no",
            TRUE ~ NA_character_
          )
        )
    } else if (!is.null(num_households_col)) {

      # Only `num_households_col` is present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] >= shared_threshold ~ "yes",
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] < shared_threshold ~ "no",
            TRUE ~ NA_character_
          )
        )
    } else if (!is.null(shared_response_col)) {

      # Only `shared_response_col` is present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% shared_values ~ ("yes"),
            !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% not_shared_values ~ ("no"),
            TRUE ~ NA_character_
          )
        )
    } else {
      # In case neither column is provided (shouldn't happen due to earlier validation)
      stop("Invalid input: No valid columns for computation.")
    }

    phrutils::phr_message(
      origin = origin,
      message = ("Shared sanitation facility indicator computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns and values exist and align with specifications."))
}
