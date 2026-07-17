#' @title Add LPPD Correction Factor
#'
#' @description Calculates the LPPD correction factor based on the number of days collected, resulting in a new column `lppd_correction_factor` in the dataset.
#'
#' @details The function takes the number of days collected from the specified column, divides these by 7, and rounds the result to 3 decimal places. Values outside the valid range [0, 7] are ignored, and their corresponding entries in the output column are set to `NA`.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param num_days_collect_col Column name (character) indicating the number of days collected. This column must contain numeric values in the range 0-7.
#'
#' @return A data frame or tibble with the following new column:
#' * **lppd_correction_factor**: The calculated correction factor based on the formula:
#'   - `num_days_collected / 7`, rounded to 3 decimal places.
#'   - Set to `NA` for invalid or out-of-range values.
#'
#' @examples
#' # Example dataset
#' df1 <- data.frame(
#'   num_days_collected = c(0, 3, 7, -1, 8, NA)
#' )
#'
#' # Add LPPD correction factor
#' df_result <- add_lppd_correction_factor(
#'   .dataset = df1,
#'   num_days_collect_col = "num_days_collected"
#' )
#'
#' @export
add_lppd_correction_factor <- function(
    .dataset,
    num_days_collect_col
) {
  origin <- "add_lppd_correction_factor"

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


    # Validate input column

    phrutils::phr_validate_columns(
      .dataset,
      num_days_collect_col,
      origin = origin,
      hint = ("Ensure the specified `num_days_collect_col` exists in the dataset."),
      soft = FALSE
    )

    phrutils::phr_validate_all_numeric(
      .dataset[[num_days_collect_col]],
      origin = origin,
      hint = ("The `num_days_collect_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for an existing output column

    output_column <- "lppd_correction_factor"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }


    # Calculate lppd_correction_factor

    .dataset <- .dataset |>
      dplyr::mutate(
        lppd_correction_factor = dplyr::case_when(
          # Values within the valid range [0, 7]
          !is.na(.data[[num_days_collect_col]]) &
            .data[[num_days_collect_col]] >= 0 &
            .data[[num_days_collect_col]] <= 7 ~
            round(.data[[num_days_collect_col]] / 7, 3),
          # Values outside the valid range [0, 7]
          TRUE ~ NA_real_
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("LPPD correction factor successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure `num_days_collect_col` exists and contains valid numeric values within the range 0-7."))
}
