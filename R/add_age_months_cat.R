#' @title Add Age Categories in Months
#'
#' @description Categorizes individuals into specific age bins based on their age in months (0-59 months).
#'
#' @details The function adds a new factor column `age_months_cat` to the dataset, where age ranges are categorized into the following specific bins:
#' * `0-5 months`
#' * `6-11 months`
#' * `12-17 months`
#' * `18-23 months`
#' * `24-29 months`
#' * `30-35 months`
#' * `36-41 months`
#' * `42-47 months`
#' * `48-53 months`
#' * `54-59 months`.
#' Any age values outside the range [0, 60) are treated as missing.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param age_months_col Column name (character) indicating the age of individuals in months.
#'
#' @return A data frame or tibble with the following new column:
#' * **age_months_cat**: A factor column indicating the specific age category in months of each individual.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   age_months = c(0, 5, 7, 12, 23, 40, 59, NA)
#' )
#'
#' # Add age categories in months
#' df_result <- add_age_months_cat(
#'   .dataset = df,
#'   age_months_col = "age_months"
#' )
#' print(df_result)
#'
#' @export
add_age_months_cat <- function(
    .dataset,
    age_months_col
) {
  origin <- "add_age_months_cat"

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
      age_months_col,
      origin = origin,
      hint = ("Ensure the specified `age_months_col` exists in the dataset."),
      soft = FALSE
    )

    # Check if all values in the specified column are NA
    phrutils::phr_assert(
      !all(is.na(.dataset[[age_months_col]])),
      origin = origin,
      (paste("The column", age_months_col, "contains only NA values. Ensure it contains valid numeric data."))
    )

    phrutils::phr_validate_all_numeric(
      .dataset[[age_months_col]],
      origin = origin,
      hint = ("The `age_months_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for existing output columns

    output_columns <- c("age_months_cat", "roster_age_6_29m", "roster_age_30_59m")

    for (output_column in output_columns) {
      if (output_column %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("The column `{output_column}` already exists and will be overwritten."))
        )
      }
    }


    # Create age categories in months

    .dataset <- .dataset |>
      dplyr::mutate(
        age_months_cat = cut(
          .data[[age_months_col]],
          breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60),
          right = FALSE,
          labels = c(
            "0-5 months", "6-11 months", "12-17 months", "18-23 months",
            "24-29 months", "30-35 months", "36-41 months", "42-47 months",
            "48-53 months", "54-59 months"
          )
        ),
        roster_age_6_29m = dplyr::case_when(
          .data[[age_months_col]] < 6 ~ NA_real_,
          .data[[age_months_col]] >= 6 & .data[[age_months_col]] <= 29 ~ 1,
          .data[[age_months_col]] >= 30 & .data[[age_months_col]] <= 59 ~ 0,
          TRUE ~ NA_real_
        ),
        roster_age_30_59m = dplyr::case_when(
          .data[[age_months_col]] < 6 ~ NA_real_,
          .data[[age_months_col]] >= 6 & .data[[age_months_col]] <= 29 ~ 0,
          .data[[age_months_col]] >= 30 & .data[[age_months_col]] <= 59 ~ 1,
          TRUE ~ NA_real_
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Age categories in months successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure `age_months_col` exists and contains valid numeric data."))
}
