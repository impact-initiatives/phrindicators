#' @title Add Age Categories
#'
#' @description Categorizes individuals into 5-year age bins based on their age in years.
#'
#' @details The function adds a new factor column `age_cat` to the dataset, where age ranges are calculated in 5-year intervals (e.g., 0-4 years, 5-9 years, etc.) up to a maximum of 120 years. The bins are created as:
#'  - "0-4", "5-9", ..., "120-124".
#' Any age values outside the range [0, 125) are treated as missing.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param age_years_col Column name (character) indicating the age of individuals in years.
#'
#' @return A data frame or tibble with the following new column:
#' * **age_cat**: A factor column indicating the 5-year age category of each individual.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   age_years = c(0, 3, 7, 10, 45, 87, 122, NA)
#' )
#'
#' # Add age categories
#' df_result <- add_age_cat(
#'   .dataset = df,
#'   age_years_col = "age_years"
#' )
#' print(df_result)
#'
#' @export
add_age_cat <- function(
    .dataset,
    age_years_col
) {
  origin <- "add_age_cat"

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
      age_years_col,
      origin = origin,
      hint = ("Ensure the specified `age_years_col` exists in the dataset."),
      soft = FALSE
    )

    phrutils::phr_validate_all_numeric(
      .dataset[[age_years_col]],
      origin = origin,
      hint = ("The `age_years_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for existing output column

    output_column <- "age_cat"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }


    # Create age categories

    .dataset <- .dataset |>
      dplyr::mutate(
        age_cat = cut(
          .data[[age_years_col]],
          breaks = seq(0, 125, by = 5),
          right = FALSE,
          labels = paste0(seq(0, 120, by = 5), "-", seq(4, 124, by = 5)),
          include.lowest = TRUE
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Age categories successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure `age_years_col` exists and contains valid numeric data."))
}
