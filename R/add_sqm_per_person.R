#' @title Add Shelter Area, Square Meters per Person, and Categorization
#'
#' @description Computes the total area (in square meters), square meters per person, and a categorical classification of square meters per person for a shelter based on its geometric shape (rectangle or circle), dimensions, household size, and confirmation of measurement. The output includes three new columns:
#' * **area_sqm**: The total area of the shelter in square meters.
#' * **sqm_per_person**: The estimated square meters per person, calculated as area divided by household size.
#' * **sqm_per_person_cat**: A categorical variable based on the `sqm_per_person` result with the following categories:
#'   - "<3.5 sqm per person"
#'   - "3.5 - < 4.5 sqm per person"
#'   - "4.5 - < 5.5 sqm per person"
#'   - ">= 5.5 sqm per person"
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param shelter_shape_col Column name (as a string) indicating the shape of the shelter.
#' @param rectangle_val A character value or vector of character values representing a rectangular shelter.
#' @param circle_val A character value or vector of character values representing a circular shelter.
#' @param shelter_length_col Column name (as a string) for the numeric length (in meters) of rectangular shelters.
#' @param shelter_width_col Column name (as a string) for the numeric width (in meters) of rectangular shelters.
#' @param shelter_diameter_col Column name (as a string) for the numeric diameter (in meters) of circular shelters.
#' @param household_size_col Column name (as a string) for the household size (number of people living in the shelter).
#' @param measure_confirm_col Column name (as a string) for a categorical column confirming if measurement is available ("yes", "no", or "dont_know").
#' @param measure_confirm_yes_val (Optional) Character value indicating "yes" in the measurement confirmation column (default is "yes").
#'
#' @return A data frame with three new columns:
#' * **area_sqm**: Calculated total area (in square meters) of the shelter.
#' * **sqm_per_person**: Square meters per person calculated as `area_sqm` divided by `household_size_col`.
#' * **sqm_per_person_cat**: Categorical classifications of `sqm_per_person`.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   shelter_shape = c("rectangle", "circle", "rectangle", "circle", "rectangle"),
#'   shelter_length_m = c(2, NA, 5, NA, 3),
#'   shelter_width_m = c(3, NA, 2, NA, 4),
#'   shelter_diameter_m = c(NA, 3, NA, 5, NA),
#'   household_size = c(4, 3, 6, 2, 5),
#'   shelter_measured = c("yes", "no", "yes", "no", "yes")
#' )
#'
#' df_result <- add_sqm_per_person(
#'   .dataset = df,
#'   shelter_shape_col = "shelter_shape",
#'   rectangle_val = "rectangle",
#'   circle_val = "circle",
#'   shelter_length_col = "shelter_length_m",
#'   shelter_width_col = "shelter_width_m",
#'   shelter_diameter_col = "shelter_diameter_m",
#'   household_size_col = "household_size",
#'   measure_confirm_col = "shelter_measured",
#'   measure_confirm_yes_val = "yes"
#' )
#'
#' @export
add_sqm_per_person <- function(
    .dataset,
    shelter_shape_col,
    rectangle_val,
    circle_val,
    shelter_length_col,
    shelter_width_col,
    shelter_diameter_col,
    household_size_col,
    measure_confirm_col,
    measure_confirm_yes_val = "yes"
) {

  origin <- "add_sqm_per_person"

  phrutils::phr_try({


    # Validate dataset

    phrutils::phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = ("Ensure you pass a valid data frame as `.dataset`."),
      soft = FALSE
    )

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin = origin,
      ("Dataset is empty.")
    )


    # Validate input columns

    required_columns <- c(shelter_shape_col, household_size_col, measure_confirm_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the shape, household size, and measurement confirmation columns exist in the dataset."),
      soft = FALSE
    )

    shape_dependent_columns <- c(shelter_length_col, shelter_width_col, shelter_diameter_col)
    phrutils::phr_validate_columns(
      .dataset,
      shape_dependent_columns,
      origin = origin,
      hint = ("Ensure the shelter length, width, and diameter columns exist in the dataset."),
      soft = FALSE
    )


    # Validate shape categories

    valid_shapes <- c(rectangle_val, circle_val, NA_character_)
    phrutils::phr_validate_choice(
      x = .dataset[[shelter_shape_col]],
      choices = valid_shapes,
      origin = origin,
      soft = FALSE
    )


    # Validate household size column

    phrutils::phr_validate_all_numeric(
      .dataset[[household_size_col]],
      origin = origin,
      hint = ("Household size column must contain numeric values."),
      soft = TRUE
    )


    # Validate measuring confirmation column

    phrutils::phr_validate_choice(
      x = .dataset[[measure_confirm_col]],
      choices = c(measure_confirm_yes_val, "no", "dont_know", NA_character_),
      origin = origin,
      soft = FALSE
    )


    # Validate numeric columns

    phrutils::phr_validate_all_numeric(
      .dataset[[shelter_length_col]],
      origin = origin,
      hint = ("Length column must contain numeric values."),
      soft = TRUE
    )
    phrutils::phr_validate_all_numeric(
      .dataset[[shelter_width_col]],
      origin = origin,
      hint = ("Width column must contain numeric values."),
      soft = TRUE
    )
    phrutils::phr_validate_all_numeric(
      .dataset[[shelter_diameter_col]],
      origin = origin,
      hint = ("Diameter column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c("area_sqm", "sqm_per_person", "sqm_per_person_cat")

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate square meters, square meters per person, and categories

    .dataset <- .dataset |>
      dplyr::mutate(
        # Calculate area based on shelter shape
        area_sqm = dplyr::case_when(
          # Only compute if measurement confirmed as 'yes'
          .data[[measure_confirm_col]] != measure_confirm_yes_val ~ NA_real_,

          # Calculate area for rectangle shelters
          .data[[shelter_shape_col]] %in% rectangle_val &
            !is.na(.data[[shelter_length_col]]) & !is.na(.data[[shelter_width_col]]) ~
            round((.data[[shelter_length_col]] * .data[[shelter_width_col]]), 1),

          # Calculate area for circle shelters
          .data[[shelter_shape_col]] %in% circle_val &
            !is.na(.data[[shelter_diameter_col]]) ~
            round(((.data[[shelter_diameter_col]] / 2)^2 * pi), 1),

          # Default: NA
          TRUE ~ NA_real_
        ),
        # Calculate square meters per person
        sqm_per_person = dplyr::case_when(
          !is.na(area_sqm) & !is.na(.data[[household_size_col]]) &
            .data[[household_size_col]] > 0 ~
            round(area_sqm / .data[[household_size_col]], 1),
          TRUE ~ NA_real_
        ),
        # Categorize square meters per person
        sqm_per_person_cat = dplyr::case_when(
          sqm_per_person < 3.5 ~ ("<3.5 sqm per person"),
          sqm_per_person >= 3.5 & sqm_per_person < 4.5 ~ ("3.5 - < 4.5 sqm per person"),
          sqm_per_person >= 4.5 & sqm_per_person < 5.5 ~ ("4.5 - < 5.5 sqm per person"),
          sqm_per_person >= 5.5 ~ (">= 5.5 sqm per person"),
          TRUE ~ NA_character_
        ) |>
          factor(
            levels = c(
              ("<3.5 sqm per person"),
              ("3.5 - < 4.5 sqm per person"),
              ("4.5 - < 5.5 sqm per person"),
              (">= 5.5 sqm per person")
            ),
            ordered = TRUE
          )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Shelter area, square meters per person, and categorization successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist and values align with specifications."))
}
