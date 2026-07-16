#' Add Sanitation Facility Categories to a Dataset
#'
#' This function categorizes sanitation facilities into the following categories:
#' "Improved", "Unimproved", and "Open Defecation". It recodes the sanitation
#' facility variable based on provided classifications and adds a new column
#' `wash_sanitation_facility_cat` to the dataset.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param sanitation_facility_col The column name for the sanitation facility variable
#'   in the dataset. Defaults to `"wash_sanitation_facility"`.
#' @param improved_facilities_val A character vector of values classified as
#'   "Improved" sanitation facilities.
#' @param unimproved_facilities_val A character vector of values classified as
#'   "Unimproved" sanitation facilities.
#' @param open_defecation_val A character vector of values classified as
#'   "Open Defecation" practices.
#' @param undefined_val A character vector of values classified as "Undefined" sanitation facilities. Defaults to `NULL` (no undefined category).
#'
#' @details
#' The function performs the following:
#' - Validates that the `.dataset` is a valid data frame or tibble and is not empty.
#' - Verifies that the `sanitation_facility_col` column exists in the dataset and contains valid responses.
#' - Categorizes the values in the `sanitation_facility_col` column based on the provided classifications.
#' - Adds a new column `wash_sanitation_facility_cat` to the dataset with recoded categories:
#'   \describe{
#'     \item{"Improved"}{Corresponding to entries from `improved_facilities_val`.}
#'     \item{"Unimproved"}{Corresponding to entries from `unimproved_facilities_val`.}
#'     \item{"Open Defecation"}{Matching entries from `open_defecation_val`.}
#'     \item{NA}{Any value not matching the above categories.}
#'   }
#'
#' If the output column `wash_sanitation_facility_cat` already exists, a warning is issued, and
#' the column is overwritten.
#'
#' @return A data frame or tibble with an additional column `wash_sanitation_facility_cat`.
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_sanitation_facility = c("flush_to_piped", "pit_lat", "open_defecation", "other", NA)
#' )
#'
#' # Add sanitation facility categories
#' result <- add_sanitation_facility_cat(
#'   .dataset = example_data,
#'   sanitation_facility_col = "wash_sanitation_facility",
#'   improved_facilities_val = c("flush_to_piped", "flush_to_septic", "flush_to_pit"),
#'   unimproved_facilities_val = c("pit_lat", "bucket"),
#'   open_defecation_val = c("open_defecation")
#' )
#'
#' @importFrom dplyr mutate case_when
#' @export
add_sanitation_facility_cat <- function(
    .dataset,
    sanitation_facility_col = "wash_sanitation_facility",
    improved_facilities_val,
    unimproved_facilities_val,
    open_defecation_val,
    undefined_val = NULL
) {
  origin <- "add_sanitation_facility_cat"

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


    # Validate column

    phrutils::phr_validate_columns(
      .dataset,
      sanitation_facility_col,
      origin = origin,
      hint = ("Sanitation facility column must exist in the dataset."),
      soft = FALSE
    )


    # Validate value choices in the sanitation facility column

    allowed_values <- c(
      improved_facilities_val,
      unimproved_facilities_val,
      open_defecation_val,
      undefined_val
    )
    phrutils::phr_validate_choice(
      x = .dataset[[sanitation_facility_col]],
      choices = c(allowed_values, NA_character_),
      origin = origin,
      soft = FALSE
    )


    # Overwrite warnings for any existing output column

    output_column <- "wash_sanitation_facility_cat"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }


    # Recode sanitation facility categories

    .dataset <- .dataset |>
      dplyr::mutate(
        wash_sanitation_facility_cat = dplyr::case_when(
          .data[[sanitation_facility_col]] %in% open_defecation_val ~ ("Open Defecation"),
          .data[[sanitation_facility_col]] %in% unimproved_facilities_val ~ ("Unimproved"),
          .data[[sanitation_facility_col]] %in% improved_facilities_val ~ ("Improved"),
          .data[[sanitation_facility_col]] %in% undefined_val ~ ("Undefined"),

          TRUE ~ NA_character_
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Sanitation facility categories recoded successfully into wash_sanitation_facility_cat.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure the sanitation facility column exists and contains valid response values."))
}
