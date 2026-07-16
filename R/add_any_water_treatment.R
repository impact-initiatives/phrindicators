#' Add Any Water Treatment Indicator to Dataset
#'
#' This function classifies households as using "yes", "no", or `NA` for any water treatment based on survey responses.
#' It adds a new column \code{wash_any_water_treatment} to the dataset, using user-specified Yes/No/string values for classification.
#'
#' @param .dataset A data frame or tibble with household response data.
#' @param water_treatment_col The column name (character) containing water treatment response values.
#' @param yes_values Character value or vector of response(s) that indicate water treatment occurred.
#' @param no_values Character value or vector of response(s) that indicate no water treatment occurred.
#'
#' @details
#' The function adds \code{wash_any_water_treatment} to \code{.dataset}, with values defined as:
#' - \code{"yes"} for responses matching any entry in \code{yes_values}.
#' - \code{"no"} for responses matching any entry in \code{no_values}.
#' - \code{NA} for anything not matching yes or no (including explicit DNK, PNTA, or missing).
#'
#' Issues warnings or errors as appropriate for empty data, missing columns, or overwriting existing output columns.
#'
#' @return The input data frame or tibble, with a new column \code{wash_any_water_treatment} (\code{"yes"}, \code{"no"}, or \code{NA}).
#'
#' @examples
#' dat <- data.frame(water_treatment = c("boiling", "none", "chlorine", NA, "dnk"))
#' add_any_water_treatment(
#'   dat,
#'   water_treatment_col = "water_treatment",
#'   yes_values = c("boiling", "chlorine", "filter"),
#'   no_values = c("none")
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @export
add_any_water_treatment <- function(
    .dataset,
    water_treatment_col = "water_treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
) {
  origin <- "add_any_water_treatment"
  phrutils::phr_try(
    expr = {
      # Validate dataset
      phrutils::phr_validate_dataframe(
        .dataset,
        origin = origin,
        hint = ("Ensure you pass a valid data frame or tibble to `.dataset`.")
      )
      phrutils::phr_assert(
        nrow(.dataset) > 0,
        origin = origin,
        ("Dataset is empty.")
      )

      # Validate column exists
      phrutils::phr_validate_columns(
        .dataset,
        water_treatment_col,
        origin = origin,
        hint = ("The water treatment column must exist in the dataset."),
        soft = FALSE
      )

      # Validate yes/no vectors not empty
      phrutils::phr_assert(
        length(yes_values) > 0 & is.character(yes_values),
        origin = origin,
        ("Argument `yes_values` must be a non-empty character vector.")
      )
      phrutils::phr_assert(
        length(no_values) > 0 & is.character(no_values),
        origin = origin,
        ("Argument `no_values` must be a non-empty character vector.")
      )

      # Warn if output already exists
      output_col <- "wash_any_water_treatment"
      if (output_col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Column {output_col} already exists and will be overwritten."))
        )
      }

      # Categorize
      .dataset <- dplyr::mutate(
        .dataset,
        !!output_col := dplyr::case_when(
          .data[[water_treatment_col]] %in% yes_values ~ "yes",
          .data[[water_treatment_col]] %in% no_values ~ "no",
          TRUE ~ NA_character_
        )
      )

      phrutils::phr_message(
        origin = origin,
        message = ("Any water treatment indicator (yes/no/NA) calculation completed successfully.")
      )
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = ("Ensure the input column exists and values are appropriate for classification.")
  )
}
