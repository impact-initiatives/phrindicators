#' Add Drinking Water Source Categories to a Dataset
#'
#' This function categorizes drinking water sources into the following categories:
#' "Improved", "Unimproved", "Surface Water", and "Undefined". It recodes the
#' drinking water source variable based on provided classifications and adds a new
#' column `wash_drinking_water_source_cat` to the dataset.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param drinking_water_source_col The column name for the drinking water source variable
#'   in the dataset. Defaults to `"wash_water_source"`.
#' @param drinking_water_source_cat_improved_val A character vector of values classified as
#'   "Improved" drinking water sources. Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "piped_dwelling", "piped_compound", "piped_neighbour",
#'       "tap", "borehole", "protected_well", "protected_spring",
#'       "rainwater_collection", "tank_truck", "cart_tank",
#'       "kiosk", "bottled_water", "sachet_water"
#'     }
#'   }
#' @param drinking_water_source_cat_unimproved_val A character vector of values classified as
#'   "Unimproved" drinking water sources. Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "unprotected_well", "unprotected_spring"
#'     }
#'   }
#' @param drinking_water_source_cat_surface_water_val A character value classified as
#'   "Surface Water". Defaults to `"surface_water"`.
#' @param drinking_water_source_cat_undefined_val A character vector of values classified as
#'   "Undefined". Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "dnk" (do not know), "pnta" (prefer not to answer), "other"
#'     }
#'   }
#'
#' @details
#' The function performs the following steps:
#' - Validates that the `.dataset` is a valid data frame or tibble and is not empty.
#' - Verifies that the `drinking_water_source_col` column exists in the dataset and contains valid responses.
#' - Categorizes the values in the `drinking_water_source_col` column based on the provided classifications.
#' - Adds a new column `wash_drinking_water_source_cat` to the dataset with the recoded categories:
#'   \describe{
#'     \item{Improved}{Corresponding to entries from `drinking_water_source_cat_improved_val`.}
#'     \item{Unimproved}{Corresponding to entries from `drinking_water_source_cat_unimproved_val`.}
#'     \item{Surface Water}{Matching `drinking_water_source_cat_surface_water_val`.}
#'     \item{Undefined}{Matching `drinking_water_source_cat_undefined_val`.}
#'   }
#'
#' If the output column `wash_drinking_water_source_cat` already exists, a warning is issued, and
#' the column is overwritten.
#'
#' @return A data frame or tibble with an additional column `wash_drinking_water_source_cat`.
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_drinking_water_source = c("piped_dwelling", "unprotected_well", "surface_water", "dnk")
#' )
#'
#' # Add drinking water source categories
#' result <- add_drinking_water_source_cat(
#'   .dataset = example_data,
#'   drinking_water_source_col = "wash_drinking_water_source"
#' )
#'
#' @importFrom dplyr mutate
#' @export
add_drinking_water_source_cat <- function(
    .dataset,
    drinking_water_source_col = "wash_water_source",
    drinking_water_source_cat_improved_val = c(
      "piped_dwelling",
      "piped_compound",
      "piped_neighbour",
      "tap",
      "borehole",
      "protected_well",
      "protected_spring",
      "rainwater_collection",
      "tank_truck",
      "cart_tank",
      "kiosk",
      "bottled_water",
      "sachet_water"
    ),
    drinking_water_source_cat_unimproved_val = c(
      "unprotected_well",
      "unprotected_spring"
    ),
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_source_cat_undefined_val = c("dnk", "pnta", "other")
) {

  origin <- "add_drinking_water_source_cat"

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
      drinking_water_source_col,
      origin = origin,
      hint = ("Drinking water source column must exist in the dataset."),
      soft = FALSE
    )


    # Validate value choices in the water source column

    # allowed_values <- c(
    #   drinking_water_source_cat_improved_val,
    #   drinking_water_source_cat_unimproved_val,
    #   drinking_water_source_cat_surface_water_val,
    #   drinking_water_source_cat_undefined_val
    # )
    phrutils::phr_validate_choice(
      x = .dataset[[drinking_water_source_col]],
      choices = c(
        drinking_water_source_cat_improved_val,
        drinking_water_source_cat_unimproved_val,
        drinking_water_source_cat_surface_water_val,
        drinking_water_source_cat_undefined_val,
        NA_character_
      ),
      origin = origin,
      soft = FALSE
    )


    # Overwrite warnings for any existing output column

    output_column <- "wash_drinking_water_source_cat"

    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }


    # Recode drinking water source categories

    .dataset <- .dataset |>
      dplyr::mutate(
        wash_drinking_water_source_cat = dplyr::case_when(
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_surface_water_val ~ ("Surface Water"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_unimproved_val ~ ("Unimproved"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_improved_val ~ ("Improved"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_undefined_val ~ ("Undefined"),
          TRUE ~ NA_character_
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Drinking water source categories recoded successfully into wash_drinking_water_source_cat.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure the drinking water source column exists and contains valid response values."))
}
