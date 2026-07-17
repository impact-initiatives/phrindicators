#' @title Add Health Barrier Indicators
#'
#' @description This function identifies specific categories of health barriers from a dataset's column containing select multiple responses, and it creates binary indicator columns for each barrier type.
#'
#' @details The input column (`health_barriers_col`) should contain multiple responses in a single cell, separated by delimiters (e.g., commas, spaces). The function searches for specific values corresponding to various barrier categories using `grepl` and creates the following binary (0/1) columns:
#' * **health_barrier_any.physical**
#' * **health_barrier_any.financial**
#' * **health_barrier_any.safety**
#' * **health_barrier_any.quality**
#' * **health_barrier_any.healthcare_seeking**
#' * **health_barrier_any.availability**
#' * **health_barrier_any.other**
#' * **health_barrier_any.none**
#'
#' These columns are coded as `1` if the corresponding values are found in the specified record or `0` if not.
#'
#' @param .dataset A data frame or tibble containing the relevant column.
#' @param health_barriers_col A character string indicating the column name containing health barriers.
#' @param physical_access_barriers_val A character vector of values indicating "physical access barriers."
#' @param financial_access_barriers_val A character vector of values indicating "financial access barriers."
#' @param safety_access_barriers_val A character vector of values indicating "safety access barriers."
#' @param quality_barriers_val A character vector of values indicating "quality-related barriers."
#' @param healthcare_seeking_barriers_val A character vector of values indicating "healthcare-seeking behavior barriers."
#' @param availability_barriers_val A character vector of values indicating "availability barriers."
#' @param other_barriers_val A character vector of values indicating "other barriers."
#' @param no_barriers_val A character vector of values indicating "no barriers."
#' @param did_not_need_val A character vector of values indicating "did not need" health services.
#'
#' @return A data frame with the following new binary columns:
#' * **health_barrier_any.physical**
#' * **health_barrier_any.financial**
#' * **health_barrier_any.safety**
#' * **health_barrier_any.quality**
#' * **health_barrier_any.healthcare_seeking**
#' * **health_barrier_any.availability**
#' * **health_barrier_any.other**
#' * **health_barrier_any.none**
#'
#' @examples
#' df <- data.frame(
#'   health_barriers = c(
#'     "long distance,cost",
#'     "poor quality,unsafe facilities",
#'     "no healthcare available",
#'     "no barriers",
#'     "other"
#'   )
#' )
#'
#' df_result <- add_health_barriers(
#'   .dataset = df,
#'   health_barriers_col = "health_barriers",
#'   physical_access_barriers_val = c("long distance", "transport issues"),
#'   financial_access_barriers_val = c("cost", "expensive"),
#'   safety_access_barriers_val = c("unsafe facilities", "insecurity"),
#'   quality_barriers_val = c("poor quality", "lack of equipment"),
#'   healthcare_seeking_barriers_val = c("refusal", "cultural reasons"),
#'   availability_barriers_val = c("no healthcare available", "services unavailable"),
#'   other_barriers_val = c("other"),
#'   no_barriers_val = c("no barriers")
#' )
#'
#' @export
add_health_barriers <- function(
    .dataset,
    health_barriers_col,
    physical_access_barriers_val,
    financial_access_barriers_val,
    safety_access_barriers_val,
    quality_barriers_val,
    healthcare_seeking_barriers_val,
    availability_barriers_val,
    other_barriers_val,
    no_barriers_val,
    did_not_need_val = NULL
) {

  origin <- "add_health_barriers"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    physical_access_barriers_val <- phrutils::ensure_value(physical_access_barriers_val, "physical")
    financial_access_barriers_val <- phrutils::ensure_value(financial_access_barriers_val, "financial")
    safety_access_barriers_val <- phrutils::ensure_value(safety_access_barriers_val, "safety")
    quality_barriers_val <- phrutils::ensure_value(quality_barriers_val, "quality")
    healthcare_seeking_barriers_val <- phrutils::ensure_value(healthcare_seeking_barriers_val, "healthcare_seeking")
    availability_barriers_val <- phrutils::ensure_value(availability_barriers_val, "availability")
    other_barriers_val <- phrutils::ensure_value(other_barriers_val, "other")
    no_barriers_val <- phrutils::ensure_value(no_barriers_val, "none")
    did_not_need_val <- phrutils::ensure_value(did_not_need_val, "did_not_need")


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


    # Validate health barriers column

    phrutils::phr_validate_columns(
      .dataset,
      health_barriers_col,
      origin = origin,
      hint = ("Ensure the health barriers column exists in the dataset."),
      soft = FALSE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "health_barrier_any.physical",
      "health_barrier_any.financial",
      "health_barrier_any.safety",
      "health_barrier_any.quality",
      "health_barrier_any.healthcare_seeking",
      "health_barrier_any.availability",
      "health_barrier_any.other",
      "health_barrier_any.none",
      "health_barrier_any.did_not_need"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Detect barriers using grepl

    .dataset <- .dataset |>
      dplyr::mutate(
        # Physical access barriers
        health_barrier_any.physical = ifelse(
          grepl(paste(physical_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Financial access barriers
        health_barrier_any.financial = ifelse(
          grepl(paste(financial_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Safety access barriers
        health_barrier_any.safety = ifelse(
          grepl(paste(safety_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Quality barriers
        health_barrier_any.quality = ifelse(
          grepl(paste(quality_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Healthcare-seeking behavior barriers
        health_barrier_any.healthcare_seeking = ifelse(
          grepl(paste(healthcare_seeking_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Availability barriers
        health_barrier_any.availability = ifelse(
          grepl(paste(availability_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Other barriers
        health_barrier_any.other = ifelse(
          grepl(paste(other_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # No barriers
        health_barrier_any.none = ifelse(
          grepl(paste(no_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Did not need
        health_barrier_any.did_not_need = ifelse(
          grepl(paste(did_not_need_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("Health barrier indicators successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure all input columns exist and the health barriers column contains valid, well-formatted data."))
}
