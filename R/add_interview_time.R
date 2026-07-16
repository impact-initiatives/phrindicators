#' Calculate and add interview duration in minutes
#'
#' @description
#' Computes the difference in time in minutes (rounded to 2 decimal places)
#' between a start datetime column and an end datetime column, and adds the
#' result as a new column in the dataset.
#'
#' Both columns must contain POSIXct, POSIXlt, or character datetime values
#' with a time component (e.g. `"2025-10-16 14:32:00"`).
#'
#' @param .dataset A data frame containing the start and end datetime columns.
#' @param start_col Character; name of the column containing start datetimes.
#'   Defaults to `"interview_start"`.
#' @param end_col Character; name of the column containing end datetimes.
#'   Defaults to `"interview_end"`.
#' @param new_col Character; name of the new duration column to create.
#'   Defaults to `"interview_duration_mins"`.
#'
#' @return The input data frame with a new numeric column containing interview
#'   duration in minutes (to 2 decimal places).
#' @export
add_interview_time <- function(.dataset,
                               start_col = "interview_start",
                               end_col = "interview_end",
                               new_col = "interview_duration_mins") {

  origin <- "add_interview_time"

  phrutils::phr_try(
    expr = {

      # Validate dataset structure
      phrutils::phr_validate_dataframe(.dataset, origin = origin, soft = FALSE)
      phrutils::phr_assert(nrow(.dataset) > 0, origin, "Dataset must not be empty.")

      # Validate required columns exist
      phrutils::phr_validate_columns(.dataset, c(start_col, end_col), origin = origin, soft = FALSE)

      # Validate that both columns contain datetime values
      phrutils::phr_validate_datetime(.dataset[[start_col]], origin = origin, soft = FALSE)
      phrutils::phr_validate_datetime(.dataset[[end_col]], origin = origin, soft = FALSE)

      # Warn if the output column already exists
      if (new_col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin,
          glue::glue("Column '{new_col}' already exists and will be overwritten.")
        )
      }

      # Calculate interview duration in minutes, rounded to 2 decimal places
      .dataset <- .dataset |>
        dplyr::mutate(
          !!new_col := round(
            as.numeric(difftime(.data[[end_col]], .data[[start_col]], units = "mins")),
            2
          )
        )

      return(.dataset)

    },
    on_error = "warn", origin = origin
  )
}
