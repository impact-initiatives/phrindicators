#' @title Add EC-FIES Score and Categorization
#'
#' @description Calculates the EC-FIES score and food insecurity category based on responses to 8 EC-FIES questions.
#'
#' @details This function adds two columns to the dataset:
#' 1. **nut_ecfies_score**: A numeric score (0-8) calculated by summing the responses to 8 EC-FIES questions. Each question contributes 1 point for `yes_val` and 0 points for all other values.
#' 2. **nut_ecfies_cat**: A categorical food insecurity category based on the score:
#'    - `"No Food Insecurity"`: Score of 0.
#'    - `"Mild Food Insecurity"`: Score between 1 and 3.
#'    - `"Moderate Food Insecurity"`: Score between 4 and 6.
#'    - `"Severe Food Insecurity"`: Score between 7 and 8.
#'
#' @param .dataset A data frame or tibble containing the EC-FIES columns.
#' @param nut_ecfies_so1_col Column name (character) of EC-FIES survey question 1.
#' @param nut_ecfies_so2_col Column name (character) of EC-FIES survey question 2.
#' @param nut_ecfies_so3_col Column name (character) of EC-FIES survey question 3.
#' @param nut_ecfies_so4_col Column name (character) of EC-FIES survey question 4.
#' @param nut_ecfies_so5_col Column name (character) of EC-FIES survey question 5.
#' @param nut_ecfies_so6_col Column name (character) of EC-FIES survey question 6.
#' @param nut_ecfies_so7_col Column name (character) of EC-FIES survey question 7.
#' @param nut_ecfies_so8_col Column name (character) of EC-FIES survey question 8.
#' @param yes_val Character string indicating a "yes" response, contributing 1 point to the score.
#' @param no_val Character string indicating a "no" response, contributing 0 points to the score.
#' @param dont_know_val Character string indicating the "don't know" response, contributing 0 points to the score.
#' @param prefer_not_to_answer_val Character string indicating "prefer not to answer," contributing 0 points to the score.
#'
#' @return A dataset with two new columns:
#' * **nut_ecfies_score**: Numeric score (0-8).
#' * **nut_ecfies_cat**: Factor variable with categories:
#'   - `"No Food Insecurity"`
#'   - `"Mild Food Insecurity"`
#'   - `"Moderate Food Insecurity"`
#'   - `"Severe Food Insecurity"`
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   so1 = c("yes", "no", "yes", "dont_know", "yes"),
#'   so2 = c("no", "no", "yes", "prefer_not_to_answer", "yes"),
#'   so3 = c("yes", "yes", "yes", "no", "dont_know"),
#'   so4 = c("no", "yes", "no", "yes", "yes"),
#'   so5 = c("no", "no", "yes", "yes", "yes"),
#'   so6 = c("yes", "no", "dont_know", "prefer_not_to_answer", "yes"),
#'   so7 = c("no", "yes", "yes", "yes", "no"),
#'   so8 = c("yes", "yes", "yes", "no", "prefer_not_to_answer")
#' )
#'
#' # Call the function
#' df_result <- add_ecfies(
#'   .dataset = df,
#'   nut_ecfies_so1_col = "so1",
#'   nut_ecfies_so2_col = "so2",
#'   nut_ecfies_so3_col = "so3",
#'   nut_ecfies_so4_col = "so4",
#'   nut_ecfies_so5_col = "so5",
#'   nut_ecfies_so6_col = "so6",
#'   nut_ecfies_so7_col = "so7",
#'   nut_ecfies_so8_col = "so8",
#'   yes_val = "yes",
#'   no_val = "no",
#'   dont_know_val = "dont_know",
#'   prefer_not_to_answer_val = "prefer_not_to_answer"
#' )
#'
#' @export
add_ecfies <- function(
    .dataset,
    nut_ecfies_so1_col,
    nut_ecfies_so2_col,
    nut_ecfies_so3_col,
    nut_ecfies_so4_col,
    nut_ecfies_so5_col,
    nut_ecfies_so6_col,
    nut_ecfies_so7_col,
    nut_ecfies_so8_col,
    yes_val ="yes",
    no_val = "no",
    dont_know_val = "dont_know",
    prefer_not_to_answer_val = "prefer_not_to_answer"
) {

  origin <- "add_ecfies"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    yes_val <- phrutils::ensure_value(yes_val, "yes")
    no_val <- phrutils::ensure_value(no_val, "no")
    dont_know_val <- phrutils::ensure_value(dont_know_val, "dont_know")
    prefer_not_to_answer_val <- phrutils::ensure_value(prefer_not_to_answer_val, "prefer_not_to_answer")


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


    # Validate input columns

    ecfies_columns <- c(
      nut_ecfies_so1_col,
      nut_ecfies_so2_col,
      nut_ecfies_so3_col,
      nut_ecfies_so4_col,
      nut_ecfies_so5_col,
      nut_ecfies_so6_col,
      nut_ecfies_so7_col,
      nut_ecfies_so8_col
    )

    phrutils::phr_validate_columns(
      .dataset,
      ecfies_columns,
      origin = origin,
      hint = ("Ensure all EC-FIES columns (1-8) exist in the dataset."),
      soft = FALSE
    )


    # Validate values in each EC-FIES column

    valid_values <- c(yes_val, no_val, dont_know_val, prefer_not_to_answer_val, NA_character_)

    for (col in ecfies_columns) {
      phrutils::phr_validate_choice(
        x = .dataset[[col]],
        choices = valid_values,
        origin = origin,
        soft = TRUE
      )
    }


    # Overwrite warnings for any existing output columns

    output_columns <- c("nut_ecfies_score", "nut_ecfies_cat")

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate ECFIES score and category

    .dataset <- .dataset |>
      dplyr::mutate(
        # Calculate numeric score
        nut_ecfies_score = rowSums(dplyr::across(c(
          nut_ecfies_so1_col, nut_ecfies_so2_col, nut_ecfies_so3_col,
          nut_ecfies_so4_col, nut_ecfies_so5_col, nut_ecfies_so6_col,
          nut_ecfies_so7_col, nut_ecfies_so8_col
        ), ~ ifelse(. == yes_val, 1, 0)), na.rm = TRUE),

        # Assign food insecurity category
        nut_ecfies_cat = dplyr::case_when(
          nut_ecfies_score == 0 ~ ("No Food Insecurity"),
          nut_ecfies_score >= 1 & nut_ecfies_score <= 3 ~ ("Mild Food Insecurity"),
          nut_ecfies_score >= 4 & nut_ecfies_score <= 6 ~ ("Moderate Food Insecurity"),
          nut_ecfies_score >= 7 & nut_ecfies_score <= 8 ~ ("Severe Food Insecurity"),
          TRUE ~ NA_character_
        ),

        # Convert nut_ecfies_cat to a factor
        nut_ecfies_cat = factor(
          nut_ecfies_cat,
          levels = c(
            ("No Food Insecurity"),
            ("Mild Food Insecurity"),
            ("Moderate Food Insecurity"),
            ("Severe Food Insecurity")
          ),
          ordered = TRUE
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("EC-FIES score and categorization successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist, contain valid data, and scoring values are correctly specified."))
}
