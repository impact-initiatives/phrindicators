#' Add Household Water Insecurity (HWISE) Scores to a Dataset
#'
#' This function calculates the Household Water Insecurity Experience (HWISE) scores for a dataset. It computes
#' the HWISE-4 and (optionally) the HWISE-12 scores based on input columns. These scores measure household water
#' insecurity levels based on survey responses, which are categorized into specific severity levels.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param wash_hwise_worry_col Column name for the variable representing "Worry about water issues".
#' @param wash_hwise_plans_col Column name for the variable representing "Plans impacted by water issues".
#' @param wash_hwise_hands_col Column name for the variable representing "Lack of water to wash hands".
#' @param wash_hwise_drink_col Column name for the variable representing "Lack of drinking water".
#' @param wash_hwise_interrupt_col Optional column name for the variable representing "Interruptions due to water issues".
#' @param wash_hwise_clothes_col Optional column name for the variable representing "Lack of water to wash clothes".
#' @param wash_hwise_food_col Optional column name for the variable representing "Lack of water to cook food".
#' @param wash_hwise_body_col Optional column name for the variable representing "Lack of water for body hygiene".
#' @param wash_hwise_angry_col Optional column name for the variable representing "Anger due to water issues".
#' @param wash_hwise_sleep_col Optional column name for the variable representing "Sleep disturbances due to water issues".
#' @param wash_hwise_none_col Optional column name for the variable representing "No water-related issues".
#' @param wash_hwise_shame_col Optional column name for the variable representing "Shame resulting from water-related issues".
#' @param never_val Response category representing "Never". Defaults to `"never"`.
#' @param rarely_val Response category representing "Rarely". Defaults to `"rarely"`.
#' @param sometimes_val Response category representing "Sometimes". Defaults to `"sometimes"`.
#' @param often_val Response category representing "Often". Defaults to `"often"`.
#' @param always_val Response category representing "Always". Defaults to `"always"`.
#'
#' @details
#' The function calculates scores for both `HWISE-4` and `HWISE-12` indicators. The `HWISE-4` score is based on
#' the required variables, while the `HWISE-12` score includes additional optional variables if provided. Both
#' scores are categorized into severity levels as follows:
#'
#' - `HWISE-4 Severity Categories`:
#'   - 0-3: No-to-marginal
#'   - 4-6: Low
#'   - 7-8: Moderate
#'   - 9-10: High
#'   - 11+: Very High
#'
#' - `HWISE-12 Severity Categories`:
#'   - 0-2: No-to-marginal
#'   - 3-11: Low
#'   - 12-23: Moderate
#'   - 24-36: High
#'
#' If a column already exists in the dataset for output variables (like `wash_hwise4_score`), a warning will be
#' generated, and the existing column will be overwritten.
#'
#' @return Returns a data frame or tibble with the calculated HWISE scores:
#' - `wash_hwise4_score`: Score for HWISE-4.
#' - `wash_hwise4_severity_cat`: Severity category for HWISE-4.
#' - `wash_hwise4_insecure_cat`: Binary insecurity category for HWISE-4 (0 = secure, 1 = insecure).
#' - `wash_hwise12_score`: Score for HWISE-12 (if applicable).
#' - `wash_hwise12_severity_cat`: Severity category for HWISE-12 (if applicable).
#' - `wash_hwise12_insecure_cat`: Binary insecurity category for HWISE-12 (0 = secure, 1 = insecure, if applicable).
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_hwise_worry = c("never", "rarely", "sometimes"),
#'   wash_hwise_plans = c("never", "rarely", "sometimes"),
#'   wash_hwise_hands = c("never", "rarely", "sometimes"),
#'   wash_hwise_drink = c("never", "rarely", "sometimes")
#' )
#'
#' # Calculate HWISE scores
#' result <- add_hwise(
#'   .dataset = example_data,
#'   wash_hwise_worry_col = "wash_hwise_worry",
#'   wash_hwise_plans_col = "wash_hwise_plans",
#'   wash_hwise_hands_col = "wash_hwise_hands",
#'   wash_hwise_drink_col = "wash_hwise_drink"
#' )
#'
#' @importFrom dplyr mutate rowwise ungroup across ends_with
#' @export
add_hwise <- function(
    .dataset,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink",
    wash_hwise_interrupt_col = NULL,
    wash_hwise_clothes_col = NULL,
    wash_hwise_food_col = NULL,
    wash_hwise_body_col = NULL,
    wash_hwise_angry_col = NULL,
    wash_hwise_sleep_col = NULL,
    wash_hwise_none_col = NULL,
    wash_hwise_shame_col = NULL,
    never_val = "never",
    rarely_val = "rarely",
    sometimes_val = "sometimes",
    often_val = "often",
    always_val = "always"
) {

  origin <- "add_hwise"

  phrutils::phr_try(

    expr = {

      # Use ensure_value for all *_val parameters
      never_val <- phrutils::ensure_value(never_val, "never")
      rarely_val <- phrutils::ensure_value(rarely_val, "rarely")
      sometimes_val <- phrutils::ensure_value(sometimes_val, "sometimes")
      often_val <- phrutils::ensure_value(often_val, "often")
      always_val <- phrutils::ensure_value(always_val, "always")

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

      # Required columns for wash_hwise-4
      hwise4_cols <- c(wash_hwise_worry_col, wash_hwise_plans_col, wash_hwise_hands_col, wash_hwise_drink_col)
      phrutils::phr_validate_columns(
        .dataset,
        hwise4_cols,
        origin = origin,
        hint = ("Ensure all required wash_hwise-4 variables are present."),
        soft = FALSE
      )

      # Validate response categories
      valid_responses <- c(never_val, rarely_val, sometimes_val, often_val, always_val)

      for (col in hwise4_cols) {
        phrutils::phr_validate_choice(
          x = .dataset[[col]],
          choices = c(valid_responses, NA_character_),
          origin = origin,
          soft = FALSE
        )
      }

      # Required columns for wash_hwise-12 (if provided)
      hwise12_cols <- c(
        wash_hwise_interrupt_col, wash_hwise_clothes_col, wash_hwise_food_col,
        wash_hwise_body_col, wash_hwise_angry_col, wash_hwise_sleep_col,
        wash_hwise_none_col, wash_hwise_shame_col
      )
      hwise12_cols <- hwise12_cols[!is.null(hwise12_cols)]

      if (length(hwise12_cols) > 0) {
        phrutils::phr_validate_columns(
          .dataset,
          hwise12_cols,
          origin = origin,
          hint = ("Ensure all required wash_hwise-12 variables are present if calculating wash_hwise-12."),
          soft = FALSE
        )

        for (col in hwise12_cols) {
          phrutils::phr_validate_choice(
            x = .dataset[[col]],
            choices = c(valid_responses, NA_character_),
            origin = origin,
            soft = FALSE
          )
        }
      }


      # Overwrite warnings for any existing output columns

      overwrite_vars <- c(
        "wash_hwise4_score", "wash_hwise4_severity_cat", "wash_hwise4_insecure_cat",
        "wash_hwise12_score", "wash_hwise12_severity_cat", "wash_hwise12_insecure_cat"
      )

      for (var in overwrite_vars) {
        if (var %in% names(.dataset)) {
          phrutils::phr_warning(
            origin = origin,
            message = (glue::glue("Variable {var} already exists and will be overwritten."))
          )
        }
      }

      # Scoring for wash_hwise-4

      .dataset <- .dataset |>
        dplyr::mutate(
          dplyr::across(
            .cols = hwise4_cols,
            .fns = ~ dplyr::case_when(
              . == never_val ~ 0,
              . == rarely_val ~ 1,
              . == sometimes_val ~ 2,
              . == often_val ~ 3,
              . == always_val ~ 3,
              TRUE ~ NA_real_
            ),
            .names = "{.col}_score"
          )
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          wash_hwise4_score = sum(dplyr::c_across(dplyr::all_of(paste0(hwise4_cols, "_score"))), na.rm = FALSE),
          wash_hwise4_severity_cat = dplyr::case_when(
            wash_hwise4_score >= 0 & wash_hwise4_score <= 3 ~ ("No-to-marginal"),
            wash_hwise4_score >= 4 & wash_hwise4_score <= 6 ~ ("Low"),
            wash_hwise4_score >= 7 & wash_hwise4_score <= 8 ~ ("Moderate"),
            wash_hwise4_score >= 9 & wash_hwise4_score <= 10 ~ ("High"),
            wash_hwise4_score >= 11 ~ ("Very High"),
            TRUE ~ NA_character_
          ),
          wash_hwise4_cat = dplyr::case_when(
            wash_hwise4_score >= 0 & wash_hwise4_score <= 3 ~ ("Water Secure"),
            wash_hwise4_score >= 4 & wash_hwise4_score <= 12 ~ ("Water Insecure"),
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::ungroup()


      # Scoring for wash_hwise-12 (if applicable)

      if (length(hwise12_cols) > 0) {
        .dataset <- .dataset |>
          dplyr::mutate(
            dplyr::across(
              .cols = hwise12_cols,
              .fns = ~ dplyr::case_when(
                . == never_val ~ 0,
                . == rarely_val ~ 1,
                . == sometimes_val ~ 2,
                . == often_val ~ 3,
                . == always_val ~ 3,
                TRUE ~ NA_real_
              ),
              .names = "{.col}_score"
            )
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            wash_hwise12_score = sum(c_across(ends_with("_score")), na.rm = FALSE),
            wash_hwise12_severity_cat = dplyr::case_when(
              wash_hwise12_score >= 0 & wash_hwise12_score <= 2 ~ ("No-to-marginal"),
              wash_hwise12_score >= 3 & wash_hwise12_score <= 11 ~ ("Low"),
              wash_hwise12_score >= 12 & wash_hwise12_score <= 23 ~ ("Moderate"),
              wash_hwise12_score >= 24 & wash_hwise12_score <= 36 ~ ("High"),
              TRUE ~ NA_character_
            )
          ) |>
          dplyr::ungroup()
      }

      phrutils::phr_message(
        origin = origin,
        message = ("wash_hwise-4 and wash_hwise-12 (if applicable) calculations completed successfully.")
      )

      return(.dataset)
    },

    on_error = "abort",
    origin = origin,
    hint = ("Ensure all input columns exist and contain valid response values.")
  )
}
