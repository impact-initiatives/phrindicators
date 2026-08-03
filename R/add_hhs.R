#' Calculate Household Hunger Scale (HHS)
#'
#' @description
#' Calculates HHS score and categories based on three core hunger questions
#' and their frequency responses.
#'
#' @param .dataset Data frame containing HHS component variables
#' @param fsl_hhs_nofoodhh Character: Column for "no food in household" question
#' @param fsl_hhs_nofoodhh_freq Character: Column for frequency of no food
#' @param fsl_hhs_sleephungry Character: Column for "went to sleep hungry" question
#' @param fsl_hhs_sleephungry_freq Character: Column for frequency of sleep hungry
#' @param fsl_hhs_alldaynight Character: Column for "whole day without eating" question
#' @param fsl_hhs_alldaynight_freq Character: Column for frequency of all day/night
#' @param yes_answer Character: Value representing "yes" response
#' @param no_answer Character: Value representing "no" response
#' @param rarely_answer Character: Value representing "rarely" frequency
#' @param sometimes_answer Character: Value representing "sometimes" frequency
#' @param often_answer Character: Value representing "often" frequency
#'
#' @return Data frame with added columns: fsl_hhs_score, fsl_hhs_cat, fsl_hhs_cat_ipc
#' @export
add_hhs <- function(
    .dataset,
    fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
) {

  origin <- "add_hhs"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    yes_answer <- phrutils::ensure_value(yes_answer, "yes")
    no_answer <- phrutils::ensure_value(no_answer, "no")
    rarely_answer <- phrutils::ensure_value(rarely_answer, "rarely")
    sometimes_answer <- phrutils::ensure_value(sometimes_answer, "sometimes")
    often_answer <- phrutils::ensure_value(often_answer, "often")


    # Validate dataset

    phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin,
      "Dataset is empty."
    )

    # Missing column check
    hhs_vars <- c(
      fsl_hhs_nofoodhh, fsl_hhs_nofoodhh_freq,
      fsl_hhs_sleephungry, fsl_hhs_sleephungry_freq,
      fsl_hhs_alldaynight, fsl_hhs_alldaynight_freq
    )

    missing_cols <- setdiff(hhs_vars, names(.dataset))
    if (length(missing_cols) > 0) {
      phrutils::phr_error(
        glue::glue("Missing required HHS columns: {paste(missing_cols, collapse=', ')}"),
        origin = origin
      )
    }


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_hhs_comp1", "fsl_hhs_comp2", "fsl_hhs_comp3",
      "fsl_hhs_score",
      "fsl_hhs_cat",
      "fsl_hhs_cat_ipc"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }
    }


    # Validate choices

    # Yes/No items
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_nofoodhh]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_sleephungry]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_alldaynight]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)

    # Frequency items
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_nofoodhh_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_sleephungry_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)
    phrutils::phr_validate_choice(.dataset[[fsl_hhs_alldaynight_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)


    # Construct numeric HHS components
    # Preserve original values; treat invalid as NA only during calculation

    .dataset <- .dataset |>
      dplyr::rowwise() |>
      dplyr::mutate(
        # Convert yes/no to numeric 1/0 (treat unrecognized values as NA)
        hhs_nofoodhh_numeric = dplyr::case_when(
          .data[[fsl_hhs_nofoodhh]] == yes_answer ~ 1,
          .data[[fsl_hhs_nofoodhh]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_sleephungry_numeric = dplyr::case_when(
          .data[[fsl_hhs_sleephungry]] == yes_answer ~ 1,
          .data[[fsl_hhs_sleephungry]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_alldaynight_numeric = dplyr::case_when(
          .data[[fsl_hhs_alldaynight]] == yes_answer ~ 1,
          .data[[fsl_hhs_alldaynight]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),

        # Convert frequency to numeric 0/1/2 (treat unrecognized values as NA)
        hhs_nofoodhh_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_nofoodhh_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_nofoodhh_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_sleephungry_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_sleephungry_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_sleephungry_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_alldaynight_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_alldaynight_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_alldaynight_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),

        # Calculate components using numeric versions
        fsl_hhs_comp1 = hhs_nofoodhh_numeric * hhs_nofoodhh_freq_numeric,
        fsl_hhs_comp2 = hhs_sleephungry_numeric * hhs_sleephungry_freq_numeric,
        fsl_hhs_comp3 = hhs_alldaynight_numeric * hhs_alldaynight_freq_numeric
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        fsl_hhs_score = fsl_hhs_comp1 + fsl_hhs_comp2 + fsl_hhs_comp3,
        fsl_hhs_score = dplyr::case_when(
          .data[[fsl_hhs_nofoodhh]] == yes_answer & is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ NA_real_,
          .data[[fsl_hhs_sleephungry]] == yes_answer & is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ NA_real_,
          .data[[fsl_hhs_alldaynight]] == yes_answer & is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ NA_real_,
          .data[[fsl_hhs_nofoodhh]] == no_answer & !is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ NA_real_,
          .data[[fsl_hhs_sleephungry]] == no_answer & !is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ NA_real_,
          .data[[fsl_hhs_alldaynight]] == no_answer & !is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_nofoodhh]]) & !is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_sleephungry]]) & !is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_alldaynight]]) & !is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_nofoodhh]]) & is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_sleephungry]]) & is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ NA_real_,
          is.na(.data[[fsl_hhs_alldaynight]]) & is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ NA_real_,
          TRUE ~ fsl_hhs_score
        )
      ) |>
      # Remove temporary numeric columns (list explicitly to avoid capturing user columns)
      dplyr::select(
        -hhs_nofoodhh_numeric,
        -hhs_sleephungry_numeric,
        -hhs_alldaynight_numeric,
        -hhs_nofoodhh_freq_numeric,
        -hhs_sleephungry_freq_numeric,
        -hhs_alldaynight_freq_numeric
      )


    # Categorisation (IPC and standard) with translatable labels

    .dataset <- .dataset |>
      dplyr::mutate(
        fsl_hhs_cat_ipc = dplyr::case_when(
          fsl_hhs_score == 0 ~ "None",
          fsl_hhs_score == 1 ~ "Little",
          fsl_hhs_score <= 3 ~ "Moderate",
          fsl_hhs_score == 4 ~ "Severe",
          fsl_hhs_score <= 6 ~ "Very Severe",
          TRUE ~ NA_character_
        ),
        fsl_hhs_cat = dplyr::case_when(
          fsl_hhs_score <= 1 ~ "Little to No",
          fsl_hhs_score <= 3 ~ "Moderate",
          fsl_hhs_score <= 6 ~ "Severe",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::mutate(
        fsl_hhs_cat = factor(fsl_hhs_cat, levels = c(
          "Severe",
          "Moderate",
          "Little to No"
        )),
        fsl_hhs_cat_ipc = factor(fsl_hhs_cat_ipc, levels = c(
          "Very Severe",
          "Severe",
          "Moderate",
          "Little",
          "None"
        ))
      )

    phrutils::phr_message(origin, "HHS score and categories computed successfully.")

    return(.dataset)

  },
  on_error = "abort",
  origin = origin,
  hint = "Ensure HHS component variables are appropriately structured.")
}
