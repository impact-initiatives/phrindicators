#' Calculate Livelihood Coping Strategies Index (LCSI)
#'
#' @description
#' Calculates LCSI categories (stress, crisis, emergency) based on yes/no/exhausted
#' responses to livelihood coping strategy questions.
#'
#' @param .dataset Data frame containing LCSI component variables
#' @param fsl_lcsi_stress1 Character: Column for stress strategy 1
#' @param fsl_lcsi_stress2 Character: Column for stress strategy 2
#' @param fsl_lcsi_stress3 Character: Column for stress strategy 3
#' @param fsl_lcsi_stress4 Character: Column for stress strategy 4
#' @param fsl_lcsi_crisis1 Character: Column for crisis strategy 1
#' @param fsl_lcsi_crisis2 Character: Column for crisis strategy 2
#' @param fsl_lcsi_crisis3 Character: Column for crisis strategy 3
#' @param fsl_lcsi_emergency1 Character: Column for emergency strategy 1
#' @param fsl_lcsi_emergency2 Character: Column for emergency strategy 2
#' @param fsl_lcsi_emergency3 Character: Column for emergency strategy 3
#' @param yes_val Character: Value representing "yes" response
#' @param no_val Character: Value representing "no, had no need" response
#' @param exhausted_val Character: Value representing "no, exhausted" response
#' @param not_applicable_val Character: Value representing "not applicable" response
#'
#' @return Data frame with added columns: fsl_lcsi_cat_stress, fsl_lcsi_cat_crisis, fsl_lcsi_cat_emergency
#' @export
add_lcsi <- function(.dataset,
                     fsl_lcsi_stress1    = "fsl_lcsi_stress1",
                     fsl_lcsi_stress2    = "fsl_lcsi_stress2",
                     fsl_lcsi_stress3    = "fsl_lcsi_stress3",
                     fsl_lcsi_stress4    = "fsl_lcsi_stress4",
                     fsl_lcsi_crisis1    = "fsl_lcsi_crisis1",
                     fsl_lcsi_crisis2    = "fsl_lcsi_crisis2",
                     fsl_lcsi_crisis3    = "fsl_lcsi_crisis3",
                     fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
                     fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
                     fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
                     yes_val             = "yes",
                     no_val              = "no_had_no_need",
                     exhausted_val       = "no_exhausted",
                     not_applicable_val  = "not_applicable") {

  origin <- "add_lcsi"

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    yes_val <- phrutils::ensure_value(yes_val, "yes")
    no_val <- phrutils::ensure_value(no_val, "no_had_no_need")
    exhausted_val <- phrutils::ensure_value(exhausted_val, "no_exhausted")
    not_applicable_val <- phrutils::ensure_value(not_applicable_val, "not_applicable")

    phrutils::phr_message(origin, "Starting LCSI calculation...")


    # 1. Basic validations

    phrutils::phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint   = "The first argument must be a data.frame or tibble.",
      soft   = FALSE
    )

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin,
      "Dataset is empty."
    )

    lcsi_vars <- c(
      fsl_lcsi_stress1, fsl_lcsi_stress2, fsl_lcsi_stress3, fsl_lcsi_stress4,
      fsl_lcsi_crisis1, fsl_lcsi_crisis2, fsl_lcsi_crisis3,
      fsl_lcsi_emergency1, fsl_lcsi_emergency2, fsl_lcsi_emergency3
    )

    phrutils::phr_validate_columns(
      df           = .dataset,
      required_cols = lcsi_vars,
      origin       = origin,
      hint         = "Ensure all required LCSI variables are present in the dataset.",
      soft         = FALSE
    )


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_lcsi_stress_yes", "fsl_lcsi_stress_exhaust", "fsl_lcsi_stress",
      "fsl_lcsi_crisis_yes", "fsl_lcsi_crisis_exhaust", "fsl_lcsi_crisis",
      "fsl_lcsi_emergency_yes", "fsl_lcsi_emergency_exhaust", "fsl_lcsi_emergency",
      "fsl_lcsi_cat_yes",
      "fsl_lcsi_cat_exhaust",
      "fsl_lcsi_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }
    }


    # 2. Allowed value validation (using phrutils::phr_validate_choice)
    #    NA is allowed and should pass silently

    lcsi_cat_values <- c(yes_val, no_val, exhausted_val, not_applicable_val)

    validate_lcsi_col <- function(col_name) {
      phrutils::phr_validate_choice(
        x       = as.character(.dataset[[col_name]]),
        choices = c(lcsi_cat_values, NA_character_),
        origin  = origin,
        soft    = FALSE
      )
    }

    purrr::walk(lcsi_vars, validate_lcsi_col)


    # 3. Compute completeness mask (Option A logic)
    #    If any of the 10 vars is NA -> all derived LCSI fields become NA

    complete_cases <- stats::complete.cases(.dataset[, lcsi_vars])


    # 4. Main LCSI derived fields
    # Treat invalid values (not in allowed list) as causing NA in calculations

    .dataset <- .dataset |>
      dplyr::mutate(
        fsl_lcsi_stress_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_stress1]]) |
            !.data[[fsl_lcsi_stress1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_stress1]] == yes_val      |
            .data[[fsl_lcsi_stress2]] == yes_val   |
            .data[[fsl_lcsi_stress3]] == yes_val   |
            .data[[fsl_lcsi_stress4]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_stress_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_stress1]]) |
            !.data[[fsl_lcsi_stress1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_stress1]] == exhausted_val      |
            .data[[fsl_lcsi_stress2]] == exhausted_val   |
            .data[[fsl_lcsi_stress3]] == exhausted_val   |
            .data[[fsl_lcsi_stress4]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_stress = dplyr::case_when(
          is.na(fsl_lcsi_stress_yes) & is.na(fsl_lcsi_stress_exhaust) ~ NA_character_,
          fsl_lcsi_stress_yes == "1" | fsl_lcsi_stress_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        fsl_lcsi_crisis_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_crisis1]]) |
            !.data[[fsl_lcsi_crisis1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_crisis1]] == yes_val      |
            .data[[fsl_lcsi_crisis2]] == yes_val   |
            .data[[fsl_lcsi_crisis3]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_crisis_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_crisis1]]) |
            !.data[[fsl_lcsi_crisis1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_crisis1]] == exhausted_val      |
            .data[[fsl_lcsi_crisis2]] == exhausted_val   |
            .data[[fsl_lcsi_crisis3]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_crisis = dplyr::case_when(
          is.na(fsl_lcsi_crisis_yes) & is.na(fsl_lcsi_crisis_exhaust) ~ NA_character_,
          fsl_lcsi_crisis_yes == "1" | fsl_lcsi_crisis_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        fsl_lcsi_emergency_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_emergency1]]) |
            !.data[[fsl_lcsi_emergency1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_emergency1]] == yes_val      |
            .data[[fsl_lcsi_emergency2]] == yes_val   |
            .data[[fsl_lcsi_emergency3]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_emergency_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_emergency1]]) |
            !.data[[fsl_lcsi_emergency1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_emergency1]] == exhausted_val      |
            .data[[fsl_lcsi_emergency2]] == exhausted_val   |
            .data[[fsl_lcsi_emergency3]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_emergency = dplyr::case_when(
          is.na(fsl_lcsi_emergency_yes) & is.na(fsl_lcsi_emergency_exhaust) ~ NA_character_,
          fsl_lcsi_emergency_yes == "1" | fsl_lcsi_emergency_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        # Category based on YES only
        fsl_lcsi_cat_yes = dplyr::case_when(
          fsl_lcsi_stress_yes    != "1" &
            fsl_lcsi_crisis_yes  != "1" &
            fsl_lcsi_emergency_yes != "1" ~ "None",
          fsl_lcsi_stress_yes == "1" &
            fsl_lcsi_crisis_yes != "1" &
            fsl_lcsi_emergency_yes != "1" ~ "Stress",
          fsl_lcsi_crisis_yes == "1" &
            fsl_lcsi_emergency_yes != "1" ~ "Crisis",
          fsl_lcsi_emergency_yes == "1" ~ "Emergency",
          TRUE ~ NA_character_
        ),

        # Category based on EXHAUSTED only
        fsl_lcsi_cat_exhaust = dplyr::case_when(
          fsl_lcsi_stress_exhaust    != "1" &
            fsl_lcsi_crisis_exhaust  != "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ "None",
          fsl_lcsi_stress_exhaust == "1" &
            fsl_lcsi_crisis_exhaust != "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ "Stress",
          fsl_lcsi_crisis_exhaust == "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ "Crisis",
          fsl_lcsi_emergency_exhaust == "1" ~ "Emergency",
          TRUE ~ NA_character_
        ),

        # Combined category (YES or EXHAUSTED)
        fsl_lcsi_cat = dplyr::case_when(
          fsl_lcsi_stress    != "1" &
            fsl_lcsi_crisis  != "1" &
            fsl_lcsi_emergency != "1" ~ "None",
          fsl_lcsi_stress == "1" &
            fsl_lcsi_crisis != "1" &
            fsl_lcsi_emergency != "1" ~ "Stress",
          fsl_lcsi_crisis == "1" &
            fsl_lcsi_emergency != "1" ~ "Crisis",
          fsl_lcsi_emergency == "1" ~ "Emergency",
          TRUE ~ NA_character_
        )
      )


    # 5. Enforce Option A: if any of 10 inputs are NA,
    #    all derived LCSI fields become NA for that row

    derived_cols <- c(
      "fsl_lcsi_stress_yes", "fsl_lcsi_stress_exhaust", "fsl_lcsi_stress",
      "fsl_lcsi_crisis_yes", "fsl_lcsi_crisis_exhaust", "fsl_lcsi_crisis",
      "fsl_lcsi_emergency_yes", "fsl_lcsi_emergency_exhaust", "fsl_lcsi_emergency",
      "fsl_lcsi_cat_yes", "fsl_lcsi_cat_exhaust", "fsl_lcsi_cat"
    )

    .dataset <- .dataset |>
      dplyr::mutate(
        complete_lcsi_inputs = complete_cases
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(derived_cols),
          ~ dplyr::if_else(
            complete_lcsi_inputs,
            .x,
            NA_character_
          )
        )
      ) |>
      dplyr::select(-complete_lcsi_inputs)


    # 6. Standardize factor outputs

    .dataset <- .dataset |>
      dplyr::mutate(
        fsl_lcsi_cat_yes = factor(
          .data$fsl_lcsi_cat_yes,
          levels = c(
            "Emergency",
            "Crisis",
            "Stress",
            "None"
          ),
          ordered = TRUE
        ),
        fsl_lcsi_cat_exhaust = factor(
          .data$fsl_lcsi_cat_exhaust,
          levels = c(
            "Emergency",
            "Crisis",
            "Stress",
            "None"
          ),
          ordered = TRUE
        ),
        fsl_lcsi_cat = factor(
          .data$fsl_lcsi_cat,
          levels = c(
            "Emergency",
            "Crisis",
            "Stress",
            "None"
          ),
          ordered = TRUE
        )
      )

    # Add logic for flag_lcsi_na
    # Count the `not_applicable` responses across all LCSI variables dynamically
    .dataset <- .dataset |>
      dplyr::mutate(
        lcsi_count_na = apply(
          dplyr::select(.dataset, dplyr::all_of(lcsi_vars)),
          1,
          function(x) sum(x == not_applicable_val, na.rm = TRUE)
        ),
        flag_lcsi_na = dplyr::case_when(
          is.na(lcsi_count_na) ~ NA_real_,
          lcsi_count_na >= 4   ~ 1,
          TRUE                 ~ 0
        )
      )

    phrutils::phr_message(origin, "LCSI indicators and categories computed successfully.")

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = "Ensure all LCSI columns exist and contain valid response values.")
}
