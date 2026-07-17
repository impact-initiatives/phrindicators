#' @title Add JMP Ladder Categories for Sanitation Facilities
#'
#' @description Categorizes sanitation facilities into JMP (Joint Monitoring Programme) ladder categories based on facility type and sharing status.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param sanitation_facility_cat_col Column name (as a string) for the sanitation facility category.
#' @param sanitation_facility_improved_val Character value for improved sanitation facilities (default: "improved").
#' @param sanitation_facility_unimproved_val Character value for unimproved sanitation facilities (default: "unimproved").
#' @param sanitation_facility_open_defecation_val Character value for open defecation (default: "open_defecation").
#' @param shared_sanitation_col Column name (as a string) for shared sanitation facilities.
#' @param shared_sanitation_yes_val Character value for indicating shared sanitation (default: "yes").
#' @param shared_sanitation_no_val Character value for indicating non-shared sanitation (default: "no").
#'
#' @return A data frame with a new column `wash_jmp_ladder_sanitation_cat`, an ordered factor with JMP ladder categories:
#' "Basic", "Limited", "Unimproved", "Open Defecation".
#'
#' @export
#'
add_sanitation_jmp_ladder <- function(
    .dataset,
    sanitation_facility_cat_col,
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col,
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
) {
  origin <- "add_sanitation_jmp_ladder"

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

    # Validate required columns
    required_columns <- c(sanitation_facility_cat_col, shared_sanitation_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the sanitation facility and shared sanitation columns are present."),
      soft = FALSE
    )

    # Validate categories in sanitation facility column
    valid_sanitation_categories <- c(
      sanitation_facility_improved_val,
      sanitation_facility_unimproved_val,
      sanitation_facility_open_defecation_val,
      NA_character_
    )
    phrutils::phr_validate_choice(
      x = .dataset[[sanitation_facility_cat_col]],
      choices = valid_sanitation_categories,
      origin = origin,
      soft = FALSE
    )

    # Validate categories in shared sanitation column
    valid_shared_categories <- c(shared_sanitation_yes_val, shared_sanitation_no_val, NA_character_)
    phrutils::phr_validate_choice(
      x = .dataset[[shared_sanitation_col]],
      choices = valid_shared_categories,
      origin = origin,
      soft = FALSE
    )

    # Overwrite warnings for any existing output column
    output_column <- "wash_jmp_ladder_sanitation_cat"
    if (output_column %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Use ensure_value to provide defaults
    sanitation_facility_improved_val <- phrutils::ensure_value(sanitation_facility_improved_val, "improved")
    sanitation_facility_unimproved_val <- phrutils::ensure_value(sanitation_facility_unimproved_val, "unimproved")
    sanitation_facility_open_defecation_val <- phrutils::ensure_value(sanitation_facility_open_defecation_val, "open_defecation")
    shared_sanitation_yes_val <- phrutils::ensure_value(shared_sanitation_yes_val, "yes")
    shared_sanitation_no_val <- phrutils::ensure_value(shared_sanitation_no_val, "no")

    # Recoding to JMP ladder categories
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_sanitation_cat = dplyr::case_when(
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_open_defecation_val ~ ("Open Defecation"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_unimproved_val ~ ("Unimproved"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_improved_val &
            as.character(.data[[shared_sanitation_col]]) == shared_sanitation_yes_val ~ ("Limited"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_improved_val &
            as.character(.data[[shared_sanitation_col]]) == shared_sanitation_no_val ~ ("Basic"),
          TRUE ~ NA_character_
        )
      )

    # Convert to factor with ordered levels
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_sanitation_cat = factor(
          wash_jmp_ladder_sanitation_cat,
          levels = c(
            ("Basic"),
            ("Limited"),
            ("Unimproved"),
            ("Open Defecation")
          ),
          ordered = TRUE
        )
      )

    phrutils::phr_message(
      origin = origin,
      message = ("JMP ladder categories for sanitation facilities computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist and contain valid categorical values."))
}
