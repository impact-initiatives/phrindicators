#' @title Add MFA-Z-Based Classifications and Flags
#'
#' @description
#' Calculates severe, moderate, and global MFA-Z classifications, flags extreme MFA-Z values,
#' and provides "no-flag" versions of MFA-Z outputs (set to `NA` when flagged).
#'
#' @details
#' The function adds the following columns to the dataset:
#' - **mfaz**: The calculated MFA-Z score using the `zscorer` package.
#' - **severe_mfaz**: Severe malnutrition (1 = severe, 0 = not severe).
#' - **moderate_mfaz**: Moderate malnutrition (1 = moderate, 0 = not moderate).
#' - **global_mfaz**: Global malnutrition (1 = global, 0 = not global).
#' - **flag_sd_mfaz**: 1 if the MFA-Z value is less than `mean - 4 * SD` or greater than `mean + 3 * SD`, 0 otherwise.
#' - **mfaz_noflag**: Same as `mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **severe_mfaz_noflag**: Same as `severe_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **moderate_mfaz_noflag**: Same as `moderate_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **global_mfaz_noflag**: Same as `global_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **nut_mfaz_cat**: Ordered factor with levels `"Normal"`, `"MAM"`, `"SAM"`, based on MFA-Z classification.
#' - **nut_mfaz_cat_noflag**: Same as `nut_mfaz_cat`, but `NA` if `flag_sd_mfaz == 1`.
#'
#' The function avoids overwriting any existing `sex` column in the dataset by creating a temporary
#' internal column (`temp_sex_for_zscorer`) for sex recoding (1 for male, 2 for female). This column
#' is used for calculations with `zscorer` and is removed from the dataset after processing.
#'
#' If `grouping` is provided, the mean and standard deviation of MFA-Z values are calculated within
#' those groups for determining `flag_sd_mfaz`.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param nut_muac_cm_col Column name (character) of the MUAC measurements (in cm).
#' @param edema_confirm_col Column name (character) confirming the presence of bilateral pitting edema.
#' @param child_age_months_col Column name (character) of the child's age in months.
#' @param child_sex_col Column name (character) of the child's sex.
#' @param male_sex_val Character value representing "male" in the `child_sex_col` column.
#' @param female_sex_val Character value representing "female" in the `child_sex_col` column.
#' @param edema_confirm_val Character value representing confirmed edema in the `edema_confirm_col` column.
#' @param grouping Optional character vector of column names for grouping mean and SD calculations for `flag_sd_mfaz`.
#'
#' @return A data frame with the added MFA-Z score, classifications, flags, no-flag variants, and categorical variables.
#'
#' @examples
#' df <- data.frame(
#'   nut_muac_cm = c(12.5, 10.5, 14.0),
#'   child_sex = c("m", "f", "f"),
#'   child_age_months = c(24, 30, 18),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' df_result <- add_mfaz(
#'   .dataset = df,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   child_sex_col = "child_sex",
#'   male_sex_val = "m",
#'   female_sex_val = "f",
#'   edema_confirm_val = "yes"
#' )
#'
#' @export
add_mfaz <- function(
    .dataset,
    nut_muac_cm_col,
    edema_confirm_col,
    child_age_months_col,
    child_sex_col,
    male_sex_val,
    female_sex_val,
    edema_confirm_val,
    grouping = NULL
) {
  origin <- "add_mfaz"

  phrutils::phr_try({

    # Use ensure_value for *_val parameters
    male_sex_val <- phrutils::ensure_value(male_sex_val, "Male")
    female_sex_val <- phrutils::ensure_value(female_sex_val, "Female")
    edema_confirm_val <- phrutils::ensure_value(edema_confirm_val, "yes")


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

    required_columns <- c(nut_muac_cm_col, edema_confirm_col, child_age_months_col, child_sex_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the required columns for MFA-Z calculation exist in the dataset."),
      soft = FALSE
    )

    phrutils::phr_validate_all_numeric(
      .dataset[[nut_muac_cm_col]],
      origin = origin,
      hint = ("The `nut_muac_cm_col` column must contain numeric values for MFA-Z calculation."),
      soft = TRUE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "severe_mfaz", "moderate_mfaz", "global_mfaz", "mfaz",
      "flag_sd_mfaz", "flag_who_mfaz",
      "mfaz_noflag", "severe_mfaz_noflag", "moderate_mfaz_noflag", "global_mfaz_noflag",
      "nut_mfaz_cat", "nut_mfaz_cat_noflag"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Intermediate sex column for zscorer (to avoid overwrites)

    temp_sex_col <- "temp_sex_for_zscorer"
    if (temp_sex_col %in% names(.dataset)) {
      phrutils::phr_warning(
        origin = origin,
        message = (glue::glue("The temporary column `{temp_sex_col}` already exists and will be overwritten for zscorer compatibility."))
      )
    }

    # Create internal `temp_sex_col` for zscorer (1 = male, 2 = female)
    .dataset <- .dataset |>
      dplyr::mutate(
        {{ temp_sex_col }} := dplyr::case_when(
          .data[[child_sex_col]] == male_sex_val ~ 1,
          .data[[child_sex_col]] == female_sex_val ~ 2,
          TRUE ~ NA_real_
        )
      )


    # Calculate MFA-Z scores using zscorer

    .dataset <- .dataset |>
      dplyr::mutate(
        # Ensure numeric conversion and calculate age in days
        !!rlang::sym(nut_muac_cm_col) := as.numeric(.data[[nut_muac_cm_col]]),
        age_months = as.numeric(.data[[child_age_months_col]]),
        age_days = age_months * 30.25
      ) |>
      zscorer::addWGSR(
        sex = temp_sex_col,
        firstPart = nut_muac_cm_col,
        secondPart = "age_days",
        index = "mfa"
      )


    # Calculate Severe, Moderate, and Global MFAZ classifications

    .dataset <- .dataset |>
      dplyr::mutate(
        severe_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz < -3 ~ 1,
          TRUE ~ 0
        ),
        moderate_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz >= -3 & mfaz < -2 ~ 1,
          TRUE ~ 0
        ),
        global_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz < -2 ~ 1,
          TRUE ~ 0
        ),
        # Adjust classifications based on confirmed edema
        severe_mfaz = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, severe_mfaz
        ),
        global_mfaz = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, global_mfaz
        ),
        # Exclude classifications for ages outside the range of 6-59 months
        severe_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, severe_mfaz),
        moderate_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, moderate_mfaz),
        global_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, global_mfaz)
      )


    # Calculate `flag_sd_mfaz` column

    calculate_flags <- function(df) {
      mean_mfaz <- mean(df$mfaz, na.rm = TRUE)
      sd_mfaz <- sd(df$mfaz, na.rm = TRUE)

      df <- df |>
        dplyr::mutate(
          flag_sd_mfaz = dplyr::case_when(
            is.na(mfaz) ~ 0,
            mfaz < (mean_mfaz - 4) | mfaz > (mean_mfaz + 3) ~ 1,
            TRUE ~ 0
          )
        )
      return(df)
    }

    if (is.null(grouping)) {
      .dataset <- calculate_flags(.dataset)
    } else {
      .dataset <- .dataset |>
        dplyr::group_by(across(all_of(grouping))) |>
        dplyr::group_modify(~ calculate_flags(.x)) |>
        dplyr::ungroup()
    }


    # Add no-flag versions and categorical MFAZ variables

    .dataset <- .dataset |>
      dplyr::mutate(
        mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, mfaz),

        severe_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, severe_mfaz),
        moderate_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, moderate_mfaz),
        global_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, global_mfaz),

        nut_mfaz_cat = dplyr::case_when(
          is.na(severe_mfaz) | is.na(moderate_mfaz) ~ NA_character_,
          severe_mfaz == 1 ~ ("SAM"),
          moderate_mfaz == 1 ~ ("MAM"),
          TRUE ~ ("Normal")
        ),

        nut_mfaz_cat = factor(
          nut_mfaz_cat,
          levels = c(("SAM"), ("MAM"), ("Normal")),
          ordered = TRUE
        ),

        nut_mfaz_cat_noflag = dplyr::if_else(
          flag_sd_mfaz == 1,
          NA_character_,
          as.character(nut_mfaz_cat)
        ),

        nut_mfaz_cat_noflag = factor(
          nut_mfaz_cat_noflag,
          levels = c(("SAM"), ("MAM"), ("Normal")),
          ordered = TRUE
        )
      )


    # Clean up temporary columns

    .dataset <- .dataset |>
      dplyr::select(-all_of(temp_sex_col))

    phrutils::phr_message(
      origin = origin,
      message = ("MFA-Z-based classifications and flags were successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist and contain valid numeric or categorical data."))
}
