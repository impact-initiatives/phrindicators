#' @title Add MUAC-Based SAM, MAM, GAM Classifications & Flag Extreme MUAC Values
#'
#' @description
#' This function calculates malnutrition categories (SAM, MAM, GAM) based on MUAC (Mid-Upper Arm Circumference)
#' measurements, and it adds a flag for extreme MUAC values. It also detects whether the provided MUAC values
#' are in centimeters or millimeters and performs the appropriate conversion if necessary.
#'
#' @details
#' The function adds or overwrites the following columns to the dataset:
#' * **nut_muac_cm**: MUAC values in centimeters, calculated from millimeters if necessary.
#' * **nut_muac_mm**: MUAC values in millimeters, calculated from centimeters if necessary.
#' * **sam_muac**: Severe Acute Malnutrition (1 = SAM, 0 = not SAM).
#' * **mam_muac**: Moderate Acute Malnutrition (1 = MAM, 0 = not MAM).
#' * **gam_muac**: Global Acute Malnutrition (1 = GAM, 0 = not GAM).
#' * **nut_muac_cat**: Categorical MUAC classification (`"Normal"`, `"MAM"`, `"SAM"`) provided as plain labeled values.
#'   This is derived from `sam_muac` and `mam_muac` after edema and age exclusions.
#' * **flag_muac_extreme**: Flag for extreme MUAC values (1 = less than 5 cm or greater than 20 cm, 0 = otherwise).
#' * **sam_muac_noflag**: Same as `sam_muac`, but `NA` if `flag_muac_extreme == 1`.
#' * **mam_muac_noflag**: Same as `mam_muac`, but `NA` if `flag_muac_extreme == 1`.
#' * **gam_muac_noflag**: Same as `gam_muac`, but `NA` if `flag_muac_extreme == 1`.
#'
#' ## Unit Detection:
#' * **Centimeter Detection**: If MUAC values in `nut_muac_cm_col` are all below 30, they are assumed to be in centimeters.
#'   The function will create or overwrite a `nut_muac_mm` column by converting the values to millimeters (multiplying by 10).
#' * **Millimeter Detection**: If MUAC values in `nut_muac_cm_col` are all above 50, they are assumed to be in millimeters.
#'   The function will create or overwrite a `nut_muac_cm` column by converting the values to centimeters (dividing by 10).
#' * If the values do not satisfy the logic for centimeters or millimeters, no additional column is created, and a warning is issued.
#'
#' Children aged 6-59 months are included in the classification. SAM and GAM classifications take edema confirmation into account.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param nut_muac_cm_col Column name (character) of the MUAC measurements (in cm) or (mm).
#'   Unit detection will determine if values are centimeters or millimeters and perform the necessary conversion.
#' @param edema_confirm_col Column name (character) confirming the presence of bilateral pitting edema.
#' @param child_age_months_col Column name (character) of the child's age in months.
#' @param edema_confirm_val Character value representing confirmed edema in the edema confirmation column.
#'
#' @return A data frame or tibble with the added MUAC unit columns (where applicable), classifications, categories,
#' extreme-value flag, and no-flag variants.
#'
#' @examples
#' # Example dataset with MUAC values in centimeters
#' df_cm <- data.frame(
#'   nut_muac_cm = c(4.8, 12.5, 21.0),
#'   child_age_months = c(14, 54, 30),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' # Add MUAC classifications for centimeters
#' df_result_cm <- add_muac(
#'   .dataset = df_cm,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   edema_confirm_val = "yes"
#' )
#'
#' # Example dataset with MUAC values in millimeters
#' df_mm <- data.frame(
#'   nut_muac_cm = c(80, 125, 210), # Actually in millimeters
#'   child_age_months = c(14, 54, 30),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' # Add MUAC classifications for millimeters
#' df_result_mm <- add_muac(
#'   .dataset = df_mm,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   edema_confirm_val = "yes"
#' )
#'
#' @export
add_muac <- function(
    .dataset,
    nut_muac_cm_col,
    edema_confirm_col,
    child_age_months_col,
    edema_confirm_val
) {
  origin <- "add_muac"

  phrutils::phr_try({

    # Use ensure_value for *_val parameter
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

    required_columns <- c(nut_muac_cm_col, edema_confirm_col, child_age_months_col)
    phrutils::phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = ("Ensure the required columns for MUAC calculation exist in the dataset."),
      soft = FALSE
    )

    phrutils::phr_validate_all_numeric(
      .dataset[[nut_muac_cm_col]],
      origin = origin,
      hint = ("The `nut_muac_cm_col` column must contain numeric values."),
      soft = TRUE
    )


    # Detect MUAC Units and Add/Overwrite Columns

    muac_is_cm <- all(.dataset[[nut_muac_cm_col]] < 30, na.rm = TRUE)
    muac_is_mm <- all(.dataset[[nut_muac_cm_col]] > 50, na.rm = TRUE)

    if (muac_is_cm) {
      # Add or overwrite millimeter column (convert centimeters to millimeters)
      if ("nut_muac_mm" %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = ("The column `nut_muac_mm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset |>
        dplyr::mutate(
          nut_muac_mm = .data[[nut_muac_cm_col]] * 10
        )

      phrutils::phr_message(
        origin = origin,
        message = ("MUAC values detected as centimeters. Converted and added `nut_muac_mm` column.")
      )
    } else if (muac_is_mm) {
      # Add or overwrite centimeter column (convert millimeters to centimeters)
      if ("nut_muac_cm" %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = ("The column `nut_muac_cm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset |>
        dplyr::mutate(
          `nut_muac_cm` = .data[[nut_muac_cm_col]] / 10,
          nut_muac_mm = as.numeric(.data[[nut_muac_cm_col]])
        )

      phrutils::phr_message(
        origin = origin,
        message = ("MUAC values detected as millimeters. Converted and added (or overwritten) `nut_muac_cm` column.")
      )
    } else {
      phrutils::phr_warning(
        origin = origin,
        message = ("MUAC values could not be clearly identified as centimeters or millimeters. No additional unit columns created.")
      )
      # Ensure required downstream columns exist
      if (!"nut_muac_mm" %in% names(.dataset)) {
        .dataset[["nut_muac_mm"]] <- NA_real_
      }
      if (!"nut_muac_cm" %in% names(.dataset)) {
        .dataset[["nut_muac_cm"]] <- NA_real_
      }
    }


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "sam_muac", "mam_muac", "gam_muac",
      "sam_muac_noflag", "mam_muac_noflag", "gam_muac_noflag",
      "nut_muac_cat", "nut_muac_cat_noflag",
      "nut_muac_cm_noflag", "nut_muac_mm_noflag",
      "flag_muac_extreme"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate SAM, MAM, GAM categories and flag extreme MUAC values

    .dataset <- .dataset |>
      dplyr::mutate(
        # Calculate age in months
        age_months = as.numeric(.data[[child_age_months_col]]),

        # Compute SAM, MAM, and GAM based on MUAC thresholds
        sam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) < 11.5 ~ 1,
          TRUE ~ 0
        ),
        mam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) >= 11.5 & as.numeric(.data[[nut_muac_cm_col]]) < 12.5 ~ 1,
          TRUE ~ 0
        ),
        gam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) < 12.5 ~ 1,
          TRUE ~ 0
        ),

        # Adjust SAM and GAM based on edema confirmation
        sam_muac = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, sam_muac
        ),
        gam_muac = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, gam_muac
        ),

        # Exclude ages outside the 6-59 months range
        sam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, sam_muac),
        mam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, mam_muac),
        gam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, gam_muac),

        # Categorize MUAC after edema + age exclusions have been applied
        nut_muac_cat = dplyr::case_when(
          is.na(sam_muac) | is.na(mam_muac) ~ NA_character_,
          sam_muac == 1 ~ ("SAM"),
          mam_muac == 1 ~ ("MAM"),
          TRUE ~ ("Normal")
        ),
        nut_muac_cat = factor(
          nut_muac_cat,
          levels = c(("SAM"), ("MAM"), ("Normal")),
          ordered = TRUE
        ),

        # Flag extreme MUAC values (< 5 cm or > 20 cm)
        flag_muac_extreme = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ 0,
          as.numeric(.data[[nut_muac_cm_col]]) < 5 | as.numeric(.data[[nut_muac_cm_col]]) > 20 ~ 1,
          TRUE ~ 0
        ),

        # No-flag versions of MUAC indicators (set to NA when MUAC is extreme)
        sam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, sam_muac),
        mam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, mam_muac),
        gam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, gam_muac),
        nut_muac_cat_noflag = dplyr::if_else(flag_muac_extreme == 1, factor(NA_character_, levels = levels(nut_muac_cat), ordered = TRUE), nut_muac_cat),
        nut_muac_cm_noflag = dplyr::if_else(
          flag_muac_extreme == 1,
          NA_real_,
          as.numeric(.data[["nut_muac_cm"]])
        ),
        nut_muac_mm_noflag = dplyr::if_else(
          flag_muac_extreme == 1,
          NA_real_,
          as.numeric(.data[["nut_muac_mm"]])
        )

      )

    phrutils::phr_message(
      origin = origin,
      message = ("MUAC-based SAM, MAM, GAM classifications and extreme value flags successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = ("Ensure input columns exist and contain valid numeric or categorical data."))
}
