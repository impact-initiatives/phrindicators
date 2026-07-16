#' Compute FEWS NET Matrix Classification (FCM Phase)
#'
#' The `add_fcm_phase` function computes the Food Consumption and Livelihood Coping Mechanism (FCM) classification based on the FEWS NET Matrix Guidance. This function dynamically processes categorical data related to food consumption scores (FCS), reduced coping strategy index (rCSI), household hunger score (HHS), and household dietary diversity score (HDDS) to calculate classification outputs.
#'
#' This function implements the methodology as outlined in the [FEWS NET Matrix Guidance Document](https://fews.net/sites/default/files/documents/reports/fews-net-matrix-guidance-document.pdf).
#'
#' @param .dataset A data frame containing columns with food consumption data (e.g., FCS, rCSI, HHS, HDDS).
#' @param fcs_col A character string indicating the column name for the food consumption score (FCS) categories. Default: `"fsl_fcs_cat"`.
#' @param rcsi_col A character string indicating the column name for the reduced coping strategy index (rCSI) categories. Default: `"fsl_rcsi_cat"`.
#' @param hhs_col A character string indicating the column name for the household hunger score (HHS) categories. Default: `"fsl_hhs_cat_ipc"`.
#' @param hdds_col A character string indicating the column name for the household dietary diversity score (HDDS) categories. Default: `"fsl_hdds_cat"`.
#' @param fcs_acceptable_val A character string for the "Acceptable" FCS category. Default: `NULL`. (Backup value: `"Acceptable"`)
#' @param fcs_borderline_val A character string for the "Borderline" FCS category. Default: `NULL`. (Backup value: `"Borderline"`)
#' @param fcs_poor_val A character string for the "Poor" FCS category. Default: `NULL`. (Backup value: `"Poor"`)
#' @param rcsi_low_val A character string for the "Low" rCSI category. Default: `NULL`. (Backup value: `"Low"`)
#' @param rcsi_medium_val A character string for the "Medium" rCSI category. Default: `NULL`. (Backup value: `"Medium"`)
#' @param rcsi_high_val A character string for the "High" rCSI category. Default: `NULL`. (Backup value: `"High"`)
#' @param hdds_low_val A character string for the "Low" HDDS category. Default: `NULL`. (Backup value: `"Low"`)
#' @param hdds_medium_val A character string for the "Medium" HDDS category. Default: `NULL`. (Backup value: `"Medium"`)
#' @param hdds_high_val A character string for the "High" HDDS category. Default: `NULL`. (Backup value: `"High"`)
#' @param hhs_none_val A character string for the "None" HHS category. Default: `NULL`. (Backup value: `"None"`)
#' @param hhs_little_val A character string for the "Little" HHS category. Default: `NULL`. (Backup value: `"Little"`)
#' @param hhs_moderate_val A character string for the "Moderate" HHS category. Default: `NULL`. (Backup value: `"Moderate"`)
#' @param hhs_severe_val A character string for the "Severe" HHS category. Default: `NULL`. (Backup value: `"Severe"`)
#' @param hhs_very_severe_val A character string for the "Very Severe" HHS category. Default: `NULL`. (Backup value: `"Very Severe"`)
#'
#' @return A data frame (`.dataset`) with additional columns:
#'   - `fsl_fc_cell`: A numeric column indicating the classification cell derived from FEWS NET lookup tables.
#'   - `fsl_fc_phase`: A character column indicating the FCM phase classification.
#'
#' @details
#' The function implements the FEWS NET methodology for classifying households or populations across phases of food insecurity based on a matrix of food consumption and livelihood coping indicators:
#' - Food consumption scores (FCS): Represents food security based on dietary diversity, food frequency, and relative nutritional importance of food groups.
#' - Reduced Coping Strategy Index (rCSI): Represents the severity of food access-related coping strategies employed by households.
#' - Household Hunger Score (HHS): Measures household hunger levels.
#' - Household Dietary Diversity Score (HDDS): Represents dietary diversity.
#'
#' The process involves:
#' 1. Setting default values for the `_val` arguments if passed as `NULL`.
#' 2. Using embedded lookup tables specifying relationships between FCS, rCSI, HHS, and HDDS categories to classification cells and phases.
#' 3. Validating expected values for category columns (dynamically based on user-provided or default `_val` arguments).
#' 4. Enriching the `dataset` with derived columns through matrix classification logic.
#'
#' @section Categories:
#' The function supports dynamically passed categories but uses sensible defaults for classification:
#' - FCS: `"Acceptable"`, `"Borderline"`, `"Poor"`.
#' - rCSI: `"Low"`, `"Medium"`, `"High"`.
#' - HDDS: `"Low"`, `"Medium"`, `"High"`.
#' - HHS: `"None"`, `"Little"`, `"Moderate"`, `"Severe"`, `"Very Severe"`.
#'
#' @section Outputs:
#' - `fsl_fc_cell`: A numeric column representing the classification cell.
#' - `fsl_fc_phase`: A character column representing the FCM phase classification (e.g., `"P1"`, `"P2"`, etc.).
#'
#' @examples
#' # Example dataset
#' example_data <- tibble::tibble(
#'   fsl_fcs_cat = c("Acceptable", "Borderline", "Poor"),
#'   fsl_rcsi_cat = c("Low", "Medium", "High"),
#'   fsl_hhs_cat_ipc = c("None", "Little", "Moderate")
#' )
#'
#' # Compute FCM classification with default FEWS NET values
#' output <- add_fcm_phase(
#'   .dataset = example_data
#' )
#'
#' # Compute FCM classification with custom category values
#' output <- add_fcm_phase(
#'   .dataset = example_data,
#'   fcs_acceptable_val = "Good",
#'   fcs_poor_val = "Bad"
#' )
#'
#' @seealso [dplyr::left_join()] for joining the dataset with lookup tables.
#' @export
add_fcm_phase <- function(
    .dataset,
    fcs_col  = "fsl_fcs_cat",
    rcsi_col = "fsl_rcsi_cat",
    hhs_col  = "fsl_hhs_cat_ipc",
    hdds_col = "fsl_hdds_cat",
    fcs_acceptable_val = NULL,
    fcs_borderline_val = NULL,
    fcs_poor_val = NULL,
    rcsi_low_val = NULL,
    rcsi_medium_val = NULL,
    rcsi_high_val = NULL,
    hdds_low_val = NULL,
    hdds_medium_val = NULL,
    hdds_high_val = NULL,
    hhs_none_val = NULL,
    hhs_little_val = NULL,
    hhs_moderate_val = NULL,
    hhs_severe_val = NULL,
    hhs_very_severe_val = NULL
) {

  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    fcs_acceptable_val <- phrutils::ensure_value(fcs_acceptable_val, "Acceptable")
    fcs_borderline_val <- phrutils::ensure_value(fcs_borderline_val, "Borderline")
    fcs_poor_val <- phrutils::ensure_value(fcs_poor_val, "Poor")
    rcsi_low_val <- phrutils::ensure_value(rcsi_low_val, "Low")
    rcsi_medium_val <- phrutils::ensure_value(rcsi_medium_val, "Medium")
    rcsi_high_val <- phrutils::ensure_value(rcsi_high_val, "High")
    hdds_low_val <- phrutils::ensure_value(hdds_low_val, "Low")
    hdds_medium_val <- phrutils::ensure_value(hdds_medium_val, "Medium")
    hdds_high_val <- phrutils::ensure_value(hdds_high_val, "High")
    hhs_none_val <- phrutils::ensure_value(hhs_none_val, "None")
    hhs_little_val <- phrutils::ensure_value(hhs_little_val, "Little")
    hhs_moderate_val <- phrutils::ensure_value(hhs_moderate_val, "Moderate")
    hhs_severe_val <- phrutils::ensure_value(hhs_severe_val, "Severe")
    hhs_very_severe_val <- phrutils::ensure_value(hhs_very_severe_val, "Very Severe")


    # 1. EMBEDDED LOOKUP TABLES
    lookup_fcs_rcsi <- tibble::tribble(
      ~fcs,          ~rcsi,      ~cell, ~cat,
      fcs_acceptable_val,  rcsi_low_val,       1, "P1",
      fcs_acceptable_val,  rcsi_medium_val,   2, "P2",
      fcs_acceptable_val,  rcsi_high_val,     3, "P3",
      fcs_borderline_val,  rcsi_low_val,      4, "P1",
      fcs_borderline_val,  rcsi_medium_val,   5, "P2",
      fcs_borderline_val,  rcsi_high_val,     6, "P3",
      fcs_poor_val,         rcsi_low_val,     7, "P2",
      fcs_poor_val,         rcsi_medium_val,  8, "P3",
      fcs_poor_val,         rcsi_high_val,    9, "P4",
    )

    lookup_hdds_rcsi <- tibble::tribble(
      ~hdds,     ~rcsi,     ~cell, ~cat,
      hdds_high_val,    rcsi_low_val,        1,   "P1",
      hdds_high_val,    rcsi_medium_val,     2,   "P2",
      hdds_high_val,    rcsi_high_val,       3,   "P3",
      hdds_medium_val,  rcsi_low_val,        4,   "P1",
      hdds_medium_val,  rcsi_medium_val,     5,   "P2",
      hdds_medium_val,  rcsi_high_val,       6,   "P3",
      hdds_low_val,     rcsi_low_val,        7,   "P2",
      hdds_low_val,     rcsi_medium_val,     8,   "P3",
      hdds_low_val,     rcsi_high_val,       9,   "P4"
    )

    lookup_hhs_fcs <- tibble::tribble(
      ~hhs,          ~fcs,          ~cell, ~cat,
      hhs_none_val,        fcs_acceptable_val,     1, "P1",
      hhs_none_val,        fcs_borderline_val,     6, "P1",
      hhs_none_val,        fcs_poor_val,           11, "P2",
      hhs_little_val,      fcs_acceptable_val,     2, "P2",
      hhs_little_val,      fcs_borderline_val,     7, "P2",
      hhs_little_val,      fcs_poor_val,           12, "P3",
      hhs_moderate_val,    fcs_acceptable_val,     3, "P2",
      hhs_moderate_val,    fcs_borderline_val,     8, "P3",
      hhs_moderate_val,    fcs_poor_val,           13, "P3",
      hhs_severe_val,      fcs_acceptable_val,    4, "P3",
      hhs_severe_val,      fcs_borderline_val,    9, "P4",
      hhs_severe_val,      fcs_poor_val,          14, "P4",
      hhs_very_severe_val, fcs_acceptable_val,    5, "P4",
      hhs_very_severe_val, fcs_borderline_val,    10, "P4",
      hhs_very_severe_val, fcs_poor_val,          15, "P5"
    )

    lookup_hhs_hdds <- tibble::tribble(
      ~hhs,          ~hdds,      ~cell, ~cat,
      hhs_none_val,        hdds_high_val,        1, "P1",
      hhs_none_val,        hdds_medium_val,      6, "P1",
      hhs_none_val,        hdds_low_val,         11, "P2",
      hhs_little_val,      hdds_high_val,        2, "P2",
      hhs_little_val,      hdds_medium_val,      7, "P2",
      hhs_little_val,      hdds_low_val,         12, "P3",
      hhs_moderate_val,    hdds_high_val,        3, "P2",
      hhs_moderate_val,    hdds_medium_val,      8, "P3",
      hhs_moderate_val,    hdds_low_val,         13, "P3",
      hhs_severe_val,      hdds_high_val,       4, "P3",
      hhs_severe_val,      hdds_medium_val,     9, "P4",
      hhs_severe_val,      hdds_low_val,        14, "P4",
      hhs_very_severe_val, hdds_high_val,       15, "P4",
      hhs_very_severe_val, hdds_medium_val,     10, "P4",
      hhs_very_severe_val, hdds_low_val,        15, "P5"
    )

    lookup_hhs_rcsi_fcs_sorted2 <- tibble::tribble(
      ~hhs,          ~rcsi,      ~fcs,          ~cell, ~cat,

      # HHS = None
      hhs_none_val,        rcsi_low_val,      fcs_acceptable_val,   1, "P1",
      hhs_none_val,        rcsi_low_val,      fcs_borderline_val,   6, "P1",
      hhs_none_val,        rcsi_low_val,      fcs_poor_val,         11, "P2",

      hhs_none_val,        rcsi_medium_val,   fcs_acceptable_val,   16, "P2",
      hhs_none_val,        rcsi_medium_val,   fcs_borderline_val,   21, "P2",
      hhs_none_val,        rcsi_medium_val,   fcs_poor_val,         26, "P2",

      hhs_none_val,        rcsi_high_val,     fcs_acceptable_val,   31, "P2",
      hhs_none_val,        rcsi_high_val,     fcs_borderline_val,   36, "P2",
      hhs_none_val,        rcsi_high_val,     fcs_poor_val,         41, "P3",


      # HHS = Little
      hhs_little_val,      rcsi_low_val,      fcs_acceptable_val,  2, "P2",
      hhs_little_val,      rcsi_low_val,      fcs_borderline_val,  7, "P2",
      hhs_little_val,      rcsi_low_val,      fcs_poor_val,        12, "P2",

      hhs_little_val,      rcsi_medium_val,   fcs_acceptable_val,  17, "P2",
      hhs_little_val,      rcsi_medium_val,   fcs_borderline_val,  22, "P2",
      hhs_little_val,      rcsi_medium_val,   fcs_poor_val,        27, "P3",

      hhs_little_val,      rcsi_high_val,     fcs_acceptable_val,  32, "P2",
      hhs_little_val,      rcsi_high_val,     fcs_borderline_val,  37, "P3",
      hhs_little_val,      rcsi_high_val,     fcs_poor_val,        42, "P3",


      # HHS = Moderate
      hhs_moderate_val,    rcsi_low_val,      fcs_acceptable_val,  3, "P2",
      hhs_moderate_val,    rcsi_low_val,      fcs_borderline_val,  8, "P3",
      hhs_moderate_val,    rcsi_low_val,      fcs_poor_val,        13, "P3",

      hhs_moderate_val,    rcsi_medium_val,   fcs_acceptable_val,  18, "P2",
      hhs_moderate_val,    rcsi_medium_val,   fcs_borderline_val,  23, "P3",
      hhs_moderate_val,    rcsi_medium_val,   fcs_poor_val,        28, "P3",

      hhs_moderate_val,    rcsi_high_val,     fcs_acceptable_val,  33, "P3",
      hhs_moderate_val,    rcsi_high_val,     fcs_borderline_val,  38, "P3",
      hhs_moderate_val,    rcsi_high_val,     fcs_poor_val,        43, "P3",


      # HHS = Severe
      hhs_severe_val,      rcsi_low_val,      fcs_acceptable_val,  4, "P3",
      hhs_severe_val,      rcsi_low_val,      fcs_borderline_val,  9, "P3",
      hhs_severe_val,      rcsi_low_val,      fcs_poor_val,        14, "P4",

      hhs_severe_val,      rcsi_medium_val,   fcs_acceptable_val,  19, "P3",
      hhs_severe_val,      rcsi_medium_val,   fcs_borderline_val,  24, "P3",
      hhs_severe_val,      rcsi_medium_val,   fcs_poor_val,        29, "P4",

      hhs_severe_val,      rcsi_high_val,     fcs_acceptable_val,  34, "P3",
      hhs_severe_val,      rcsi_high_val,     fcs_borderline_val,  39, "P4",
      hhs_severe_val,      rcsi_high_val,     fcs_poor_val,        44, "P4",


      # HHS = Very Severe
      hhs_very_severe_val, rcsi_low_val,      fcs_acceptable_val,  5, "P3",
      hhs_very_severe_val, rcsi_low_val,      fcs_borderline_val,  10, "P4",
      hhs_very_severe_val, rcsi_low_val,      fcs_poor_val,        15, "P4",

      hhs_very_severe_val, rcsi_medium_val,   fcs_acceptable_val,  20, "P3",
      hhs_very_severe_val, rcsi_medium_val,   fcs_borderline_val,  25, "P4",
      hhs_very_severe_val, rcsi_medium_val,   fcs_poor_val,        30, "P5",

      hhs_very_severe_val, rcsi_high_val,     fcs_acceptable_val,  35, "P4",
      hhs_very_severe_val, rcsi_high_val,     fcs_borderline_val,  40, "P4",
      hhs_very_severe_val, rcsi_high_val,     fcs_poor_val,        45, "P5"
    )



    # 2. PRECONDITIONS


    phrutils::phr_validate_dataframe(
      .dataset,
      origin = "add_fcm_phase",
      soft = FALSE
    )

    phrutils::phr_assert(
      nrow(.dataset) > 0,
      origin = "add_fcm_phase"
    )



    # 3. DETECT AVAILABLE INDICATORS


    has_fcs  <- fcs_col  %in% names(.dataset)
    has_rcsi <- rcsi_col %in% names(.dataset)
    has_hhs  <- hhs_col  %in% names(.dataset)
    has_hdds <- hdds_col %in% names(.dataset)



    # Overwrite warnings for existing output columns (consistent)

    overwrite_vars <- c(
      "fsl_fc_cell",
      "fsl_fc_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        phrutils::phr_warning(
          origin  = "add_fcm_phase",
          message = glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }
    }


    # 4. VALIDATE CATEGORIES using phrutils::phr_validate_choices


    if (has_fcs) {
      phrutils::phr_validate_choice(
        .dataset[[fcs_col]],
        choices = c(fcs_acceptable_val, fcs_borderline_val, fcs_poor_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_rcsi) {
      phrutils::phr_validate_choice(
        .dataset[[rcsi_col]],
        choices = c(rcsi_low_val, rcsi_medium_val, rcsi_high_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_hhs) {
      phrutils::phr_validate_choice(
        .dataset[[hhs_col]],
        choices = c(hhs_none_val, hhs_little_val, hhs_moderate_val, hhs_severe_val, hhs_very_severe_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_hdds) {
      phrutils::phr_validate_choice(
        .dataset[[hdds_col]],
        choices = c(hdds_low_val, hdds_medium_val, hdds_high_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }



    # 5. SELECT CORRECT LOOKUP TABLE


    if (has_fcs && has_rcsi && has_hhs) {

      lookup <- lookup_hhs_rcsi_fcs_sorted2

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col)  == hhs,
        !!rlang::sym(rcsi_col) == rcsi,
        !!rlang::sym(fcs_col)  == fcs
      )

    } else if (has_hdds && has_rcsi && !has_fcs && !has_hhs) {

      lookup <- lookup_hdds_rcsi

      join_condition <- dplyr::join_by(
        !!rlang::sym(hdds_col) == hdds,
        !!rlang::sym(rcsi_col) == rcsi
      )

    } else if (has_fcs && has_rcsi && !has_hhs) {

      lookup <- lookup_fcs_rcsi

      join_condition <- dplyr::join_by(
        !!rlang::sym(fcs_col)  == fcs,
        !!rlang::sym(rcsi_col) == rcsi
      )

    } else if (has_fcs && has_hhs && !has_rcsi) {

      lookup <- lookup_hhs_fcs

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col) == hhs,
        !!rlang::sym(fcs_col) == fcs
      )

    } else if (has_hdds && has_hhs && !has_rcsi && !has_fcs) {

      lookup <- lookup_hhs_hdds

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col)  == hhs,
        !!rlang::sym(hdds_col) == hdds
      )

    } else {
      phrutils::phr_error(
        origin = "add_fcm_phase",
        "No valid FEWS NET indicator combination detected.",
        hint = "Provide at least one valid combination: (FCS+rCSI), (HDDS+rCSI), (FCS+HHS), (HDDS+HHS), or (FCS+rCSI+HHS)."
      )
    }



    # 6. JOIN LOOKUP TABLE

    # Preserve factor attributes of join key columns before left_join
    # (dplyr::left_join coerces factor columns to character when joining with character lookup)
    join_key_cols <- intersect(c(fcs_col, rcsi_col, hhs_col, hdds_col), names(.dataset))
    factor_attrs <- Filter(Negate(is.null), lapply(join_key_cols, function(col) {
      if (is.factor(.dataset[[col]])) {
        list(col = col, levels = levels(.dataset[[col]]), ordered = is.ordered(.dataset[[col]]))
      } else NULL
    }))

    out <- .dataset |>
      dplyr::left_join(
        lookup,
        by = join_condition
      )



    # 7. OUTPUT COLUMNS


    out <- out |>
      dplyr::rename(
        fsl_fc_cell = cell,
        fsl_fc_phase  = cat
      ) |>
      dplyr::mutate(
        fsl_fc_phase = factor(fsl_fc_phase, levels = c("P5", "P4", "P3", "P2", "P1"), ordered = TRUE)
      )

    # Restore factor attributes lost during dplyr::left_join type coercion
    for (fi in factor_attrs) {
      if (fi$col %in% names(out)) {
        out[[fi$col]] <- factor(out[[fi$col]], levels = fi$levels, ordered = fi$ordered)
      }
    }

    phrutils::phr_message(
      "FEWS NET matrix classification computed: fsl_fc_cell, fsl_fc_phase."
    )

    return(out)

  }, on_error = "abort", origin = "add_fcm_phase")
}
