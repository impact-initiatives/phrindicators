#' Compute FCLCM Phase Classification
#'
#' The `add_fclcm_phase` function computes the Food Consumption and Livelihood Coping Mechanism (FCLCM) composite classifications based on user-specified or default categorical values for FC phase and LCSI categories. It joins the dataset with predefined lookup tables to create two new columns: `fsl_fclcm_cell` (classification cell) and `fsl_fclcm_phase` (classification phase).
#' This function implements the methodology as outlined in the [FEWS NET Matrix Guidance Document](https://fews.net/sites/default/files/documents/reports/fews-net-matrix-guidance-document.pdf).
#'
#' @param .dataset A data frame containing the FC phase and LCSI category columns.
#' @param fc_phase_col A character string specifying the column name for the FC phase. Default: `"fsl_fc_phase"`.
#' @param lcsi_col A character string specifying the column name for the LCSI category. Default: `"fsl_lcsi_cat"`.
#' @param p1_val A character string for the "P1" FC phase. Default: `NULL`. (Backup value: `"P1"`.)
#' @param p2_val A character string for the "P2" FC phase. Default: `NULL`. (Backup value: `"P2"`.)
#' @param p3_val A character string for the "P3" FC phase. Default: `NULL`. (Backup value: `"P3"`.)
#' @param p4_val A character string for the "P4" FC phase. Default: `NULL`. (Backup value: `"P4"`.)
#' @param p5_val A character string for the "P5" FC phase. Default: `NULL`. (Backup value: `"P5"`.)
#' @param lcsi_none_val A character string for the "None" LCSI category. Default: `NULL`. (Backup value: `"None"`.)
#' @param lcsi_stress_val A character string for the "Stress" LCSI category. Default: `NULL`. (Backup value: `"Stress"`.)
#' @param lcsi_crisis_val A character string for the "Crisis" LCSI category. Default: `NULL`. (Backup value: `"Crisis"`.)
#' @param lcsi_emergency_val A character string for the "Emergency" LCSI category. Default: `NULL`. (Backup value: `"Emergency"`.)
#' @param lcsi_exhaustion_val A character string for the "Exhaustion" LCSI category. Default: `NULL`. (Backup value: `"Exhaustion"`.)
#'
#' @return A data frame (`.dataset`) with two additional columns:
#'   - `fsl_fclcm_cell`: Classification cell based on the lookup tables.
#'   - `fsl_fclcm_phase`: Classification phase based on the lookup tables.
#'
#' @details
#' The function performs the following steps:
#' 1. Sets default values for the `_val` arguments if they are NULL.
#' 2. Defines two lookup tables:
#'    - `lookup_fclcm_4`: For cases where the LCSI category excludes "Exhaustion".
#'    - `lookup_fclcm_5`: For cases where the LCSI category includes "Exhaustion".
#' 3. Validates that the `.dataset` contains the required columns (`fc_phase_col` and `lcsi_col`).
#' 4. Joins the `lookup` table with `.dataset` by matching the `fc_phase_col` and `lcsi_col` values.
#' 5. Returns the modified `.dataset` with the newly added classification columns.
#'
#' @section Validation:
#' The function validates the following:
#' - That `.dataset` is a valid data frame.
#' - That `fc_phase_col` and `lcsi_col` exist in `.dataset`.
#' - That the values in `fc_phase_col` and `lcsi_col` match the allowable choices specified in the lookup tables (`p1_val` to `p5_val` for FC phases, and `lcsi_*_val` for LCSI categories).
#'
#' @section Outputs:
#' - `fsl_fclcm_cell`: A numeric column indicating the classification cell as defined in the lookup table.
#' - `fsl_fclcm_phase`: A character column indicating the classification phase (e.g., `"P1"`, `"P2"`, etc.).
#'
#' @examples
#' # Example data
#' example_data <- tibble::tibble(
#'   fsl_fc_phase = c("P1", "P2", "P3", "P4", "P5"),
#'   fsl_lcsi_cat = c("None", "Stress", "Crisis", "Emergency", "Exhaustion")
#' )
#'
#' # Compute FCLCM classification using default values
#' output <- add_fclcm_phase(
#'   .dataset = example_data
#' )
#'
#' # Compute FCLCM classification with customized values
#' output <- add_fclcm_phase(
#'   .dataset = example_data,
#'   p1_val = "Phase 1",
#'   p2_val = "Phase 2",
#'   lcsi_stress_val = "Stressed"
#' )
#'
#' @seealso [dplyr::left_join()]
#' @export
add_fclcm_phase <- function(
    .dataset,
    fc_phase_col = "fsl_fc_phase",
    lcsi_col     = "fsl_lcsi_cat",
    p1_val       = NULL,
    p2_val       = NULL,
    p3_val       = NULL,
    p4_val       = NULL,
    p5_val       = NULL,
    lcsi_none_val      = NULL,
    lcsi_stress_val    = NULL,
    lcsi_crisis_val    = NULL,
    lcsi_emergency_val = NULL,
    lcsi_exhaustion_val = NULL
) {
  phrutils::phr_try({

    # Use ensure_value for all *_val parameters
    p1_val <- phrutils::ensure_value(p1_val, "P1")
    p2_val <- phrutils::ensure_value(p2_val, "P2")
    p3_val <- phrutils::ensure_value(p3_val, "P3")
    p4_val <- phrutils::ensure_value(p4_val, "P4")
    p5_val <- phrutils::ensure_value(p5_val, "P5")
    lcsi_none_val <- phrutils::ensure_value(lcsi_none_val, "None")
    lcsi_stress_val <- phrutils::ensure_value(lcsi_stress_val, "Stress")
    lcsi_crisis_val <- phrutils::ensure_value(lcsi_crisis_val, "Crisis")
    lcsi_emergency_val <- phrutils::ensure_value(lcsi_emergency_val, "Emergency")
    lcsi_exhaustion_val <- phrutils::ensure_value(lcsi_exhaustion_val, "Exhaustion")

    # 1. EMBEDDED LOOKUP TABLES
    lookup_fclcm_4 <- dplyr::tribble(
      ~fc_phase, ~lcsi,        ~cell, ~cat,
      # None: 1\u20135
      p1_val, lcsi_none_val,        1,  p1_val,
      p2_val, lcsi_none_val,        2,  p2_val,
      p3_val, lcsi_none_val,        3,  p3_val,
      p4_val, lcsi_none_val,        4,  p4_val,
      p5_val, lcsi_none_val,        5,  p5_val,

      # Stress: 6\u201310
      p1_val, lcsi_stress_val,      6,  p1_val,
      p2_val, lcsi_stress_val,      7,  p2_val,
      p3_val, lcsi_stress_val,      8,  p3_val,
      p4_val, lcsi_stress_val,      9,  p4_val,
      p5_val, lcsi_stress_val,     10,  p5_val,

      # Crisis: 11\u201315
      p1_val, lcsi_crisis_val,     11,  p1_val,
      p2_val, lcsi_crisis_val,     12,  p2_val,
      p3_val, lcsi_crisis_val,     13,  p3_val,
      p4_val, lcsi_crisis_val,     14,  p4_val,
      p5_val, lcsi_crisis_val,     15,  p5_val,

      # Emergency: 16\u201320
      p1_val, lcsi_emergency_val,  16,  p1_val,
      p2_val, lcsi_emergency_val,  17,  p2_val,
      p3_val, lcsi_emergency_val,  18,  p3_val,
      p4_val, lcsi_emergency_val,  19,  p4_val,
      p5_val, lcsi_emergency_val,  20,  p5_val
    )

    # -------- 5-category LCSI (+ Exhaustion)
    lookup_fclcm_5 <- dplyr::tribble(
      ~fc_phase, ~lcsi,        ~cell, ~cat,
      # None: 1\u20135
      p1_val, lcsi_none_val,       1, p1_val,
      p2_val, lcsi_none_val,       2, p2_val,
      p3_val, lcsi_none_val,       3, p3_val,
      p4_val, lcsi_none_val,       4, p4_val,
      p5_val, lcsi_none_val,       5, p5_val,

      # Stress: 6\u201310
      p1_val, lcsi_stress_val,     6, p1_val,
      p2_val, lcsi_stress_val,     7, p2_val,
      p3_val, lcsi_stress_val,     8, p3_val,
      p4_val, lcsi_stress_val,     9, p4_val,
      p5_val, lcsi_stress_val,    10, p5_val,

      # Crisis: 11\u201315
      p1_val, lcsi_crisis_val,    11, p1_val,
      p2_val, lcsi_crisis_val,    12, p2_val,
      p3_val, lcsi_crisis_val,    13, p3_val,
      p4_val, lcsi_crisis_val,    14, p4_val,
      p5_val, lcsi_crisis_val,    15, p5_val,

      # Emergency: 16\u201320
      p1_val, lcsi_emergency_val, 16, p1_val,
      p2_val, lcsi_emergency_val, 17, p2_val,
      p3_val, lcsi_emergency_val, 18, p3_val,
      p4_val, lcsi_emergency_val, 19, p4_val,
      p5_val, lcsi_emergency_val, 20, p5_val,

      # Exhaustion: 21\u201325
      p1_val, lcsi_exhaustion_val,21, p1_val,
      p2_val, lcsi_exhaustion_val,22, p2_val,
      p3_val, lcsi_exhaustion_val,23, p3_val,
      p4_val, lcsi_exhaustion_val,24, p4_val,
      p5_val, lcsi_exhaustion_val,25, p5_val
    )

    # 2. PRECONDITIONS

    phrutils::phr_validate_dataframe(
      .dataset,
      origin = "add_fclcm_phase",
      hint   = "Input must be a valid dataframe.",
      soft   = FALSE
    )

    phrutils::phr_assert(
      condition = nrow(.dataset) > 0,
      message   = "Dataset is empty; cannot create FCLCM composite.",
      origin    = "add_fclcm_phase",
      hint      = "Provide at least one row of data."
    )

    phrutils::phr_assert(
      condition = fc_phase_col %in% names(.dataset),
      message   = glue::glue("Missing FC phase column '{fc_phase_col}'."),
      origin    = "add_fclcm_phase",
      hint      = "Ensure the dataset contains the FC phase variable."
    )

    phrutils::phr_assert(
      condition = lcsi_col %in% names(.dataset),
      message   = glue::glue("Missing LCSI column '{lcsi_col}'."),
      origin    = "add_fclcm_phase",
      hint      = "Ensure the dataset contains the LCSI variable."
    )

    # 3. CATEGORY VALIDATION

    phrutils::phr_validate_choice(
      .dataset[[fc_phase_col]],
      choices = c(p1_val, p2_val, p3_val, p4_val, p5_val, NA_character_),
      origin  = "add_fclcm_phase",
      soft    = FALSE
    )

    phrutils::phr_validate_choice(
      .dataset[[lcsi_col]],
      choices = c(lcsi_none_val, lcsi_stress_val, lcsi_crisis_val, lcsi_emergency_val, lcsi_exhaustion_val, NA_character_),
      origin  = "add_fclcm_phase",
      soft    = FALSE
    )

    # Overwrite warnings for existing output columns (consistent)

    overwrite_vars <- c(
      "fsl_fclcm_cell",
      "fsl_fclcm_phase"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        phrutils::phr_warning(
          origin  = "add_fclcm_phase",
          message = glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }
    }

    # 4. SELECT LOOKUP TABLE

    has_exhaustion <- lcsi_exhaustion_val %in% unique(stats::na.omit(.dataset[[lcsi_col]]))
    lookup <- if (has_exhaustion) lookup_fclcm_5 else lookup_fclcm_4
    lut_label <- if (has_exhaustion) "5-category LCSI" else "4-category LCSI"

    # 5. JOIN + CREATE OUTPUT

    # Preserve factor attributes of join key columns before left_join
    # (dplyr::left_join coerces factor columns to character when joining with character lookup)
    join_key_cols <- intersect(c(fc_phase_col, lcsi_col), names(.dataset))
    factor_attrs <- Filter(Negate(is.null), lapply(join_key_cols, function(col) {
      if (is.factor(.dataset[[col]])) {
        list(col = col, levels = levels(.dataset[[col]]), ordered = is.ordered(.dataset[[col]]))
      } else NULL
    }))

    out <- .dataset |>
      dplyr::left_join(
        lookup,
        by = dplyr::join_by(
          !!rlang::sym(fc_phase_col) == fc_phase,
          !!rlang::sym(lcsi_col)     == lcsi
        )
      ) |>
      dplyr::rename(
        fsl_fclcm_cell  = cell,
        fsl_fclcm_phase = cat
      ) |>
      dplyr::mutate(
        fsl_fclcm_phase = factor(fsl_fclcm_phase, levels = c("P5", "P4", "P3", "P2", "P1"), ordered = TRUE)
      )

    # Restore factor attributes lost during dplyr::left_join type coercion
    for (fi in factor_attrs) {
      if (fi$col %in% names(out)) {
        out[[fi$col]] <- factor(out[[fi$col]], levels = fi$levels, ordered = fi$ordered)
      }
    }

    phrutils::phr_message(
      "FCLCM composite applied using {lut_label}. Output columns: fsl_fclcm_cell and fsl_fclcm_phase."
    )

    return(out)

  }, on_error = "abort", origin = "add_fclcm_phase")
}
