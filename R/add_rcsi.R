#' Calculate Reduced Coping Strategies Index (rCSI)
#'
#' @description
#' Calculates rCSI score and category based on five coping strategy questions
#' with weighted frequencies.
#'
#' @param .dataset Data frame containing rCSI component variables
#' @param fsl_rcsi_lessquality Character: Column for "less quality food" frequency (0-7 days)
#' @param fsl_rcsi_borrow Character: Column for "borrow food" frequency (0-7 days)
#' @param fsl_rcsi_mealsize Character: Column for "reduce meal size" frequency (0-7 days)
#' @param fsl_rcsi_mealadult Character: Column for "adults eat less" frequency (0-7 days)
#' @param fsl_rcsi_mealnb Character: Column for "reduce meal number" frequency (0-7 days)
#'
#' @return Data frame with added columns: fsl_rcsi_score, fsl_rcsi_cat
#' @export
add_rcsi <- function(.dataset,
                     fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                     fsl_rcsi_borrow = "fsl_rcsi_borrow",
                     fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                     fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                     fsl_rcsi_mealnb = "fsl_rcsi_mealnb") {

  origin <- "add_rcsi"

  phrutils::phr_try(

    expr = {

      # Basic dataset checks

      phrutils::phr_validate_dataframe(.dataset, origin, soft = FALSE)

      phrutils::phr_assert(
        nrow(.dataset) > 0,
        origin,
        "Dataset is empty."
      )

      rcsi_vars <- c(
        fsl_rcsi_lessquality,
        fsl_rcsi_borrow,
        fsl_rcsi_mealsize,
        fsl_rcsi_mealadult,
        fsl_rcsi_mealnb
      )

      phrutils::phr_validate_columns(
        .dataset,
        rcsi_vars,
        origin,
        hint = "Ensure all required RCSI columns are present.",
        soft = FALSE
      )


      # Overwrite warnings

      if ("fsl_rcsi_score" %in% names(.dataset)) {
        var <- "fsl_rcsi_score"
        phrutils::phr_warning(
          origin,
          glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }

      if ("fsl_rcsi_cat" %in% names(.dataset)) {
        var <- "fsl_rcsi_cat"
        phrutils::phr_warning(
          origin,
          glue::glue("Variable {var} already exists and will be overwritten.")
        )
      }


      # Enforce allowed 0\u20137 range using phrutils::phr_validate_range()

      for (v in rcsi_vars) {
        phrutils::phr_validate_all_numeric(.dataset[[v]], origin, soft = TRUE)

        phrutils::phr_validate_range(
          df  = .dataset,
          col = v,
          min = 0,
          max = 7,
          origin = origin,
          hint = "RCSI frequency values must be between 0 and 7.",
          soft = TRUE
        )
      }


      # Main computation
      # Treat out-of-range values (< 0 or > 7) as NA during calculation
      # but preserve original values

      rcs_columns <- rcsi_vars

      .dataset <- .dataset |>
        dplyr::mutate_at(dplyr::vars(rcs_columns), as.numeric) |>
        dplyr::mutate(
          rcsi_lessquality_weighted = ifelse(
            is.na(.data[[fsl_rcsi_lessquality]]) | .data[[fsl_rcsi_lessquality]] < 0 | .data[[fsl_rcsi_lessquality]] > 7,
            NA,
            .data[[fsl_rcsi_lessquality]] * 1
          ),
          rcsi_borrow_weighted = ifelse(
            is.na(.data[[fsl_rcsi_borrow]]) | .data[[fsl_rcsi_borrow]] < 0 | .data[[fsl_rcsi_borrow]] > 7,
            NA,
            .data[[fsl_rcsi_borrow]] * 2
          ),
          rcsi_mealsize_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealsize]]) | .data[[fsl_rcsi_mealsize]] < 0 | .data[[fsl_rcsi_mealsize]] > 7,
            NA,
            .data[[fsl_rcsi_mealsize]] * 1
          ),
          rcsi_mealadult_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealadult]]) | .data[[fsl_rcsi_mealadult]] < 0 | .data[[fsl_rcsi_mealadult]] > 7,
            NA,
            .data[[fsl_rcsi_mealadult]] * 3
          ),
          rcsi_mealnb_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealnb]]) | .data[[fsl_rcsi_mealnb]] < 0 | .data[[fsl_rcsi_mealnb]] > 7,
            NA,
            .data[[fsl_rcsi_mealnb]] * 1
          ),

          # Check if any weighted columns are NA
          any_weighted_na = is.na(rcsi_lessquality_weighted) |
            is.na(rcsi_borrow_weighted) |
            is.na(rcsi_mealsize_weighted) |
            is.na(rcsi_mealadult_weighted) |
            is.na(rcsi_mealnb_weighted),

          fsl_rcsi_score = ifelse(
            any_weighted_na,
            NA_real_,
            rowSums(
              dplyr::across(
                c(
                  rcsi_lessquality_weighted,
                  rcsi_borrow_weighted,
                  rcsi_mealsize_weighted,
                  rcsi_mealadult_weighted,
                  rcsi_mealnb_weighted
                ),
                as.numeric
              ),
              na.rm = TRUE
            )
          ),

          # Calculate standard deviation of days across all food groups
          sd_rcsicoping = ifelse(
            any_weighted_na,
            NA_real_,
            apply(
              dplyr::across(c(
                rcsi_lessquality_weighted,
                rcsi_borrow_weighted,
                rcsi_mealsize_weighted,
                rcsi_mealadult_weighted,
                rcsi_mealnb_weighted
              )), 1, stats::sd, na.rm = TRUE
            )
          ),

          fsl_rcsi_cat = dplyr::case_when(
            any_weighted_na ~ NA_character_,
            fsl_rcsi_score <= 3 ~ "No to Low",
            fsl_rcsi_score <= 18 ~ "Medium",
            fsl_rcsi_score > 18 ~ "High",
            TRUE ~ NA_character_
          ),

          # Standardize factor levels
          fsl_rcsi_cat = factor(
            fsl_rcsi_cat,
            levels = c(
              "High",
              "Medium",
              "No to Low"
            )
          )
        ) |>
        dplyr::select(-any_weighted_na)  # Remove helper column

      phrutils::phr_message(origin, "RCSI successfully calculated.")

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = "Ensure all RCSI fields exist and contain integer values 0\u20137."
  )
}
