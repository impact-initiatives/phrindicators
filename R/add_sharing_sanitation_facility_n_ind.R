#' Add Number of People Sharing a Sanitation Facility Indicator
#'
#' @description
#' Estimates how many people share each household's sanitation facility and
#' buckets that estimate into an ordinal indicator.
#'
#' For households that report sharing, the reported number of households sharing
#' the facility (`sanitation_facility_sharing_n`) is converted to a number of
#' people using the (optionally weighted) mean household size of the dataset, and
#' the household's own size is added back exactly:
#' `(sanitation_facility_sharing_n - 1) * mean_hh_size + hh_size`. Households that
#' report not sharing are assigned their own size. Every other response
#' (`not_applicable`, `undefined`, missing) yields `NA`.
#'
#' @param .dataset Input data frame or tibble.
#' @param sharing_sanitation_facility_cat Column name (as a string) for the
#'   shared-sanitation category. Defaults to
#'   `"wash_sharing_sanitation_facility_cat"`.
#' @param sharing_sanitation_facility_cat_shared Value in
#'   `sharing_sanitation_facility_cat` meaning the facility is shared with other
#'   households. Defaults to `"shared"`.
#' @param sharing_sanitation_facility_cat_not_shared Value meaning the facility is
#'   not shared. Defaults to `"not_shared"`.
#' @param sharing_sanitation_facility_cat_not_applicable Value meaning the
#'   question does not apply (e.g. open defecation). Defaults to
#'   `"not_applicable"`.
#' @param sharing_sanitation_facility_cat_undefined Value meaning an undefined /
#'   "don't know" / "prefer not to answer" response. Defaults to `"undefined"`.
#' @param sanitation_facility_sharing_n Column name (as a string) for the reported
#'   number of households sharing the facility (including the respondent's own).
#'   Must be numeric. Defaults to `"wash_sanitation_facility_sharing_n"`.
#' @param hh_size Column name (as a string) for household size. Must be numeric
#'   and strictly positive. Defaults to `"hh_size"`.
#' @param weight Column name (as a string) for the survey weight used when
#'   computing the mean household size, or `NULL` for an unweighted mean. If the
#'   named column is absent the mean is computed unweighted with a warning.
#'   Defaults to `"weight"`.
#'
#' @details
#' Number of people sharing (`wash_sharing_sanitation_facility_n`):
#' * `shared` -> `(sanitation_facility_sharing_n - 1) * mean_hh_size + hh_size`
#' * `not_shared` -> `hh_size`
#' * anything else -> `NA`
#'
#' where `mean_hh_size` is `stats::weighted.mean(hh_size, weight)` over all rows
#' (unweighted if `weight` is `NULL` or absent).
#'
#' The people estimate is then bucketed into
#' `wash_sharing_sanitation_facility_n_ind`:
#' * `>= 50` -> `"50_and_above"`
#' * `20`-`49` -> `"20_to_49"`
#' * `0`-`19` -> `"19_and_below"`
#' * `< 0` or `NA` -> `NA`
#'
#' @return `.dataset` with two added columns:
#'   `wash_sharing_sanitation_facility_n` (numeric estimate of people sharing the
#'   facility) and `wash_sharing_sanitation_facility_n_ind` (ordered factor with
#'   levels `"19_and_below"`, `"20_to_49"`, `"50_and_above"`).
#'
#' @examples
#' df <- data.frame(
#'   wash_sharing_sanitation_facility_cat = c("shared", "not_shared", "not_applicable", "shared"),
#'   wash_sanitation_facility_sharing_n = c(10, NA, NA, 2),
#'   hh_size = c(5, 4, 6, 5),
#'   weight = c(1, 1, 1, 1)
#' )
#' add_sharing_sanitation_facility_n_ind(df)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @importFrom stats weighted.mean
#' @export
add_sharing_sanitation_facility_n_ind <- function(
    .dataset,
    sharing_sanitation_facility_cat = "wash_sharing_sanitation_facility_cat",
    sharing_sanitation_facility_cat_shared = "shared",
    sharing_sanitation_facility_cat_not_shared = "not_shared",
    sharing_sanitation_facility_cat_not_applicable = "not_applicable",
    sharing_sanitation_facility_cat_undefined = "undefined",
    sanitation_facility_sharing_n = "wash_sanitation_facility_sharing_n",
    hh_size = "hh_size",
    weight = "weight"
) {
  origin <- "add_sharing_sanitation_facility_n_ind"

  phrutils::phr_try(
    expr = {

      # Value-defining (non-column-name) parameters

      sharing_sanitation_facility_cat_shared <-
        phrutils::ensure_value(sharing_sanitation_facility_cat_shared, "shared")
      sharing_sanitation_facility_cat_not_shared <-
        phrutils::ensure_value(sharing_sanitation_facility_cat_not_shared, "not_shared")
      sharing_sanitation_facility_cat_not_applicable <-
        phrutils::ensure_value(sharing_sanitation_facility_cat_not_applicable, "not_applicable")
      sharing_sanitation_facility_cat_undefined <-
        phrutils::ensure_value(sharing_sanitation_facility_cat_undefined, "undefined")


      # Basic dataset checks

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


      # Required columns

      phrutils::phr_validate_columns(
        .dataset,
        c(sharing_sanitation_facility_cat, sanitation_facility_sharing_n, hh_size),
        origin = origin,
        hint = ("Ensure sharing_sanitation_facility_cat, sanitation_facility_sharing_n and hh_size all exist in `.dataset`."),
        soft = FALSE
      )


      # Validate categorical input

      levels <- c(
        sharing_sanitation_facility_cat_shared,
        sharing_sanitation_facility_cat_not_shared,
        sharing_sanitation_facility_cat_not_applicable,
        sharing_sanitation_facility_cat_undefined
      )

      phrutils::phr_validate_choice(
        .dataset[[sharing_sanitation_facility_cat]],
        choices = c(levels, NA_character_),
        origin = origin,
        soft = FALSE
      )


      # Validate numeric inputs

      phrutils::phr_validate_all_numeric(
        .dataset[[sanitation_facility_sharing_n]],
        origin = origin,
        hint = (glue::glue("Column {sanitation_facility_sharing_n} must be numeric.")),
        soft = FALSE
      )

      phrutils::phr_validate_all_numeric(
        .dataset[[hh_size]],
        origin = origin,
        hint = (glue::glue("Column {hh_size} must be numeric.")),
        soft = FALSE
      )

      phrutils::phr_assert(
        !any(.dataset[[hh_size]] <= 0, na.rm = TRUE),
        origin = origin,
        (glue::glue("The values in {hh_size} must be strictly above 0."))
      )


      # Mean household size (weighted, with unweighted fallback)

      if (is.null(weight) || !(weight %in% names(.dataset))) {
        if (!is.null(weight) && !(weight %in% names(.dataset))) {
          phrutils::phr_warning(
            origin = origin,
            message = (glue::glue("Weight column {weight} not found; computing an unweighted mean household size."))
          )
        }
        w <- rep(1, nrow(.dataset))
      } else {
        phrutils::phr_validate_all_numeric(
          .dataset[[weight]],
          origin = origin,
          hint = (glue::glue("Column {weight} must be numeric.")),
          soft = FALSE
        )
        w <- .dataset[[weight]]
      }

      mean_hh_size <- stats::weighted.mean(.dataset[[hh_size]], w, na.rm = TRUE)

      phrutils::phr_assert(
        is.finite(mean_hh_size),
        origin = origin,
        (glue::glue("Could not compute a finite mean household size from {hh_size}."))
      )


      # Overwrite warnings for output columns

      people_col <- "wash_sharing_sanitation_facility_n"
      output_col <- "wash_sharing_sanitation_facility_n_ind"

      for (col in c(people_col, output_col)) {
        if (col %in% names(.dataset)) {
          phrutils::phr_warning(
            origin = origin,
            message = (glue::glue("Column {col} already exists and will be overwritten."))
          )
        }
      }


      # Estimate number of people sharing the facility

      .dataset <- dplyr::mutate(
        .dataset,
        !!people_col := dplyr::case_when(
          .data[[sharing_sanitation_facility_cat]] == sharing_sanitation_facility_cat_shared ~
            (.data[[sanitation_facility_sharing_n]] - 1) * mean_hh_size + .data[[hh_size]],
          .data[[sharing_sanitation_facility_cat]] == sharing_sanitation_facility_cat_not_shared ~
            as.numeric(.data[[hh_size]]),
          .default = NA_real_
        )
      )


      # Bucket into the ordinal indicator

      .dataset <- dplyr::mutate(
        .dataset,
        !!output_col := factor(
          dplyr::case_when(
            .data[[people_col]] >= 50 ~ "50_and_above",
            .data[[people_col]] >= 20 ~ "20_to_49",
            .data[[people_col]] >= 0  ~ "19_and_below",
            .default = NA_character_
          ),
          levels = c("19_and_below", "20_to_49", "50_and_above"),
          ordered = TRUE
        )
      )

      phrutils::phr_message(
        origin = origin,
        message = (glue::glue("Shared-sanitation people-count columns successfully added: {people_col}, {output_col}."))
      )

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = ("Ensure input columns and values exist and align with specifications.")
  )
}
