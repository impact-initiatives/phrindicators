#' Add Drinking Water Collection Time Category to Dataset
#'
#' @description
#' Categorizes households by how long it takes to collect drinking water,
#' reconciling three possible sources of information from the survey:
#' a response-type column (`drinking_water_time_yn`) indicating whether the
#' respondent reported water as on-premises, an exact number of minutes, or
#' "don't know"/"prefer not to answer"; a numeric minutes column
#' (`drinking_water_time_int`) used when an exact number was given; and a
#' categorical skip-logic column (`drinking_water_time_sl`) used as a
#' fallback bucket when the respondent answered "don't know". The
#' household's drinking water source (`drinking_water_source`) can also
#' short-circuit the calculation straight to `"premises"` or `"undefined"`.
#'
#' @param .dataset Input data frame or tibble.
#' @param drinking_water_time_yn Column name (as a string) indicating how
#'   the respondent answered the time-to-fetch-water question. Defaults to
#'   `"wash_drinking_water_time_yn"`.
#' @param water_on_premises Value(s) in `drinking_water_time_yn` indicating
#'   water is collected on premises. Defaults to
#'   `c("water_in_dwelling", "water_in_plot")`.
#' @param number_minutes Value in `drinking_water_time_yn` indicating the
#'   respondent reported an exact number of minutes (found in
#'   `drinking_water_time_int`). Defaults to `"number_minutes"`.
#' @param dnk Value in `drinking_water_time_yn` indicating "don't know" (a
#'   fallback bucket may still be available via `drinking_water_time_sl`).
#'   Defaults to `"dnk"`.
#' @param undefined Value in `drinking_water_time_yn` indicating an
#'   undefined/"prefer not to answer" response. Defaults to `"pnta"`.
#' @param drinking_water_time_int Column name (as a string) for the number
#'   of minutes it takes to fetch water, used when `drinking_water_time_yn`
#'   equals `number_minutes`. Must be numeric and strictly positive; if it
#'   isn't already numeric, it is coerced to integer via
#'   `phrutils::is_safely_coercible()`/`as.integer()`, erroring if the
#'   coercion wouldn't be safe. Defaults to `"wash_drinking_water_time_int"`.
#' @param max Maximum plausible number of minutes for
#'   `drinking_water_time_int`; values above this are treated as `NA`.
#'   Defaults to `600`.
#' @param drinking_water_time_sl Column name (as a string) for the
#'   categorical skip-logic time bucket, used as a fallback when
#'   `drinking_water_time_yn` equals `dnk`. Defaults to
#'   `"wash_drinking_water_time_sl"`.
#' @param sl_under_30_min Value(s) in `drinking_water_time_sl` representing
#'   under 30 minutes - a vector, since some tools use several distinct
#'   range codes for the same bucket (e.g.
#'   `c("5min_or_less", "5min_15min", "15min_30min")`, the default). The
#'   *first* element doubles as the single canonical label written to the
#'   output column for this bucket, whether reached via
#'   `drinking_water_time_sl` or via the numeric `drinking_water_time_int`
#'   path.
#' @param sl_30min_1hr Value(s) in `drinking_water_time_sl` representing 30
#'   minutes to 1 hour (vector; first element is the output label). Defaults
#'   to `"30min_1hr"`.
#' @param sl_more_than_1hr Value(s) in `drinking_water_time_sl` representing
#'   more than 1 hour (vector; first element is the output label). Defaults
#'   to `"more_than_1hr"`.
#' @param sl_undefined Value(s) in `drinking_water_time_sl` indicating an
#'   undefined/don't-know skip-logic response. Defaults to
#'   `c("dnk", "pnta")`.
#' @param drinking_water_source Column name (as a string) for the
#'   household's drinking water source, used to short-circuit the time
#'   calculation (e.g. piped-to-dwelling implies `"premises"` regardless of
#'   the time question). Defaults to `"wash_drinking_water_source"`.
#' @param skipped_drinking_water_source_premises Value(s) in
#'   `drinking_water_source` implying water is collected on premises.
#'   Defaults to `"piped_dwelling"`.
#' @param skipped_drinking_water_source_undefined Value(s) in
#'   `drinking_water_source` implying an undefined water source. Defaults
#'   to `c("dnk", "pnta")`.
#' @param drinking_water_time_under_30min_val Value written to the binary
#'   `wash_drinking_water_time` output column when time is `"premises"` or
#'   under 30 minutes - matches
#'   `add_drinking_water_jmp_ladder()`'s `drinking_water_time_under_30min_val`
#'   default. Defaults to `"1"`.
#' @param drinking_water_time_over_30min_val Value written to the binary
#'   `wash_drinking_water_time` output column when time is 30 minutes or
#'   more - matches
#'   `add_drinking_water_jmp_ladder()`'s `drinking_water_time_over_30min_val`
#'   default. Defaults to `"0"`.
#'
#' @details
#' Values are resolved in priority order:
#' 1. `drinking_water_source` in `skipped_drinking_water_source_premises` -> `"premises"`.
#' 2. `drinking_water_source` in `skipped_drinking_water_source_undefined` -> `"undefined"`.
#' 3. `drinking_water_time_yn` in `water_on_premises` -> `"premises"`.
#' 4. `drinking_water_time_yn` equal to `number_minutes` -> bucketed from
#'    `drinking_water_time_int` (< 30 min, 30-60 min, 1hr-`max`); above `max`
#'    yields `NA`.
#' 5. `drinking_water_time_yn` in `undefined` -> `"undefined"`.
#' 6. `drinking_water_time_yn` equal to `dnk` and `drinking_water_time_sl` in
#'    `sl_undefined` -> `"undefined"`.
#' 7. `drinking_water_time_yn` equal to `dnk` and `drinking_water_time_sl` in
#'    `sl_under_30_min`/`sl_30min_1hr`/`sl_more_than_1hr` -> that bucket's
#'    canonical label (its first element).
#' 8. Anything else -> `NA`.
#'
#' The resulting `wash_drinking_water_time_cat` is then collapsed into a
#' second, binary `wash_drinking_water_time` column (`"premises"`/under-30-min
#' label -> `drinking_water_time_under_30min_val`; the 30min-1hr/more-than-1hr
#' labels -> `drinking_water_time_over_30min_val`; `"undefined"`/`NA` -> `NA`)
#' so `add_drinking_water_jmp_ladder()` can consume it directly via its own
#' `drinking_water_time_col` default (`"wash_drinking_water_time"`).
#'
#' @return `.dataset` with two added columns: `wash_drinking_water_time_cat`
#'   (the detailed category) and `wash_drinking_water_time` (the binary
#'   under/over-30-minutes value used by `add_drinking_water_jmp_ladder()`).
#'
#' @examples
#' example_data <- data.frame(
#'   wash_drinking_water_source = c("borehole", "piped_dwelling", "borehole", "borehole"),
#'   wash_drinking_water_time_yn = c("number_minutes", "number_minutes", "water_in_dwelling", "dnk"),
#'   wash_drinking_water_time_int = c(15, 45, NA, NA),
#'   wash_drinking_water_time_sl = c(NA, NA, NA, "30min_1hr")
#' )
#' add_drinking_water_time_cat(example_data)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @export
add_drinking_water_time_cat <- function(
    .dataset,
    drinking_water_time_yn = "wash_drinking_water_time_yn",
    water_on_premises = c("water_in_dwelling", "water_in_plot"),
    number_minutes = "number_minutes",
    dnk = "dnk",
    undefined = "pnta",
    drinking_water_time_int = "wash_drinking_water_time_int",
    max = 600,
    drinking_water_time_sl = "wash_drinking_water_time_sl",
    sl_under_30_min = c("5min_or_less", "5min_15min", "15min_30min"),
    sl_30min_1hr = "30min_1hr",
    sl_more_than_1hr = "more_than_1hr",
    sl_undefined = c("dnk", "pnta"),
    drinking_water_source = "wash_drinking_water_source",
    skipped_drinking_water_source_premises = "piped_dwelling",
    skipped_drinking_water_source_undefined = c("dnk", "pnta"),
    drinking_water_time_under_30min_val = "1",
    drinking_water_time_over_30min_val = "0"
) {
  origin <- "add_drinking_water_time_cat"

  phrutils::phr_try(
    expr = {

      # Use ensure_value for all value-defining (non-column-name) parameters

      water_on_premises <- phrutils::ensure_value(water_on_premises, c("water_in_dwelling", "water_in_plot"))
      number_minutes <- phrutils::ensure_value(number_minutes, "number_minutes")
      dnk <- phrutils::ensure_value(dnk, "dnk")
      undefined <- phrutils::ensure_value(undefined, "pnta")
      max <- phrutils::ensure_value(max, 600)
      sl_under_30_min <- phrutils::ensure_value(sl_under_30_min, c("5min_or_less", "5min_15min", "15min_30min"))
      sl_30min_1hr <- phrutils::ensure_value(sl_30min_1hr, "30min_1hr")
      sl_more_than_1hr <- phrutils::ensure_value(sl_more_than_1hr, "more_than_1hr")
      sl_undefined <- phrutils::ensure_value(sl_undefined, c("dnk", "pnta"))
      skipped_drinking_water_source_premises <- phrutils::ensure_value(skipped_drinking_water_source_premises, "piped_dwelling")
      skipped_drinking_water_source_undefined <- phrutils::ensure_value(skipped_drinking_water_source_undefined, c("dnk", "pnta"))
      drinking_water_time_under_30min_val <- phrutils::ensure_value(drinking_water_time_under_30min_val, "1")
      drinking_water_time_over_30min_val <- phrutils::ensure_value(drinking_water_time_over_30min_val, "0")

      # sl_under_30_min/sl_30min_1hr/sl_more_than_1hr each accept a VECTOR of
      # raw drinking_water_time_sl codes belonging to that bucket (some tools
      # use many distinct range labels, e.g. "5min_or_less", "5min_15min",
      # "15min_30min" all meaning under 30 minutes). Each bucket's first
      # element is used as the single canonical label written to the output
      # column, both for this skip-logic passthrough and for the numeric
      # drinking_water_time_int bucketing below.
      sl_under_30_min_label <- sl_under_30_min[[1]]
      sl_30min_1hr_label <- sl_30min_1hr[[1]]
      sl_more_than_1hr_label <- sl_more_than_1hr[[1]]


      # Basic dataset checks

      phrutils::phr_validate_dataframe(
        .dataset,
        origin = origin,
        hint = ("Ensure you pass a valid data frame or tibble to `.dataset`.")
      )

      phrutils::phr_assert(
        nrow(.dataset) > 0,
        origin = origin,
        ("Dataset is empty.")
      )


      # Required columns

      phrutils::phr_validate_columns(
        .dataset,
        c(drinking_water_time_yn, drinking_water_time_int, drinking_water_time_sl, drinking_water_source),
        origin = origin,
        hint = ("Ensure drinking_water_time_yn, drinking_water_time_int, drinking_water_time_sl and drinking_water_source all exist in `.dataset`."),
        soft = FALSE
      )


      # drinking_water_time_int must be numeric and strictly positive;
      # coerce to integer if it safely can be, error otherwise.

      if (!is.numeric(.dataset[[drinking_water_time_int]])) {
        original_class <- class(.dataset[[drinking_water_time_int]])[1]

        phrutils::phr_assert(
          phrutils::is_safely_coercible(.dataset[[drinking_water_time_int]], "numeric"),
          origin = origin,
          (glue::glue("Column {drinking_water_time_int} cannot be safely coerced to numeric."))
        )

        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Column {drinking_water_time_int} was {original_class} and has been coerced to integer."))
        )

        .dataset[[drinking_water_time_int]] <- as.integer(as.numeric(.dataset[[drinking_water_time_int]]))
      }

      phrutils::phr_assert(
        !any(.dataset[[drinking_water_time_int]] <= 0, na.rm = TRUE),
        origin = origin,
        (glue::glue("The values in {drinking_water_time_int} must be strictly above 0."))
      )


      # sl_under_30_min, sl_30min_1hr, sl_more_than_1hr must each have at least one code

      phrutils::phr_assert(
        length(sl_under_30_min) >= 1 && length(sl_30min_1hr) >= 1 && length(sl_more_than_1hr) >= 1,
        origin = origin,
        ("sl_under_30_min, sl_30min_1hr and sl_more_than_1hr must each contain at least one value.")
      )


      # Validate categorical inputs. `phr_validate_choice()` only warns (even with
      # soft = FALSE) and its phr_validate_not_null() step aborts on a length-1
      # all-NA column, so a valid single-row dataset would wrongly error. Assert
      # directly instead: NA is always allowed, any other unlisted value aborts.

      time_yn_choices <- c(water_on_premises, number_minutes, dnk, undefined)
      time_yn_bad <- setdiff(stats::na.omit(.dataset[[drinking_water_time_yn]]), time_yn_choices)
      phrutils::phr_assert(
        length(time_yn_bad) == 0,
        origin = origin,
        (glue::glue("Column {drinking_water_time_yn} contains values outside the allowed set: {paste(time_yn_bad, collapse = ', ')}."))
      )

      time_sl_choices <- c(sl_under_30_min, sl_30min_1hr, sl_more_than_1hr, sl_undefined)
      time_sl_bad <- setdiff(stats::na.omit(.dataset[[drinking_water_time_sl]]), time_sl_choices)
      phrutils::phr_assert(
        length(time_sl_bad) == 0,
        origin = origin,
        (glue::glue("Column {drinking_water_time_sl} contains values outside the allowed set: {paste(time_sl_bad, collapse = ', ')}."))
      )


      # Overwrite warnings for output columns

      output_col <- "wash_drinking_water_time_cat"
      binary_output_col <- "wash_drinking_water_time"

      for (col in c(output_col, binary_output_col)) {
        if (col %in% names(.dataset)) {
          phrutils::phr_warning(
            origin = origin,
            message = (glue::glue("Column {col} already exists and will be overwritten."))
          )
        }
      }


      # Recode

      .dataset <- dplyr::mutate(
        .dataset,
        !!output_col := dplyr::case_when(
          .data[[drinking_water_source]] %in% skipped_drinking_water_source_premises ~ "premises",
          .data[[drinking_water_source]] %in% skipped_drinking_water_source_undefined ~ "undefined",
          .data[[drinking_water_time_yn]] %in% water_on_premises ~ "premises",
          .data[[drinking_water_time_yn]] %in% number_minutes ~ dplyr::case_when(
            .data[[drinking_water_time_int]] < 30 ~ sl_under_30_min_label,
            .data[[drinking_water_time_int]] >= 30 & .data[[drinking_water_time_int]] < 60 ~ sl_30min_1hr_label,
            .data[[drinking_water_time_int]] <= max ~ sl_more_than_1hr_label
          ),
          # Fix don't know
          .data[[drinking_water_time_yn]] %in% undefined ~ "undefined",
          .data[[drinking_water_time_yn]] %in% dnk & .data[[drinking_water_time_sl]] %in% sl_undefined ~ "undefined",
          .data[[drinking_water_time_yn]] %in% dnk & .data[[drinking_water_time_sl]] %in% sl_under_30_min ~ sl_under_30_min_label,
          .data[[drinking_water_time_yn]] %in% dnk & .data[[drinking_water_time_sl]] %in% sl_30min_1hr ~ sl_30min_1hr_label,
          .data[[drinking_water_time_yn]] %in% dnk & .data[[drinking_water_time_sl]] %in% sl_more_than_1hr ~ sl_more_than_1hr_label,
          .default = NA_character_
        )
      )

      # Collapse the rich category into the binary under/over-30-minutes value
      # expected by add_drinking_water_jmp_ladder() (drinking_water_time_col,
      # matched against its drinking_water_time_under_30min_val/
      # drinking_water_time_over_30min_val defaults of "1"/"0"). output_col is
      # always one of the canonical labels above (never a raw passthrough), so
      # matching against the single labels here is sufficient.
      .dataset <- dplyr::mutate(
        .dataset,
        !!binary_output_col := dplyr::case_when(
          .data[[output_col]] %in% c("premises", sl_under_30_min_label) ~ drinking_water_time_under_30min_val,
          .data[[output_col]] %in% c(sl_30min_1hr_label, sl_more_than_1hr_label) ~ drinking_water_time_over_30min_val,
          .default = NA_character_
        )
      )

      phrutils::phr_message(
        origin = origin,
        message = (glue::glue("Drinking water time columns successfully added: {output_col}, {binary_output_col}."))
      )

      return(.dataset)
    },
    on_error = "abort",
    origin = origin
  )
}
