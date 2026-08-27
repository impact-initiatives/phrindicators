#' Add Handwashing Facility JMP Category
#'
#' @description
#' Categorizes each household's handwashing facility into the JMP (Joint
#' Monitoring Programme) service-ladder classes `"basic"`, `"limited"`,
#' `"no_facility"` or `"undefined"` (`NA` when nothing can be decided).
#'
#' Two mutually exclusive paths are used, chosen per row:
#' * **Observed** - used when the interview was *in person* **and** the
#'   enumerator had permission to observe (`facility` not equal to
#'   `facility_no_permission`). Uses the observed facility, water, soap and
#'   soap-type columns.
#' * **Reported** - used when the interview was *remote*, or in person but
#'   without permission to observe. Uses the reported facility, water, soap and
#'   soap-type columns.
#'
#' @param .dataset Input data frame or tibble.
#' @param survey_modality Column name (as a string) for the survey modality.
#'   Defaults to `"survey_modality"`.
#' @param survey_modality_in_person Value(s) of `survey_modality` meaning the
#'   interview was in person. Defaults to `"in_person"`.
#' @param survey_modality_remote Value(s) of `survey_modality` meaning the
#'   interview was remote. Defaults to `"remote"`.
#' @param facility Column name (as a string) for the observed handwashing
#'   facility. Defaults to `"wash_handwashing_facility"`.
#' @param facility_yes Value(s) of `facility` indicating a facility is present.
#'   Defaults to `c("available_fixed_in_dwelling", "available_fixed_in_plot", "available_mobile")`.
#' @param facility_no Single value of `facility` indicating no facility. Defaults
#'   to `"none"`.
#' @param facility_no_permission Single value of `facility` indicating no
#'   permission to observe. Defaults to `"no_permission"`.
#' @param facility_undefined Value(s) of `facility` indicating an undefined
#'   response. Defaults to `"other"`.
#' @param facility_observed_water Column name (as a string) for observed water
#'   availability. Defaults to `"wash_handwashing_facility_observed_water"`.
#' @param facility_observed_water_yes Value indicating observed water is
#'   available. Defaults to `"water_available"`.
#' @param facility_observed_water_no Value(s) indicating observed water is not
#'   available. Defaults to `"water_not_available"`.
#' @param facility_observed_soap Column name (as a string) for observed soap
#'   availability. Defaults to `"wash_soap_observed_yn"`.
#' @param facility_observed_soap_yes Value indicating observed soap is available.
#'   Defaults to `"soap_available"`.
#' @param facility_observed_soap_no Value(s) indicating observed soap is not
#'   available. Defaults to `"soap_not_available"`.
#' @param facility_observed_soap_undefined Value(s) for undefined observed soap.
#'   Defaults to `c("other", "pnta")`.
#' @param facility_reported Column name (as a string) for the reported
#'   handwashing facility. Defaults to `"wash_handwashing_facility_reported"`.
#' @param facility_reported_yes Value(s) indicating a reported facility is
#'   present. Defaults to `c("fixed_dwelling", "fixed_yard", "mobile")`.
#' @param facility_reported_no Value(s) indicating no reported facility. Defaults
#'   to `"none"`.
#' @param facility_reported_undefined Value(s) for an undefined reported facility.
#'   Defaults to `c("other", "dnk", "pnta")`.
#' @param facility_reported_water Column name (as a string) for reported water
#'   availability. Defaults to `"wash_handwashing_facility_water_reported_yn"`.
#' @param facility_reported_water_yes Value(s) indicating reported water is
#'   available. Defaults to `"yes"`.
#' @param facility_reported_water_no Value(s) indicating reported water is not
#'   available. Defaults to `"no"`.
#' @param facility_reported_water_undefined Value(s) for undefined reported water.
#'   Defaults to `c("dnk", "pnta")`.
#' @param facility_reported_soap Column name (as a string) for reported soap
#'   availability. Defaults to `"wash_soap_reported_yn"`.
#' @param facility_reported_soap_yes Value(s) indicating reported soap is
#'   available. Defaults to `"yes"`.
#' @param facility_reported_soap_no Value(s) indicating reported soap is not
#'   available. Defaults to `"no"`.
#' @param facility_reported_soap_undefined Value(s) for undefined reported soap.
#'   Defaults to `c("dnk", "pnta")`.
#' @param soap_type_observed Column name (as a string) for the observed soap
#'   type. Defaults to `"wash_soap_observed_type"`.
#' @param soap_type_observed_yes Value(s) of `soap_type_observed` that count as
#'   soap. Defaults to `c("soap", "detergent")`.
#' @param soap_type_observed_no Value(s) of `soap_type_observed` that do not
#'   count as soap (e.g. ash/mud/sand). Defaults to `"ash_mud_sand"`.
#' @param soap_type_observed_undefined Value(s) for an undefined observed soap
#'   type. Defaults to `c("dnk", "pnta", "other")`.
#' @param soap_type_reported Column name (as a string) for the reported soap
#'   type. Defaults to `"wash_soap_reported_type"`.
#' @param soap_type_reported_yes Value(s) of `soap_type_reported` that count as
#'   soap. Defaults to `c("soap", "detergent")`.
#' @param soap_type_reported_no Value(s) of `soap_type_reported` that do not
#'   count as soap. Defaults to `"ash_mud_sand"`.
#' @param soap_type_reported_undefined Value(s) for an undefined reported soap
#'   type. Defaults to `c("dnk", "pnta", "other")`.
#'
#' @details
#' Within the applicable path, classes are resolved in priority order:
#'
#' Observed path:
#' 1. facility is `facility_no` -> `"no_facility"`.
#' 2. facility present, water and soap both observed available, but soap type is
#'    `soap_type_observed_no` or `soap_type_observed_undefined` -> `"limited"`.
#' 3. facility present, water and soap both observed available -> `"basic"`.
#' 4. facility present (water and/or soap missing) -> `"limited"`.
#'
#' Reported path:
#' 1. reported facility is `facility_reported_undefined` -> `"undefined"`.
#' 2. reported facility present, water and soap both reported available, but soap
#'    type is `soap_type_reported_no`/`soap_type_reported_undefined` -> `"limited"`.
#' 3. reported facility present, water and soap both reported available -> `"basic"`.
#' 4. reported facility present -> `"limited"`.
#' 5. reported facility is `facility_reported_no` -> `"no_facility"`.
#'
#' Anything not matched -> `NA`. Intermediate helper columns are dropped before
#' returning.
#'
#' @return `.dataset` with one added character column
#'   `wash_handwashing_facility_jmp_cat` taking values `"basic"`, `"limited"`,
#'   `"no_facility"`, `"undefined"` or `NA`.
#'
#' @examples
#' df <- data.frame(
#'   survey_modality = c("in_person", "in_person", "remote"),
#'   wash_handwashing_facility = c("available_fixed_in_dwelling", "none", "no_permission"),
#'   wash_handwashing_facility_observed_water = c("water_available", "water_not_available", NA),
#'   wash_soap_observed_yn = c("soap_available", "soap_not_available", NA),
#'   wash_soap_observed_type = c("soap", NA, NA),
#'   wash_handwashing_facility_reported = c(NA, NA, "fixed_dwelling"),
#'   wash_handwashing_facility_water_reported_yn = c(NA, NA, "yes"),
#'   wash_soap_reported_yn = c(NA, NA, "yes"),
#'   wash_soap_reported_type = c(NA, NA, "soap")
#' )
#' add_handwashing_facility_cat(df)
#'
#' @importFrom dplyr mutate case_when select all_of
#' @importFrom rlang .data
#' @export
add_handwashing_facility_cat <- function(
    .dataset,
    survey_modality = "survey_modality",
    survey_modality_in_person = c("in_person"),
    survey_modality_remote = c("remote"),
    facility = "wash_handwashing_facility",
    facility_yes = c(
      "available_fixed_in_dwelling",
      "available_fixed_in_plot",
      "available_mobile"
    ),
    facility_no = "none",
    facility_no_permission = "no_permission",
    facility_undefined = "other",
    facility_observed_water = "wash_handwashing_facility_observed_water",
    facility_observed_water_yes = "water_available",
    facility_observed_water_no = c("water_not_available"),
    facility_observed_soap = "wash_soap_observed_yn",
    facility_observed_soap_yes = "soap_available",
    facility_observed_soap_no = "soap_not_available",
    facility_observed_soap_undefined = c("other", "pnta"),
    facility_reported = "wash_handwashing_facility_reported",
    facility_reported_yes = c("fixed_dwelling", "fixed_yard", "mobile"),
    facility_reported_no = c("none"),
    facility_reported_undefined = c("other", "dnk", "pnta"),
    facility_reported_water = "wash_handwashing_facility_water_reported_yn",
    facility_reported_water_yes = "yes",
    facility_reported_water_no = c("no"),
    facility_reported_water_undefined = c("dnk", "pnta"),
    facility_reported_soap = "wash_soap_reported_yn",
    facility_reported_soap_yes = "yes",
    facility_reported_soap_no = c("no"),
    facility_reported_soap_undefined = c("dnk", "pnta"),
    soap_type_observed = "wash_soap_observed_type",
    soap_type_observed_yes = c("soap", "detergent"),
    soap_type_observed_no = c("ash_mud_sand"),
    soap_type_observed_undefined = c("dnk", "pnta", "other"),
    soap_type_reported = "wash_soap_reported_type",
    soap_type_reported_yes = c("soap", "detergent"),
    soap_type_reported_no = c("ash_mud_sand"),
    soap_type_reported_undefined = c("dnk", "pnta", "other")
) {
  origin <- "add_handwashing_facility_cat"

  phrutils::phr_try(
    expr = {

      # Value-defining (non-column-name) parameters

      survey_modality_in_person <- phrutils::ensure_value(survey_modality_in_person, "in_person")
      survey_modality_remote <- phrutils::ensure_value(survey_modality_remote, "remote")
      facility_yes <- phrutils::ensure_value(facility_yes, c("available_fixed_in_dwelling", "available_fixed_in_plot", "available_mobile"))
      facility_no <- phrutils::ensure_value(facility_no, "none")
      facility_no_permission <- phrutils::ensure_value(facility_no_permission, "no_permission")
      facility_undefined <- phrutils::ensure_value(facility_undefined, "other")
      facility_observed_water_yes <- phrutils::ensure_value(facility_observed_water_yes, "water_available")
      facility_observed_water_no <- phrutils::ensure_value(facility_observed_water_no, "water_not_available")
      facility_observed_soap_yes <- phrutils::ensure_value(facility_observed_soap_yes, "soap_available")
      facility_observed_soap_no <- phrutils::ensure_value(facility_observed_soap_no, "soap_not_available")
      facility_observed_soap_undefined <- phrutils::ensure_value(facility_observed_soap_undefined, c("other", "pnta"))
      facility_reported_yes <- phrutils::ensure_value(facility_reported_yes, c("fixed_dwelling", "fixed_yard", "mobile"))
      facility_reported_no <- phrutils::ensure_value(facility_reported_no, "none")
      facility_reported_undefined <- phrutils::ensure_value(facility_reported_undefined, c("other", "dnk", "pnta"))
      facility_reported_water_yes <- phrutils::ensure_value(facility_reported_water_yes, "yes")
      facility_reported_water_no <- phrutils::ensure_value(facility_reported_water_no, "no")
      facility_reported_water_undefined <- phrutils::ensure_value(facility_reported_water_undefined, c("dnk", "pnta"))
      facility_reported_soap_yes <- phrutils::ensure_value(facility_reported_soap_yes, "yes")
      facility_reported_soap_no <- phrutils::ensure_value(facility_reported_soap_no, "no")
      facility_reported_soap_undefined <- phrutils::ensure_value(facility_reported_soap_undefined, c("dnk", "pnta"))
      soap_type_observed_yes <- phrutils::ensure_value(soap_type_observed_yes, c("soap", "detergent"))
      soap_type_observed_no <- phrutils::ensure_value(soap_type_observed_no, "ash_mud_sand")
      soap_type_observed_undefined <- phrutils::ensure_value(soap_type_observed_undefined, c("dnk", "pnta", "other"))
      soap_type_reported_yes <- phrutils::ensure_value(soap_type_reported_yes, c("soap", "detergent"))
      soap_type_reported_no <- phrutils::ensure_value(soap_type_reported_no, "ash_mud_sand")
      soap_type_reported_undefined <- phrutils::ensure_value(soap_type_reported_undefined, c("dnk", "pnta", "other"))


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

      required_columns <- c(
        survey_modality,
        facility,
        facility_observed_water,
        facility_observed_soap,
        facility_reported,
        facility_reported_water,
        facility_reported_soap,
        soap_type_observed,
        soap_type_reported
      )

      phrutils::phr_validate_columns(
        .dataset,
        required_columns,
        origin = origin,
        hint = ("Ensure the survey modality, observed and reported handwashing columns all exist in `.dataset`."),
        soft = FALSE
      )


      # Length checks

      phrutils::phr_assert(
        length(facility_yes) >= 1,
        origin = origin,
        ("facility_yes must contain at least one valid response code.")
      )
      phrutils::phr_assert(
        length(facility_no) == 1,
        origin = origin,
        ("facility_no must be of length 1.")
      )
      phrutils::phr_assert(
        length(facility_no_permission) == 1,
        origin = origin,
        ("facility_no_permission must be of length 1.")
      )
      phrutils::phr_assert(
        length(facility_undefined) >= 1,
        origin = origin,
        ("facility_undefined must contain at least one valid response code.")
      )


      # Validate categorical inputs

      phrutils::phr_validate_choice(
        .dataset[[survey_modality]],
        choices = c(survey_modality_in_person, survey_modality_remote, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility]],
        choices = c(facility_yes, facility_no, facility_no_permission, facility_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility_observed_water]],
        choices = c(facility_observed_water_yes, facility_observed_water_no, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility_observed_soap]],
        choices = c(facility_observed_soap_yes, facility_observed_soap_no, facility_observed_soap_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility_reported]],
        choices = c(facility_reported_yes, facility_reported_no, facility_reported_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility_reported_water]],
        choices = c(facility_reported_water_yes, facility_reported_water_no, facility_reported_water_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[facility_reported_soap]],
        choices = c(facility_reported_soap_yes, facility_reported_soap_no, facility_reported_soap_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[soap_type_observed]],
        choices = c(soap_type_observed_yes, soap_type_observed_no, soap_type_observed_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )
      phrutils::phr_validate_choice(
        .dataset[[soap_type_reported]],
        choices = c(soap_type_reported_yes, soap_type_reported_no, soap_type_reported_undefined, NA_character_),
        origin = origin,
        soft = FALSE
      )


      # Overwrite warning for output column

      output_col <- "wash_handwashing_facility_jmp_cat"
      keep_cols <- union(names(.dataset), output_col)

      if (output_col %in% names(.dataset)) {
        phrutils::phr_warning(
          origin = origin,
          message = (glue::glue("Column {output_col} already exists and will be overwritten."))
        )
      }


      # Helper flags

      .dataset <- dplyr::mutate(
        .dataset,
        # Observed allowed only if in-person AND permission to observe
        .in_person_with_perm = (.data[[survey_modality]] %in% survey_modality_in_person) &
          !(.data[[facility]] %in% facility_no_permission),
        # Reported applies if remote OR (in-person BUT no permission)
        .reported_applicable = !(.data[[survey_modality]] %in% survey_modality_in_person) |
          ((.data[[survey_modality]] %in% survey_modality_in_person) &
             (.data[[facility]] %in% facility_no_permission)),

        # Observed helpers
        .obs_has_fac = .data[[facility]] %in% facility_yes,
        .obs_no_fac = .data[[facility]] %in% facility_no,
        .obs_basic = .data[[".obs_has_fac"]] &
          (.data[[facility_observed_water]] %in% facility_observed_water_yes) &
          (.data[[facility_observed_soap]] %in% facility_observed_soap_yes),
        .obs_both_absent = (.data[[facility_observed_water]] %in% facility_observed_water_no) &
          (.data[[facility_observed_soap]] %in% facility_observed_soap_no),

        # Reported helpers
        .rep_undefined = .data[[facility_reported]] %in% facility_reported_undefined,
        .rep_yes = .data[[facility_reported]] %in% facility_reported_yes,
        .rep_no = .data[[facility_reported]] %in% facility_reported_no,
        .rep_basic = .data[[".rep_yes"]] &
          (.data[[facility_reported_water]] %in% facility_reported_water_yes) &
          (.data[[facility_reported_soap]] %in% facility_reported_soap_yes),

        # Soap-type helpers
        .soap_type_observed_is_no = .data[[soap_type_observed]] %in% soap_type_observed_no,
        .soap_type_reported_is_no = .data[[soap_type_reported]] %in% soap_type_reported_no,
        .soap_type_observed_is_undefined = .data[[soap_type_observed]] %in% soap_type_observed_undefined,
        .soap_type_reported_is_undefined = .data[[soap_type_reported]] %in% soap_type_reported_undefined
      )


      # Classify

      .dataset <- dplyr::mutate(
        .dataset,
        !!output_col := dplyr::case_when(
          # OBSERVED path (in-person WITH permission)
          .data[[".in_person_with_perm"]] & .data[[".obs_no_fac"]] ~ "no_facility",
          .data[[".in_person_with_perm"]] & .data[[".obs_basic"]] & .data[[".soap_type_observed_is_no"]] ~ "limited",
          .data[[".in_person_with_perm"]] & .data[[".obs_basic"]] & .data[[".soap_type_observed_is_undefined"]] ~ "limited",
          .data[[".in_person_with_perm"]] & .data[[".obs_basic"]] ~ "basic",
          .data[[".in_person_with_perm"]] & .data[[".obs_has_fac"]] & !.data[[".obs_both_absent"]] ~ "limited",
          .data[[".in_person_with_perm"]] & .data[[".obs_has_fac"]] ~ "limited",

          # REPORTED path (remote OR in-person without permission)
          .data[[".reported_applicable"]] & .data[[".rep_undefined"]] ~ "undefined",
          .data[[".reported_applicable"]] & .data[[".rep_basic"]] & .data[[".soap_type_reported_is_no"]] ~ "limited",
          .data[[".reported_applicable"]] & .data[[".rep_basic"]] & .data[[".soap_type_reported_is_undefined"]] ~ "limited",
          .data[[".reported_applicable"]] & .data[[".rep_basic"]] ~ "basic",
          .data[[".reported_applicable"]] & .data[[".rep_yes"]] ~ "limited",
          .data[[".reported_applicable"]] & .data[[".rep_no"]] ~ "no_facility",

          .default = NA_character_
        )
      )


      # Drop intermediate helper columns

      .dataset <- dplyr::select(.dataset, dplyr::all_of(keep_cols))

      phrutils::phr_message(
        origin = origin,
        message = (glue::glue("Handwashing facility JMP category successfully added: {output_col}."))
      )

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = ("Ensure input columns and values exist and align with specifications.")
  )
}
