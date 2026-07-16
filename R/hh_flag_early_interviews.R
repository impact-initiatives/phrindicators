#' Flag interview dates earlier than project start
#' @param dates Character, Date, or numeric vector of interview dates to check.
#' @param project_start A Date or character value specifying the project start date.
hh_flag_early_interviews <- function(dates, project_start) {
  phrutils::phr_try({

    d <- suppressWarnings(phrutils::phr_convert_date(dates))
    bad <- which(!is.na(d) & d < project_start)

    if (length(bad) > 0) {
      phrutils::phr_warning(
        "HouseholdData",
        glue::glue("Interview dates earlier than project start detected at rows: {paste(bad, collapse=', ')}")
      )
    }

    bad

  }, on_error = "warn", origin = "hh_flag_early_interviews")
}
