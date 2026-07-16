#' Check relationship-to-head plausibility
#' @param rel_vec Character vector of relationship-to-head codes to validate.
hh_check_roster_relationships <- function(rel_vec) {
  phrutils::phr_try({

    allowed <- c("head", "spouse", "child", "parent", "relative", "other")
    bad <- setdiff(unique(rel_vec), allowed)

    if (length(bad) > 0) {
      phrutils::phr_warning(
        "HouseholdData",
        glue::glue("Invalid relationship codes detected: {paste(bad, collapse=', ')}")
      )
    }

    bad[!is.na(bad)]

  }, on_error = "warn", origin = "hh_check_roster_relationships")
}
