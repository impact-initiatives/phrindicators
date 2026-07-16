#' Validate admin codes/names against reference values
#' @param x Character vector of administrative unit values to validate.
#' @param valid Character vector of valid/accepted administrative unit values.
hh_validate_adm <- function(x, valid) {
  phrutils::phr_try({

    bad <- setdiff(unique(x), valid)

    if (length(bad) > 0) {
      phrutils::phr_warning(
        "HouseholdData",
        glue::glue("Invalid administrative units detected: {paste(bad, collapse=', ')}")
      )
    }

    bad[!is.na(bad)]

  }, on_error = "warn", origin = "hh_validate_adm")
}
