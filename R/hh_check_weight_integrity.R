#' Detect invalid weights: negative, zero, missing
#' @param w Numeric vector of weight values to check for integrity issues.
hh_check_weight_integrity <- function(w) {
  phrutils::phr_try({

    w <- suppressWarnings(as.numeric(w))
    issues <- list(
      negative = which(w < 0),
      zero     = which(w == 0),
      missing  = which(is.na(w))
    )

    if (length(unlist(issues)) > 0) {
      phrutils::phr_warning("HouseholdData","Weight integrity issues: {length(issues$negative)} negative, {length(issues$zero)} zero, {length(issues$missing)} missing.")
    }

    issues

  }, on_error = "warn", origin = "hh_check_weight_integrity")
}
