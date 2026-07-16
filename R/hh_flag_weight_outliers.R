#' Identify outliers using IQR rule
#' @param w Numeric vector of weight values to check for outliers.
hh_flag_weight_outliers <- function(w) {
  phrutils::phr_try({

    w <- suppressWarnings(as.numeric(w))
    if (length(w) == 0) return(integer(0))

    q <- stats::quantile(w, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]

    if (iqr == 0) return(integer(0))

    lower <- q[1] - 3 * iqr
    upper <- q[2] + 3 * iqr

    outliers <- which(w < lower | w > upper)

    if (length(outliers) > 0) {
      phrutils::phr_warning(
        "HouseholdData",
        glue::glue("Weight outliers detected at rows: {paste(outliers, collapse=', ')}")
      )
    }

    outliers

  }, on_error = "warn", origin = "hh_flag_weight_outliers")
}
