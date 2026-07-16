#' Snap GPS coordinates to fixed decimal precision
#' @param x Numeric vector of GPS coordinate values to round.
#' @param digits Integer specifying the number of decimal places to round to. Default: 6.
hh_snap_coord <- function(x, digits = 6) {
  phrutils::phr_try({
    if (is.null(x)) return(NULL)
    as.numeric(round(as.numeric(x), digits = digits))
  }, on_error = "warn", origin = "hh_snap_coord")
}
