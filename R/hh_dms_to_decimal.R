#' Convert DMS to decimal degrees
#' @param x Character or numeric vector of coordinates in degrees-minutes-seconds (DMS) format.
hh_dms_to_decimal <- function(x) {
  phrutils::phr_try({

    parse_one <- function(val) {
      val <- trimws(val)
      val <- gsub("[\u00b0'\"\\\"]", " ", val)

      nums <- suppressWarnings(as.numeric(strsplit(val, "\\s+")[[1]]))
      if (length(nums) == 3) nums[1] + nums[2] / 60 + nums[3] / 3600
      else suppressWarnings(as.numeric(val))
    }

    vapply(x, parse_one, numeric(1))

  }, on_error = "warn", origin = "hh_dms_to_decimal")
}
