#' Normalize administrative names
#' @param x Character vector of administrative unit names to normalize.
hh_normalize_adm <- function(x) {
  phrutils::phr_try({
    x <- trimws(as.character(x))
    x <- ifelse(x == "", NA_character_, x)
    tools::toTitleCase(tolower(x))
  }, on_error = "warn", origin = "hh_normalize_adm")
}
