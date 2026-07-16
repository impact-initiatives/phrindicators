#' Recode IYCF yes/no/dnk values to numeric
#'
#' @description
#' Internal helper that recodes categorical IYCF responses to numeric values.
#' If the input is already numeric, it is returned as-is. Otherwise, values
#' matching yes_val are recoded to 1, no_val to 2, dnk_val to NA, and
#' unrecognized values to NA.
#'
#' @param x Vector to recode
#' @param yes_val Character: Value representing "yes"
#' @param no_val Character: Value representing "no"
#' @param dnk_val Character: Value representing "don't know" (recoded to NA)
#'
#' @return Numeric vector with yes=1, no=2, dnk/other=NA
#' @keywords internal
iycf_recode_yesno <- function(x, yes_val = "yes", no_val = "no", dnk_val = "dnk") {
  if (is.numeric(x)) return(x)
  dplyr::case_when(
    x == yes_val ~ 1,
    x == no_val ~ 2,
    x == dnk_val ~ NA_real_,
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}
