make_iycf_data <- function(n = NULL, age_months = 12, ...) {
  overrides <- list(...)
  override_lengths <- vapply(overrides, length, integer(1))
  lengths <- c(length(age_months), override_lengths)
  lengths <- lengths[lengths > 0]
  target_n <- if (is.null(n)) max(lengths, 1L) else n

  recycle_input <- function(x, name) {
    if (length(x) %in% c(1L, target_n)) {
      return(rep(x, length.out = target_n))
    }

    stop(sprintf("%s must have length 1 or %d", name, target_n), call. = FALSE)
  }

  df <- tibble::tibble(
    age_months = recycle_input(age_months, "age_months"),
    iycf_1 = rep("no", target_n),
    iycf_2 = rep(3, target_n),
    iycf_3 = rep(1, target_n),
    iycf_4 = rep("no", target_n),
    iycf_5 = rep("no", target_n),
    iycf_6a = rep("no", target_n),
    iycf_6b = rep("no", target_n),
    iycf_6c = rep("no", target_n),
    iycf_6d = rep("no", target_n),
    iycf_6e = rep("no", target_n),
    iycf_6f = rep("no", target_n),
    iycf_6g = rep("no", target_n),
    iycf_6h = rep("no", target_n),
    iycf_6i = rep("no", target_n),
    iycf_6j = rep("no", target_n),
    iycf_6b_num = rep(0, target_n),
    iycf_6c_num = rep(0, target_n),
    iycf_6d_num = rep(0, target_n),
    iycf_6c_swt = rep("no", target_n),
    iycf_6d_swt = rep("no", target_n),
    iycf_6h_swt = rep("no", target_n),
    iycf_6j_swt = rep("no", target_n),
    iycf_7a = rep("no", target_n),
    iycf_7b = rep("no", target_n),
    iycf_7c = rep("no", target_n),
    iycf_7d = rep("no", target_n),
    iycf_7e = rep("no", target_n),
    iycf_7f = rep("no", target_n),
    iycf_7g = rep("no", target_n),
    iycf_7h = rep("no", target_n),
    iycf_7i = rep("no", target_n),
    iycf_7j = rep("no", target_n),
    iycf_7k = rep("no", target_n),
    iycf_7l = rep("no", target_n),
    iycf_7m = rep("no", target_n),
    iycf_7n = rep("no", target_n),
    iycf_7o = rep("no", target_n),
    iycf_7p = rep("no", target_n),
    iycf_7q = rep("no", target_n),
    iycf_7r = rep("no", target_n),
    iycf_7a_num = rep(0, target_n),
    iycf_8 = rep(0, target_n),
    iycf_mdd_cat = rep(0, target_n),
    iycf_mmf = rep(0, target_n),
    iycf_mmff = rep(0, target_n)
  )

  for (nm in names(overrides)) {
    df[[nm]] <- recycle_input(overrides[[nm]], nm)
  }

  df
}
