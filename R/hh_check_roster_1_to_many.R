#' Check 1:many linkage integrity
#' @param hh_df A data frame containing household-level data.
#' @param roster_df A data frame containing roster (individual-level) data.
#' @param uuid_hh Character string specifying the UUID column name in the household data frame.
#' @param uuid_roster Character string specifying the UUID column name in the roster data frame that links to households.
hh_check_roster_1_to_many <- function(hh_df, roster_df, uuid_hh, uuid_roster) {
  phrutils::phr_try({

    res <- list(
      missing_households = hh_roster_link_check(hh_df, roster_df, uuid_hh, uuid_roster),
      households_with_members = unique(roster_df[[uuid_roster]])
    )

    res

  }, on_error = "warn", origin = "hh_check_roster_1_to_many")
}
