# Tests for hh_check_roster_1_to_many

test_that("hh_check_roster_1_to_many() returns households with members", {
  hh_df <- tibble::tibble(
    uuid_hh = c("HH1", "HH2", "HH3")
  )

  roster_df <- tibble::tibble(
    uuid_roster = c("HH1", "HH1", "HH2")
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "uuid_hh", "uuid_roster")

  expect_true("households_with_members" %in% names(out))
  expect_true("HH1" %in% out$households_with_members)
  expect_true("HH2" %in% out$households_with_members)
  expect_false("HH3" %in% out$households_with_members)
})

test_that("hh_check_roster_1_to_many() identifies missing households", {
  hh_df <- tibble::tibble(
    uuid_hh = c("HH1", "HH2")
  )

  roster_df <- tibble::tibble(
    uuid_roster = c("HH1", "HH3")  # HH3 not in parent
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "uuid_hh", "uuid_roster")

  expect_true("missing_households" %in% names(out))
})

test_that("hh_check_roster_1_to_many() handles empty roster", {
  hh_df <- tibble::tibble(
    uuid_hh = c("HH1", "HH2")
  )

  roster_df <- tibble::tibble(
    uuid_roster = character(0)
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "uuid_hh", "uuid_roster")

  expect_equal(length(out$households_with_members), 0)
})

test_that("hh_check_roster_1_to_many() handles multiple members per household", {
  hh_df <- tibble::tibble(
    uuid_hh = c("HH1", "HH2")
  )

  roster_df <- tibble::tibble(
    uuid_roster = c("HH1", "HH1", "HH1", "HH2")
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "uuid_hh", "uuid_roster")

  expect_true("HH1" %in% out$households_with_members)
  expect_true("HH2" %in% out$households_with_members)
})

test_that("hh_check_roster_1_to_many() returns unique households", {
  hh_df <- tibble::tibble(
    uuid_hh = c("HH1", "HH2")
  )

  roster_df <- tibble::tibble(
    uuid_roster = c("HH1", "HH1", "HH1")
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "uuid_hh", "uuid_roster")

  expect_equal(length(out$households_with_members), 1)
  expect_equal(out$households_with_members, "HH1")
})

test_that("hh_check_roster_1_to_many() handles custom column names", {
  hh_df <- tibble::tibble(
    hh_id = c("HH1", "HH2")
  )

  roster_df <- tibble::tibble(
    hh_link = c("HH1", "HH2")
  )

  out <- hh_check_roster_1_to_many(hh_df, roster_df, "hh_id", "hh_link")

  expect_true("HH1" %in% out$households_with_members)
  expect_true("HH2" %in% out$households_with_members)
})
