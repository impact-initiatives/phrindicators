# Tests for hh_check_roster_relationships

test_that("hh_check_roster_relationships() returns empty vector for valid relationships", {
  x <- c("head", "spouse", "child", "parent")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_equal(length(out), 0)
})

test_that("hh_check_roster_relationships() detects invalid relationship codes", {
  x <- c("head", "spouse", "invalid", "child")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_true("invalid" %in% out)
})

test_that("hh_check_roster_relationships() returns unique invalid values", {
  x <- c("invalid1", "invalid1", "invalid2", "invalid2")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_equal(length(out), 2)
  expect_true("invalid1" %in% out)
  expect_true("invalid2" %in% out)
})

test_that("hh_check_roster_relationships() allows all valid codes", {
  x <- c("head", "spouse", "child", "parent", "relative", "other")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_equal(length(out), 0)
})

test_that("hh_check_roster_relationships() handles NA values", {
  x <- c("head", NA, "spouse", "invalid")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_true("invalid" %in% out)
  expect_false(NA %in% out)
})

test_that("hh_check_roster_relationships() is case-sensitive", {
  x <- c("head", "Head", "HEAD", "spouse")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_true("Head" %in% out)
  expect_true("HEAD" %in% out)
})

test_that("hh_check_roster_relationships() handles whitespace", {
  x <- c("head", " head ", "spouse")

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_true(" head " %in% out)
})

test_that("hh_check_roster_relationships() warns when invalid codes detected", {
  x <- c("head", "invalid")

  expect_warning(hh_check_roster_relationships(x))
})

test_that("hh_check_roster_relationships() handles all NA values", {
  x <- c(NA, NA, NA)

  out <- suppressWarnings(hh_check_roster_relationships(x))

  expect_equal(length(out), 0)
})
