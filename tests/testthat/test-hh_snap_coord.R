# Tests for hh_snap_coord

test_that("hh_snap_coord() rounds to specified decimal places", {
  x <- c(1.123456, 2.654321, 3.999999)

  out <- hh_snap_coord(x, digits = 4)

  expect_equal(out[1], 1.1235)
  expect_equal(out[2], 2.6543)
  expect_equal(out[3], 4.0000)
})

test_that("hh_snap_coord() defaults to 6 decimal places", {
  x <- 1.1234567

  out <- hh_snap_coord(x)

  expect_equal(out, 1.123457)
})

test_that("hh_snap_coord() handles zero decimal places", {
  x <- c(1.5, 2.4, 3.6)

  out <- hh_snap_coord(x, digits = 0)

  expect_equal(out, c(2, 2, 4))
})

test_that("hh_snap_coord() handles NA values", {
  x <- c(1.123456, NA, 3.654321)

  out <- hh_snap_coord(x, digits = 3)

  expect_equal(out[1], 1.123)
  expect_true(is.na(out[2]))
  expect_equal(out[3], 3.654)
})

test_that("hh_snap_coord() handles NULL input", {
  x <- NULL

  out <- hh_snap_coord(x)

  expect_null(out)
})

test_that("hh_snap_coord() converts character to numeric", {
  x <- c("1.123456", "2.654321")

  out <- hh_snap_coord(x, digits = 2)

  expect_equal(out, c(1.12, 2.65))
})

test_that("hh_snap_coord() handles vector of length 1", {
  x <- 45.123456

  out <- hh_snap_coord(x, digits = 3)

  expect_equal(out, 45.123)
})

test_that("hh_snap_coord() handles negative values", {
  x <- c(-1.123456, -2.654321)

  out <- hh_snap_coord(x, digits = 2)

  expect_equal(out, c(-1.12, -2.65))
})
