# Tests for hh_check_weight_integrity

test_that("hh_check_weight_integrity() returns empty lists when all weights valid", {
  w <- c(50, 51, 52, 53, 54)

  out <- hh_check_weight_integrity(w)

  expect_equal(length(out$negative), 0)
  expect_equal(length(out$zero), 0)
  expect_equal(length(out$missing), 0)
})

test_that("hh_check_weight_integrity() detects negative weights", {
  w <- c(50, -10, 52)

  out <- hh_check_weight_integrity(w)

  expect_equal(out$negative, 2)
})

test_that("hh_check_weight_integrity() detects zero weights", {
  w <- c(50, 0, 52)

  out <- hh_check_weight_integrity(w)

  expect_equal(out$zero, 2)
})

test_that("hh_check_weight_integrity() detects missing weights", {
  w <- c(50, NA, 52)

  out <- hh_check_weight_integrity(w)

  expect_equal(out$missing, 2)
})

test_that("hh_check_weight_integrity() detects multiple issues", {
  w <- c(50, -10, 0, NA, 52)

  out <- hh_check_weight_integrity(w)

  expect_equal(out$negative, 2)
  expect_equal(out$zero, 3)
  expect_equal(out$missing, 4)
})

test_that("hh_check_weight_integrity() handles character input", {
  w <- c("50", "-10", "0", "52")

  out <- hh_check_weight_integrity(w)

  expect_equal(out$negative, 2)
  expect_equal(out$zero, 3)
})

test_that("hh_check_weight_integrity() handles vector of length 1", {
  w <- 50

  out <- hh_check_weight_integrity(w)

  expect_equal(length(out$negative), 0)
  expect_equal(length(out$zero), 0)
  expect_equal(length(out$missing), 0)
})

test_that("hh_check_weight_integrity() returns indices", {
  w <- c(50, -10, 0, NA, 52)

  out <- hh_check_weight_integrity(w)

  expect_true(is.numeric(out$negative))
  expect_true(is.numeric(out$zero))
  expect_true(is.numeric(out$missing))
})

test_that("hh_check_weight_integrity() warns when issues detected", {
  w <- c(50, -10, 52)

  expect_warning(hh_check_weight_integrity(w))
})

test_that("hh_check_weight_integrity() handles decimal weights", {
  w <- c(50.5, 51.2, 0.0, -0.5)

  out <- hh_check_weight_integrity(w)

  expect_equal(out$negative, 4)
  expect_equal(out$zero, 3)
})

test_that("hh_check_weight_integrity() handles very small positive values", {
  w <- c(0.001, 0.0001, 50)

  out <- hh_check_weight_integrity(w)

  expect_equal(length(out$negative), 0)
  expect_equal(length(out$zero), 0)
})
