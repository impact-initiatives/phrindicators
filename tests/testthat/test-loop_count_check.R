# Tests for loop_count_check

test_that("loop_count_check() returns ok=TRUE when all counts match", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B", "C"),
    count = c(2, 1, 3)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("A", "A", "B", "C", "C", "C")
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))

  expect_true(out$ok)
  expect_equal(length(out$mismatches), 0)
})

test_that("loop_count_check() detects count mismatches", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B"),
    count = c(2, 1)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("A", "A", "A")  # 3 records instead of 2
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))

  expect_false(out$ok)
  expect_true("A" %in% out$mismatches)
})

test_that("loop_count_check() detects orphan loop records", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B"),
    count = c(1, 1)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("A", "C")  # C is not in parent
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))

  expect_false(out$ok)
  expect_true("C" %in% out$mismatches)
})

test_that("loop_count_check() handles zero counts", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B", "C"),
    count = c(1, 0, 2)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("A", "C", "C")
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))

  expect_true(out$ok)
})

test_that("loop_count_check() handles NA expected counts with soft_missing_parent=TRUE", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B"),
    count = c(NA, 1)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("B")
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count", soft_missing_parent = TRUE))

  expect_true(out$ok)
})

test_that("loop_count_check() flags NA expected counts with soft_missing_parent=FALSE", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B"),
    count = c(NA, 1)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("B")
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count", soft_missing_parent = FALSE))

  expect_false(out$ok)
  expect_true("A" %in% out$mismatches)
})

test_that("loop_count_check() errors when parent UUID column missing", {
  parent_df <- tibble::tibble(uuid_parent = c("A", "B"))
  child_df <- tibble::tibble(parent_uuid = c("A"))

  expect_error(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))
})

test_that("loop_count_check() errors when child UUID column missing", {
  parent_df <- tibble::tibble(uuid = c("A", "B"), count = c(1, 1))
  child_df <- tibble::tibble(child_uuid = c("A"))

  expect_error(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))
})

test_that("loop_count_check() errors when parent count column missing", {
  parent_df <- tibble::tibble(uuid = c("A", "B"))
  child_df <- tibble::tibble(parent_uuid = c("A", "B"))

  expect_error(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))
})

test_that("loop_count_check() returns detailed comparison", {
  parent_df <- tibble::tibble(
    uuid = c("A", "B"),
    count = c(2, 1)
  )

  child_df <- tibble::tibble(
    parent_uuid = c("A", "A", "B")
  )

  out <- suppressWarnings(loop_count_check(parent_df, child_df, "uuid", "parent_uuid", "count"))

  expect_true("uuid" %in% names(out$details))
  expect_true("expected" %in% names(out$details))
  expect_true("actual" %in% names(out$details))
})
