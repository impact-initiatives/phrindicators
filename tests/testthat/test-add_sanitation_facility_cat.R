# Tests for add_sanitation_facility_cat

test_that("add_sanitation_facility_cat() — valid dataset creates sanitation category", {

  df <- tibble::tibble(
    wash_sanitation_facility = c("flush_to_piped", "pit_lat", "open_defecation", "other")
  )

  out <- add_sanitation_facility_cat(
    .dataset = df,
    sanitation_facility_col = "wash_sanitation_facility",
    improved_facilities_val = c("flush_to_piped", "flush_to_septic"),
    unimproved_facilities_val = c("pit_lat", "bucket"),
    open_defecation_val = c("open_defecation"),
    undefined_val = c("other", "dnk")
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_sanitation_facility_cat" %in% names(out))
})


test_that("add_sanitation_facility_cat() — categorization works correctly", {

  df <- tibble::tibble(
    facility = c("flush_to_piped", "pit_lat", "open_defecation", "dnk")
  )

  out <- add_sanitation_facility_cat(
    .dataset = df,
    sanitation_facility_col = "facility",
    improved_facilities_val = c("flush_to_piped"),
    unimproved_facilities_val = c("pit_lat"),
    open_defecation_val = c("open_defecation"),
    undefined_val = c("dnk")
  )

  expect_true(grepl("Improved", out$wash_sanitation_facility_cat[1]))
  expect_true(grepl("Unimproved", out$wash_sanitation_facility_cat[2]))
  expect_true(grepl("Open Defecation", out$wash_sanitation_facility_cat[3]))
  expect_true(grepl("Undefined", out$wash_sanitation_facility_cat[4]))
})


test_that("add_sanitation_facility_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    facility = character(0)
  )

  expect_error(
    add_sanitation_facility_cat(
      .dataset = df_empty,
      sanitation_facility_col = "facility",
      improved_facilities_val = c("flush"),
      unimproved_facilities_val = c("pit"),
      open_defecation_val = c("open"),
      undefined_val = c("other")
    )
  )
})
