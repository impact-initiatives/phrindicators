# Tests for add_drinking_water_source_cat

test_that("add_drinking_water_source_cat() — valid dataset creates water source category", {

  df <- tibble::tibble(
    wash_water_source = c("piped_dwelling", "unprotected_well", "surface_water", "dnk")
  )

  out <- add_drinking_water_source_cat(
    .dataset = df,
    drinking_water_source_col = "wash_water_source"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_drinking_water_source_cat" %in% names(out))
})


test_that("add_drinking_water_source_cat() — categorization works correctly", {

  df <- tibble::tibble(
    source = c("piped_dwelling", "unprotected_well", "surface_water", "other")
  )

  out <- add_drinking_water_source_cat(
    .dataset = df,
    drinking_water_source_col = "source"
  )

  expect_true(grepl("Improved", out$wash_drinking_water_source_cat[1]))
  expect_true(grepl("Unimproved", out$wash_drinking_water_source_cat[2]))
  expect_true(grepl("Surface Water", out$wash_drinking_water_source_cat[3]))
  expect_true(grepl("Undefined", out$wash_drinking_water_source_cat[4]))
})


test_that("add_drinking_water_source_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    source = character(0)
  )

  expect_error(
    add_drinking_water_source_cat(
      .dataset = df_empty,
      drinking_water_source_col = "source"
    )
  )
})


test_that("add_drinking_water_source_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("piped_dwelling")
  )

  expect_error(
    add_drinking_water_source_cat(
      .dataset = df,
      drinking_water_source_col = "source"
    )
  )
})
