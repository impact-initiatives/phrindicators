# Tests for add_drinking_water_jmp_ladder

test_that("add_drinking_water_jmp_ladder() — valid dataset creates JMP ladder", {

  df <- tibble::tibble(
    wash_drinking_water_source_cat = c("improved", "improved", "unimproved", "surface_water"),
    wash_drinking_water_time = c("under_30min", "over_30min", "over_30min", "under_30min")
  )

  out <- suppressMessages(add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "wash_drinking_water_source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "wash_drinking_water_time",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  ))
  expect_equal(nrow(out), 4)
  expect_true("wash_jmp_ladder_drinking_water_cat" %in% names(out))
  expect_s3_class(out$wash_jmp_ladder_drinking_water_cat, "factor")
})


test_that("add_drinking_water_jmp_ladder() — categorization logic is correct", {

  df <- tibble::tibble(
    source_cat = c("improved", "improved", "unimproved", "surface_water"),
    time_cat = c("under_30min", "over_30min", "under_30min", "under_30min")
  )

  out <- suppressMessages(add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "time_cat",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  ))
  expect_true(grepl("Basic", out$wash_jmp_ladder_drinking_water_cat[1]))
  expect_true(grepl("Limited", out$wash_jmp_ladder_drinking_water_cat[2]))
  expect_true(grepl("Unimproved", out$wash_jmp_ladder_drinking_water_cat[3]))
  expect_true(grepl("Surface Water", out$wash_jmp_ladder_drinking_water_cat[4]))
})


test_that("add_drinking_water_jmp_ladder() — error on empty dataset", {

  df_empty <- tibble::tibble(
    source = character(0),
    time = character(0)
  )

  expect_error(
    add_drinking_water_jmp_ladder(
      .dataset = df_empty,
      drinking_water_source_cat_col = "source",
      drinking_water_source_cat_improved_val = "improved",
      drinking_water_source_cat_unimproved_val = "unimproved",
      drinking_water_source_cat_surface_water_val = "surface_water",
      drinking_water_time_col = "time",
      drinking_water_time_under_30min_val = "under_30min",
      drinking_water_time_over_30min_val = "over_30min"
    )
  )
})


test_that("add_drinking_water_jmp_ladder() — ordered factor levels are correct", {

  df <- tibble::tibble(
    source_cat = c("improved", "improved", "unimproved", "surface_water"),
    time_cat = c("under_30min", "over_30min", "under_30min", "under_30min")
  )

  out <- suppressMessages(add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "time_cat",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  ))
  expect_true(is.ordered(out$wash_jmp_ladder_drinking_water_cat))
  levels_order <- levels(out$wash_jmp_ladder_drinking_water_cat)
  expect_true(grepl("Basic", levels_order[1]))
  expect_true(grepl("Surface Water", levels_order[4]))
})
