# Tests for add_sanitation_jmp_ladder

test_that("add_sanitation_jmp_ladder() — valid dataset creates JMP ladder", {

  df <- tibble::tibble(
    wash_sanitation_facility_cat = c("improved", "improved", "unimproved", "open_defecation"),
    wash_shared_sanitation = c("no", "yes", "no", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "wash_sanitation_facility_cat",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "wash_shared_sanitation",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_jmp_ladder_sanitation_cat" %in% names(out))
  expect_s3_class(out$wash_jmp_ladder_sanitation_cat, "factor")
})


test_that("add_sanitation_jmp_ladder() — categorization logic is correct", {

  df <- tibble::tibble(
    facility = c("improved", "improved", "unimproved", "open_defecation"),
    shared = c("no", "yes", "yes", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "facility",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "shared",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_true(grepl("Basic", out$wash_jmp_ladder_sanitation_cat[1]))
  expect_true(grepl("Limited", out$wash_jmp_ladder_sanitation_cat[2]))
  expect_true(grepl("Unimproved", out$wash_jmp_ladder_sanitation_cat[3]))
  expect_true(grepl("Open Defecation", out$wash_jmp_ladder_sanitation_cat[4]))
})


test_that("add_sanitation_jmp_ladder() — ordered factor levels are correct", {

  df <- tibble::tibble(
    facility = c("improved", "improved", "unimproved", "open_defecation"),
    shared = c("no", "yes", "yes", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "facility",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "shared",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_true(is.ordered(out$wash_jmp_ladder_sanitation_cat))
  levels_order <- levels(out$wash_jmp_ladder_sanitation_cat)
  expect_true(grepl("Basic", levels_order[1]))
  expect_true(grepl("Open Defecation", levels_order[4]))
})


test_that("add_sanitation_jmp_ladder() — error on empty dataset", {

  df_empty <- tibble::tibble(
    facility = character(0),
    shared = character(0)
  )

  expect_error(
    add_sanitation_jmp_ladder(
      .dataset = df_empty,
      sanitation_facility_cat_col = "facility",
      sanitation_facility_improved_val = "improved",
      sanitation_facility_unimproved_val = "unimproved",
      sanitation_facility_open_defecation_val = "open_defecation",
      shared_sanitation_col = "shared",
      shared_sanitation_yes_val = "yes",
      shared_sanitation_no_val = "no"
    )
  )
})
