# Tests for add_health_barriers

test_that("add_health_barriers() — valid dataset creates barrier indicators", {

  df <- tibble::tibble(
    health_barriers = c(
      "long distance,cost",
      "poor quality,unsafe facilities",
      "no healthcare available",
      "no barriers",
      "other"
    )
  )

  out <- suppressMessages(add_health_barriers(
    .dataset = df,
    health_barriers_col = "health_barriers",
    physical_access_barriers_val = c("long distance", "transport issues"),
    financial_access_barriers_val = c("cost", "expensive"),
    safety_access_barriers_val = c("unsafe facilities", "insecurity"),
    quality_barriers_val = c("poor quality", "lack of equipment"),
    healthcare_seeking_barriers_val = c("refusal", "cultural reasons"),
    availability_barriers_val = c("no healthcare available", "services unavailable"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  ))
  expect_equal(nrow(out), 5)
  expect_true("health_barrier_any.physical" %in% names(out))
  expect_true("health_barrier_any.financial" %in% names(out))
  expect_true("health_barrier_any.safety" %in% names(out))
  expect_true("health_barrier_any.quality" %in% names(out))
  expect_true("health_barrier_any.availability" %in% names(out))
  expect_true("health_barrier_any.other" %in% names(out))
  expect_true("health_barrier_any.none" %in% names(out))
})


test_that("add_health_barriers() — barrier detection works correctly", {

  df <- tibble::tibble(
    barriers = c(
      "long distance",
      "cost",
      "unsafe facilities",
      "no barriers"
    )
  )

  out <- suppressMessages(add_health_barriers(
    .dataset = df,
    health_barriers_col = "barriers",
    physical_access_barriers_val = c("long distance"),
    financial_access_barriers_val = c("cost"),
    safety_access_barriers_val = c("unsafe facilities"),
    quality_barriers_val = c("poor quality"),
    healthcare_seeking_barriers_val = c("refusal"),
    availability_barriers_val = c("no healthcare"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  ))
  expect_equal(out$health_barrier_any.physical[1], 1)
  expect_equal(out$health_barrier_any.financial[2], 1)
  expect_equal(out$health_barrier_any.safety[3], 1)
  expect_equal(out$health_barrier_any.none[4], 1)
})


test_that("add_health_barriers() — multiple barriers detected in one response", {

  df <- tibble::tibble(
    barriers = c("long distance,cost,unsafe facilities")
  )

  out <- suppressMessages(add_health_barriers(
    .dataset = df,
    health_barriers_col = "barriers",
    physical_access_barriers_val = c("long distance"),
    financial_access_barriers_val = c("cost"),
    safety_access_barriers_val = c("unsafe facilities"),
    quality_barriers_val = c("poor quality"),
    healthcare_seeking_barriers_val = c("refusal"),
    availability_barriers_val = c("no healthcare"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  ))
  expect_equal(out$health_barrier_any.physical[1], 1)
  expect_equal(out$health_barrier_any.financial[1], 1)
  expect_equal(out$health_barrier_any.safety[1], 1)
})


test_that("add_health_barriers() — error on empty dataset", {

  df_empty <- tibble::tibble(
    barriers = character(0)
  )

  expect_error(
    add_health_barriers(
      .dataset = df_empty,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})


test_that("add_health_barriers() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("barriers")
  )

  expect_error(
    add_health_barriers(
      .dataset = df,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})


test_that("add_health_barriers() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    barriers = c("long distance"),
    health_barrier_any.physical = 0
  )

  expect_warning(
    add_health_barriers(
      .dataset = df,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("long distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})
