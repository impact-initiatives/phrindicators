#' Validate loop dataset counts against parent dataset
#'
#' @description
#' Checks that the number of loop records per parent UUID matches the expected
#' count recorded in the parent dataset. Identifies mismatches and orphan records.
#'
#' @param parent_df Data frame: Parent dataset (e.g., household data)
#' @param child_df Data frame: Loop/child dataset (e.g., individual roster)
#' @param parent_uuid Character: Column name for UUID in parent dataset
#' @param child_uuid Character: Column name for parent UUID in child dataset
#' @param parent_count Character: Column name for expected count in parent dataset
#' @param soft_missing_parent Logical: If TRUE, allows missing counts when actual=0; if FALSE, flags as mismatch
#'
#' @return Character vector of UUIDs with mismatches
#' @export
loop_count_check <- function(parent_df,
                             child_df,
                             parent_uuid,
                             child_uuid,
                             parent_count,
                             soft_missing_parent = TRUE) {

  phrutils::phr_try({


    # 1. Structural validation

    if (!parent_uuid %in% names(parent_df)) {
      phrutils::phr_error("LoopCheck",
                  glue::glue("Parent UUID column '{parent_uuid}' not found in parent dataset.")
      )
    }

    if (!child_uuid %in% names(child_df)) {
      phrutils::phr_error("LoopCheck",
                  glue::glue("Child UUID column '{child_uuid}' not found in child dataset.")
      )
    }

    if (!parent_count %in% names(parent_df)) {
      phrutils::phr_error("LoopCheck",
                  glue::glue("Parent count column '{parent_count}' missing in parent dataset.")
      )
    }


    # 2. Prepare vectors

    parent_ids <- as.character(parent_df[[parent_uuid]])
    expected_counts <- parent_df[[parent_count]]

    # Compute loop counts
    child_ids <- as.character(child_df[[child_uuid]])
    actual_counts <- table(child_ids)

    # Convert table \u2192 numeric vector
    actual_counts <- as.numeric(actual_counts)
    names(actual_counts) <- names(table(child_ids))

    # Build comparison table
    df_out <- data.frame(
      uuid = parent_ids,
      expected = expected_counts,
      actual = actual_counts[parent_ids],
      stringsAsFactors = FALSE
    )

    df_out$actual[is.na(df_out$actual)] <- 0

    mismatches <- c()


    # 3. Loop UUIDs not in parent \u2192 mismatch

    extra_loop_ids <- setdiff(names(table(child_ids)), parent_ids)

    if (length(extra_loop_ids) > 0) {
      mismatches <- c(mismatches, extra_loop_ids)
      phrutils::phr_warning(
        "LoopCheck",
        glue::glue("Loop dataset contains UUIDs not found in parent dataset: {paste(extra_loop_ids, collapse=', ')}")
      )
    }


    # 4. Parent expected = NA AND actual > 0 \u2192 mismatch

    problem_na_child <- df_out$uuid[
      is.na(df_out$expected) & df_out$actual > 0
    ]

    if (length(problem_na_child) > 0) {
      mismatches <- c(mismatches, problem_na_child)
      phrutils::phr_warning(
        "LoopCheck",
        glue::glue("Parent has missing expected count but loop contains rows for: {paste(problem_na_child, collapse=', ')}")
      )
    }


    # 5. NEW RULE: Parent expected = NA AND actual = 0

    if (!soft_missing_parent) {
      problem_na_soft <- df_out$uuid[
        is.na(df_out$expected) & df_out$actual == 0
      ]

      if (length(problem_na_soft) > 0) {
        mismatches <- c(mismatches, problem_na_soft)
        phrutils::phr_warning(
          "LoopCheck",
          glue::glue("Missing count detected for UUIDs with no loop rows under 'hard' rule: {paste(problem_na_soft, collapse=', ')}")
        )
      }
    }


    # 6. Numeric mismatch when expected is known

    numeric_mismatch <- df_out$uuid[
      !is.na(df_out$expected) &
        df_out$expected != df_out$actual
    ]

    if (length(numeric_mismatch) > 0) {
      mismatches <- c(mismatches, numeric_mismatch)
      phrutils::phr_warning(
        "LoopCheck",
        glue::glue("Count mismatches detected for: {paste(numeric_mismatch, collapse=', ')}")
      )
    }

    mismatches <- unique(mismatches)


    # 7. Return results

    return(list(
      ok = (length(mismatches) == 0),
      mismatches = mismatches,
      details = df_out
    ))

  }, on_error = "warn", origin = "loop_count_check")
}
