test_that("tally_wide produces correct contingency tables", {
  # basic test with counts
  df <- tibble::tibble(
    group = c("A", "A", "B", "B", "B"),
    outcome = c("yes", "no", "yes", "yes", "no")
  )
  
  res <- tally_wide(df, rows = group, cols = outcome)
  
  # expected structure: one row per group, columns for outcomes
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("A", "B") %in% res$group))
  expect_true(all(c("yes", "no") %in% names(res)))
  expect_equal(res$yes[res$group == "A"], 1)
  expect_equal(res$no[res$group == "A"], 1)
  expect_equal(res$yes[res$group == "B"], 2)
  expect_equal(res$no[res$group == "B"], 1)
  
  # test with weights
  df2 <- tibble::tibble(
    group = c("A", "A", "B"),
    outcome = c("yes", "yes", "no"),
    weight = c(2, 3, 5)
  )
  
  res2 <- tally_wide(df2, rows = group, cols = outcome, wt = weight)
  
  expect_equal(res2$yes[res2$group == "A"], 5) # 2 + 3
  expect_equal(res2$no[res2$group == "B"], 5)
})

test_that("tally_wide respects values_fill argument", {
  df <- tibble::tibble(
    group = c("A", "B"),
    outcome = c("yes", "yes")
  )
  
  # Without values_fill, missing cells are NA
  res_na <- tally_wide(df, rows = group, cols = outcome)
  expect_true(any(is.na(res_na$no)))
  
  # With values_fill = 0, missing cells are 0
  res_zero <- tally_wide(df, rows = group, cols = outcome, values_fill = list(n = 0))
  expect_equal(res_zero$no[res_zero$group == "A"], 0)
  expect_equal(res_zero$no[res_zero$group == "B"], 0)
})
