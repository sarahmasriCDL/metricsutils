test_that("last_non_na returns last non-missing value", {
  x <- c(NA, 1, NA, 3, NA)
  expect_equal(last_non_na(x), 3)
})

test_that("last_non_na returns NA if all values are NA", {
  x <- c(NA, NA, NA)
  expect_true(is.na(last_non_na(x)))
})



test_that("first_last_numeric_summary computes correct values", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2),
    session = c(1, 2, 3, 1, 2),
    value = c(10, NA, 30, 5, 15)
  )
  
  result <- first_last_numeric_summary(df, id, session, value)
  
  expect_equal(result$first_value, c(10, 5))
  expect_equal(result$last_value, c(30, 15))
  expect_equal(result$change, c(20, 10))
})



test_that("first_last_numeric_summary handles all NA values", {
  df <- data.frame(
    id = c(1, 1, 1),
    session = c(1, 2, 3),
    value = c(NA, NA, NA)
  )
  
  result <- first_last_numeric_summary(df, id, session, value)
  
  expect_true(is.na(result$last_value))
})



test_that("first_last_logical_summary tracks boolean changes", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2),
    session = c(1, 2, 3, 1, 2),
    flag = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  
  result <- first_last_logical_summary(df, id, session, flag)
  
  expect_equal(result$first_value, c(FALSE, FALSE))
  expect_equal(result$last_value, c(TRUE, FALSE))
})



test_that("count_true_by_group counts correctly", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    flag = c(TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  
  result <- count_true_by_group(df, group, flag, "n_true")
  
  expect_equal(result$n_true, c(1, 2))
})