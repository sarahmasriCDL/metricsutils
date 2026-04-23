test_that("clean_money handles basic cases", {
  x <- c("$1,200", "2.5k", "3M", "N/A", "")
  result <- clean_money(x)
  
  expect_equal(result[1], 1200)
  expect_equal(result[2], 2500)
  expect_equal(result[3], 3000000)
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
})


test_that("clean_money handles currency conversion", {
  x <- c("USD 2.5K", "3M")
  currency <- c(NA, "USD")
  
  result <- clean_money(x, currency, rates = c(CAD = 1, USD = 1.37))
  
  expect_equal(result[1], 2500 * 1.37)
  expect_equal(result[2], 3000000 * 1.37)
})


test_that("clean_money handles messy real-world strings", {
  x <- c("US$7k approx.", "CAD 4.2M", "~1,500", "1.2 million")
  
  result <- clean_money(x, rates = c(CAD = 1, USD = 1.37))
  
  expect_equal(result[1], 7000 * 1.37)
  expect_equal(result[2], 4200000)
  expect_equal(result[3], 1500)
  expect_equal(result[4], 1200000)
})


test_that("clean_money uses outer total when present", {
  x <- "$1.75M CAD ($750k CAD closed, $1M CAD committed)"
  result <- clean_money(x)
  expect_equal(result, 1750000)
})


test_that("clean_money handles committed note without double counting", {
  x <- "$250k USD (committed)"
  result <- clean_money(x, rates = c(CAD = 1, USD = 1.37))
  expect_equal(result, 250000 * 1.37)
})


test_that("clean_money adds multiple amounts when no outer total exists", {
  x <- "$750k CAD closed, $1M CAD committed"
  result <- clean_money(x)
  expect_equal(result, 1750000)
})


test_that("clean_money uses fallback currency column", {
  x <- "250k committed"
  currency <- "USD"
  result <- clean_money(x, currency = currency, rates = c(CAD = 1, USD = 1.37))
  expect_equal(result, 250000 * 1.37)
})