test_that("format_number keeps whole numbers and applies significant digits", {
  expect_equal(
    format_number(c(1512, 22.46, 0.0019841), signif = 2, decimal_mark = ",", big_mark = ""),
    c("1512", "22", "0,0020")
  )
})

test_that("format_number applies fixed decimals and big mark", {
  expect_equal(
    format_number(c(6732, 22.46, 2.78), decimals = 1, decimal_mark = ",", big_mark = "."),
    c("6.732", "22,5", "2,8")
  )
})

test_that("format_number without rounding keeps the decimals", {
  expect_equal(
    format_number(c(22.46, 0.5), decimals = NULL, signif = NULL, decimal_mark = ".", big_mark = ""),
    c("22.46", "0.5")
  )
})

test_that("format_number handles NA", {
  expect_equal(
    format_number(c(NA, 3), decimals = 1, decimal_mark = ".", big_mark = ""),
    c(NA, "3")
  )
})

test_that("format_number validates its arguments", {
  expect_error(format_number(1, decimals = "a"), "whole number")
  expect_error(format_number(1, decimal_mark = 1), "single string")
})

test_that("table_to_markdown produces a pipe table", {
  tab <- data.frame(a = c("x", "y"), n = c(1, 2.5))
  md <- table_to_markdown(tab, decimals = 1, decimal_mark = ".", big_mark = "")
  expect_equal(md[[1]], "| a | n |")
  expect_equal(md[[2]], "|---|---|")
  expect_equal(md[[4]], "| y | 2.5 |")
})

test_that("table_to_markdown adds a title", {
  tab <- data.frame(a = 1)
  md <- table_to_markdown(tab, title = "Table 1")
  expect_equal(md[[1]], "**Table 1**")
})
