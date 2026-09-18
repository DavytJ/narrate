tab <- data.frame(
  site = c("A", "B"),
  n = c(1512, 1898),
  pct = c(22.46, 28.19),
  prop = c(0.0019, 0.0021)
)

test_that("verify_figures finds exact figures and diagnoses the rest", {
  text <- "A had 1,512 procedures (22.46%) and B 1898 (28.2%). Total: 3410. Mortality 0.19%."
  v <- verify_figures(text, tab, decimals = NULL, signif = NULL, decimal_mark = ".", big_mark = "")
  s <- attr(v, "summary")

  expect_s3_class(v, "narrate_verification")
  expect_equal(s$figures, 6)
  expect_equal(v$status[v$figure == "1,512"], "ok")
  expect_equal(v$status[v$figure == "22.46"], "ok")
  expect_equal(v$status[v$figure == "1898"], "ok")
  expect_equal(v$status[v$figure == "28.2"], "rounded")
  expect_equal(v$table_figure[v$figure == "28.2"], "28.19")
  expect_equal(v$status[v$figure == "3410"], "unsourced")
  expect_equal(v$status[v$figure == "0.19"], "proportion")
  expect_equal(s$flagged, 3)
})

test_that("verify_figures compares against the formatted table", {
  text <- "A had 22.5% and B 28.2%."
  v <- verify_figures(text, tab, decimals = 1, decimal_mark = ".", big_mark = "")
  expect_true(all(v$status == "ok"))
})

test_that("verify_figures works with comma decimal mark", {
  text <- "A tuvo 1.512 procedimientos (22,46 %) y B 28,2 %."
  v <- verify_figures(text, tab, decimals = NULL, decimal_mark = ",", big_mark = "")
  expect_equal(v$status, c("ok", "ok", "rounded"))
})

test_that("exclude prevents flagging years", {
  text <- "Period 2022 to 2025: 1512 cases."
  v <- verify_figures(text, tab, exclude = 2018:2030, decimals = NULL, decimal_mark = ".", big_mark = "")
  expect_equal(sum(v$status == "excluded"), 2)
  expect_equal(attr(v, "summary")$flagged, 0)
})

test_that("verify_figures counts [MISSING DATA] markers", {
  v <- verify_figures("Neither [MISSING DATA] nor [MISSING DATA].", tab, decimal_mark = ".")
  expect_equal(attr(v, "summary")$missing, 2)
})

test_that("verify_figures locates table, row and column", {
  v <- verify_figures("B: 1898.", tab, decimals = NULL, decimal_mark = ".", big_mark = "")
  expect_equal(v$row, 2L)
  expect_equal(v$column, "n")
  expect_equal(v$table, "table")
})

test_that("verify_figures accepts a named list of tables", {
  tables <- list(counts = tab[, c("site", "n")], rates = tab[, c("site", "pct")])
  v <- verify_figures("22.46 and 1512", tables, decimals = NULL, decimal_mark = ".", big_mark = "")
  expect_equal(v$table, c("rates", "counts"))
})

test_that("verify_figures validates tables", {
  expect_error(verify_figures("x", 1), "data frame")
})

test_that("verify_figures handles text without figures", {
  v <- verify_figures("No numbers here.", tab)
  expect_equal(nrow(v), 0)
  expect_equal(attr(v, "summary")$flagged, 0)
})
