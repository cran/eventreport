test_that("calc_min_precision returns the least precise value", {
  result <- calc_min_precision(
    x = c("Tranas", "Smaland", "Sweden"),
    precision_var = c(3, 2, 1)
  )
  expect_equal(result, "Sweden")
})

test_that("calc_min_precision returns Indeterminate when there are multiple least precise values", {
  result <- calc_min_precision(
    x = c("Tranas", "Finland", "Sweden"),
    precision_var = c(3, 1, 1)
  )
  expect_equal(result, "Indeterminate")
})

test_that("calc_min_precision returns NA when there are multiple least precise values and the variable is numeric", {
  result <- calc_min_precision(
    x = c(0, 2, 4),
    precision_var = c(3, 1, 1)
  )
  expect_equal(result, NA_real_)
})

test_that("calc_min_precision returns the least precise value when using one tie-break", {
  result <- calc_min_precision(
    x = c("Tranas", "Finland", "Sweden"),
    precision_var = c(3, 1, 1),
    tie_break = c(1, 2, 1)
  )
  expect_equal(result, "Finland")
})

test_that("calc_min_precision returns the least precise value when using two tie-breaks", {
  result <- calc_min_precision(
    x = c("Tranas", "Finland", "Sweden"),
    precision_var = c(3, 1, 1),
    tie_break = c(1, 1, 1),
    second_tie_break = c(1, 1, 2)
  )
  expect_equal(result, "Sweden")
})

