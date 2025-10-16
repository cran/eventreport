test_that("calc_max_precision returns the most precise value", {
  result <- calc_max_precision(
    x = c("Tranas", "Smaland", "Sweden"),
    precision_var = c(3, 2, 1)
  )
  expect_equal(result, "Tranas")
})

test_that("calc_max_precision returns Indeterminate when there are multiple least precise values", {
  result <- calc_max_precision(
    x = c("Tranas", "Aneby", "Sweden"),
    precision_var = c(3, 3, 1)
  )
  expect_equal(result, "Indeterminate")
})

test_that("calc_max_precision returns NA when there are multiple least precise values and the variable is numeric", {
  result <- calc_max_precision(
    x = c(0, 7, 13),
    precision_var = c(3, 3, 1)
  )
  expect_equal(result, NA_real_)
})

test_that("calc_max_precision returns the most precise value when using one tie-break", {
  result <- calc_max_precision(
    x = c("Tranas", "Aneby", "Sweden"),
    precision_var = c(3, 3, 1),
    tie_break = c(1, 2, 1)
  )
  expect_equal(result, "Aneby")
})

test_that("calc_max_precision returns the most precise value when using two tie-breaks", {
  result <- calc_max_precision(
    x = c("Tranas", "Aneby", "Sweden"),
    precision_var = c(3, 3, 1),
    tie_break = c(1, 1, 1),
    second_tie_break = c(1, 3, 1)
  )
  expect_equal(result, "Aneby")
})


