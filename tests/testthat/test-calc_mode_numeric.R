test_that("calc_mode_numeric returns the correct mode", {
  result <- calc_mode_numeric(
    x = c(1, 1, 1, 2, 3)
  )
  expect_equal(result, 1)
})

test_that("calc_mode_numeric returns the lowest mode for multi-modal vectors", {
  result <- calc_mode_numeric(
    x = c(1, 1, 2, 2)
  )
  expect_equal(result, 1)
})

test_that("calc_mode_numeric returns the lowest mode for multi-modal vectors", {
  result <- calc_mode_numeric(
    x = c(1, 1, 2, 2, 3, 3)
  )
  expect_equal(result, 1)
})
