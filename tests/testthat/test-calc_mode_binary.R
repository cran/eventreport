test_that("calc_mode_binary returns the correct mode value", {
  result <- calc_mode_binary(
    x = c(0, 0, 1, 1, 1)
  )
  expect_equal(result, 1)
})

test_that("calc_mode_binary returns the highest mode value when there are multiple modes", {
  result <- calc_mode_binary(
    x = c(0, 0, 1, 1)
  )
  expect_equal(result, 1)
})

test_that("calc_mode_binary returns the correct mode value", {
  result <- calc_mode_binary(
    x = c(0, 0, 0, 1, 1)
  )
  expect_equal(result, 0)
})
