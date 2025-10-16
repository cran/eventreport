test_that("calc_mode_date returns the correct mode date", {
  result <- calc_mode_date(
    x = c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-03")
  )
  expect_equal(result, as.Date("2024-01-01"))
})

test_that("calc_mode_date returns the mean of modes date when there are multiple modes", {
  result <- calc_mode_date(
    x = c("2024-01-01", "2024-01-01", "2024-01-03", "2024-01-03")
  )
  expect_equal(result, as.Date("2024-01-02"))
})
