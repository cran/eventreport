test_that("calc_mode returns the correct mode", {
  result <- calc_mode(x = c("a", "b", "b", "b"))
  expect_equal(result, "b")
})

test_that("calc_mode returns the correct mode", {
  result <- calc_mode(x = c("", "", "a", "b"))
  expect_equal(result, "")
})

test_that("calc_mode returns the correct mode", {
  result <- calc_mode(c("a", "b", "a", "b"))
  expect_equal(result, "Indeterminate")
})

test_that("calc_mode returns the correct mode when using one tie-break", {
  result <- calc_mode(
    x = c("a", "a", "b", "b"),
    tie_break = c(1, 1, 2, 1)
  )
  expect_equal(result, "b")
})

test_that("calc_mode returns the correct mode when using two tie-breaks", {
  result <- calc_mode(
    x = c("a", "a", "b", "b"),
    tie_break = c(1, 1, 1, 1),
    second_tie_break = c(1, 1, 2, 1)
  )
  expect_equal(result, "b")
})

test_that("calc_mode returns the correct mode when there are both empty strings and NA values", {
  result <- calc_mode(
    x = c("a", "b", "", "", NA)
  )
  expect_equal(result, "")
})

test_that("calc_mode returns the correct mode when the mode value is NA_character_", {
  result <- calc_mode(
    x = c("a", "b", "", NA_character_, NA_character_, NA_character_)
  )
  expect_equal(result, NA_character_)
})

test_that("calc_mode returns the correct mode when the mode value is NA", {
  result <- calc_mode(
    x = c(TRUE, FALSE, FALSE, NA, NA, NA)
  )
  expect_equal(result, NA)
})

test_that("calc_mode returns the correct mode when the mode value is NA_real_", {
  result <- calc_mode(
    x = c(1, 2, 3, NA_real_, NA_real_, NA_real_)
  )
  expect_equal(result, NA_real_)
})

test_that("calc_mode returns the correct mode when the mode value is NA_character_ and tie breaks are specified", {
  result <- calc_mode(
    x = c("a", "b", "", NA_character_, NA_character_, NA_character_),
    tie_break = c(1, 1, 1, 1, 1, 1),
    second_tie_break = c(1, 1, 2, 1, 1, 1)
  )
  expect_equal(result, NA_character_)
})

test_that("calc_mode returns the correct mode when the mode value is NA and tie breaks are specified", {
  result <- calc_mode(
    x = c(TRUE, FALSE, FALSE, NA, NA, NA),
    tie_break = c(1, 1, 1, 1, 1, 1),
    second_tie_break = c(1, 1, 2, 1, 1, 1)
  )
  expect_equal(result, NA)
})

test_that("calc_mode returns the correct mode when the mode value is NA_real_ and tie breaks are specified", {
  result <- calc_mode(
    x = c(1, 2, 3, NA_real_, NA_real_, NA_real_),
    tie_break = c(1, 1, 1, 1, 1, 1),
    second_tie_break = c(1, 1, 2, 1, 1, 1)
  )
  expect_equal(result, NA_real_)
})
