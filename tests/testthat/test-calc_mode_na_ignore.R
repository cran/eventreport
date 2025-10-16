test_that("calc_mode_na_ignore returns the correct mode", {
  result <- calc_mode_na_ignore(
    x = c("", "", "", "a")
  )
  expect_equal(result, "a")
})

test_that("calc_mode_na_ignore returns the correct mode when using one tie-break", {
  result <- calc_mode_na_ignore(
    x = c("a", "a", "b", "b", "", ""),
    tie_break = c(1, 2, 1, 1, 1, 1)
  )
  expect_equal(result, "a")
})

test_that("calc_mode_na_ignore returns the correct mode when using two tie-breaks", {
  result <- calc_mode_na_ignore(
    x = c("a", "a", "b", "b", "", ""),
    tie_break = c(1, 2, 1, 1, 1, 1),
    second_tie_break = c(1, 1, 2, 1, 1, 1)
  )
  expect_equal(result, "a")
})

test_that("calc_mode_na_ignore returns the correct mode when the true mode is NA_character_", {
  result <- calc_mode_na_ignore(
    x = c("a", "a", "b", "c", NA_character_, NA_character_)
  )
  expect_equal(result, "a")
})

test_that("calc_mode_na_ignore returns the correct mode when the true mode is an empty string", {
  result <- calc_mode_na_ignore(
    x = c("a", "a", "b", "c", "", "", "")
  )
  expect_equal(result, "a")
})

test_that("calc_mode_na_ignore returns the correct mode when the vector includes only empty strings and NA_character_", {
  result <- calc_mode_na_ignore(
    x = c("", "", "", "", NA_character_, NA_character_)
  )
  expect_equal(result, "")
})

