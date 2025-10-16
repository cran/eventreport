test_that("aggregateData returns the mode value when using the find_mode argument", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Tranas", "Smaland", "Smaland", "Sweden")
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_mode = "city"
  )

  expect_equal(result$city, "Smaland")
})

test_that("aggregateData returns the mode value when using the find_mode argument for multiple variables", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Tranas", "Smaland", "Smaland", "Sweden"),
    actor = c("Police", "Police", "Army", "Unknown")
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_mode = c("city", "actor")
  )

  expect_equal(result$city, "Smaland")
  expect_equal(result$actor, "Police")
})


test_that("aggregdateData returns the most precise value when using multiple precision variables", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Tranas", "Smaland", "Smaland", "Sweden"),
    geo_precision = c(3, 2, 2, 1),
    actor = c("Police", "Police", "Army", "Insatsstyrkan"),
    actor_precision = c(1, 1, 1, 2)
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_most_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "actor", precision_var = "actor_precision")
    )
  )

  expect_equal(result$city, "Tranas")
  expect_equal(result$actor, "Insatsstyrkan")
})

test_that("aggregdateData returns the least precise value when using multiple precision variables", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Tranas", "Smaland", "Smaland", "Sweden"),
    geo_precision = c(3, 2, 2, 1),
    actor = c("Unknown", "Police", "Army", "Insatsstyrkan"),
    actor_precision = c(0, 1, 1, 2)
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_least_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "actor", precision_var = "actor_precision")
    )
  )

  expect_equal(result$city, "Sweden")
  expect_equal(result$actor, "Unknown")
})

test_that("aggregdateData returns the most precise value when using multiple precision variables and tie-breaks", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Smaland", "Smaland", "Tranas", "Aneby"),
    geo_precision = c(1, 1, 2, 2),
    actor = c("Police", "Police", "Tranaspolisen", "Insatsstyrkan"),
    actor_precision = c(1, 1, 2, 2),
    tie_break = c(1, 1, 1, 1),
    second_tie_break = c(1, 1, 3, 1)
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_most_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "actor", precision_var = "actor_precision")
    ),
    tie_break = "tie_break",
    second_tie_break = "second_tie_break"
  )

  expect_equal(result$city, "Tranas")
  expect_equal(result$actor, "Tranaspolisen")
})

test_that("aggregdateData returns the least precise value when using multiple precision variables and tie-breaks", {

  data <- data.frame(
    event_id = c(1, 1, 1, 1),
    city = c("Ostergotland", "Smaland", "Tranas", "Aneby"),
    geo_precision = c(1, 1, 2, 2),
    actor = c("Army", "Police", "Tranaspolisen", "Insatsstyrkan"),
    actor_precision = c(1, 1, 2, 2),
    tie_break = c(1, 3, 1, 1),
    second_tie_break = c(1, 1, 1, 1)
  )

  result <- aggregateData(
    data = data,
    group_var = "event_id",
    find_least_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "actor", precision_var = "actor_precision")
    ),
    tie_break = "tie_break",
    second_tie_break = "second_tie_break"
  )

  expect_equal(result$city, "Smaland")
  expect_equal(result$actor, "Police")
})
