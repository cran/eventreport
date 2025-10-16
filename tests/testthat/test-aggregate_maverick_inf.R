test_that("aggregate_maverick_inf returns correct actor role scores", {

  library(dplyr)

  data <- aggregate_maverick_inf()

  # Test: Ensure the sum of actor*_initiator is never > 1, ignoring NA values
  data <- data %>%
    mutate(
      initiator_sum = rowSums(
        select(., starts_with("actor") & ends_with("_initiator")),
        na.rm = TRUE
      )
    )

  expect_true(
    all(data$initiator_sum <= 1, na.rm = TRUE),
    info = "The sum of actor*_initiator columns exceeds 1 in at least one row."
  )

  # Test: Ensure rows with actor*_bystander == 1 do not have a sum > 0 for corresponding actor*_initiator, actor*_perpetrator, and actor*_intervener
  data <- data %>%
    mutate(
      bystander_conflict_sum = rowSums(
        select(., starts_with("actor") & ends_with("_bystander")) *
          (select(., starts_with("actor") & ends_with("_initiator")) +
             select(., starts_with("actor") & ends_with("_perpetrator")) +
             select(., starts_with("actor") & ends_with("_intervener"))),
        na.rm = TRUE
      )
    )

  expect_true(
    all(data$bystander_conflict_sum == 0, na.rm = TRUE),
    info = "Rows with actor*_bystander == 1 have conflicting roles in initiator, perpetrator, or intervener columns."
  )
})

