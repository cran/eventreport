#' Calculate the modal confidence across event reports
#'
#' This function calculates the modal confidence score for one or more variables grouped by an event identifier.
#' The modal confidence score captures how dominant the most common value is within each event — that is,
#' the proportion of event reports that agree with the modal (most frequent) value for each variable.
#'
#' For each variable and event, the function computes the share of event reports that match the modal value.
#' These proportions are then averaged across all events to produce a single score per variable. The result is
#' a long-format dataframe that shows which variables tend to exhibit the greatest agreement in reporting.
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to assess modal confidence for.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{variable}{The name of each variable.}
#'   \item{modal_confidence}{The average share of reports per event that match the modal value.}
#' }
#'
#' @importFrom dplyr group_by summarise ungroup across mutate select everything any_of
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
#' df <- data.frame(
#'   event_id = c(1, 1, 2, 2, 3),
#'   actor1 = c("A", "A", "B", "C", "D"),
#'   deaths_best = c(10, 10, 5, 15, 10)
#' )
#' modal_confidence(
#'   df,
#'   group_var = "event_id",
#'   variables = c("actor1", "deaths_best")
#' )

modal_confidence <- function(data, group_var, variables) {
  # Ensure the input data is a dataframe
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  if (!is.character(group_var) || !(group_var %in% names(data))) {
    stop("group_var must be a character string and exist in the dataframe.")
  }

  if (!is.character(variables) || !all(variables %in% names(data))) {
    stop("All elements of variables must exist in the dataframe.")
  }

  data <- data %>%
    mutate(across(all_of(variables), as.character))

  modal_scores <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(
        all_of(variables),
        ~ {
          x <- na.omit(.)
          if (length(x) == 0) {
            NA_real_
          } else {
            tab <- table(x)
            max(tab) / sum(tab)
          }
        },
        .names = "mc_{.col}"
      ),
      .groups = "drop"
    ) %>%
    select(-any_of(group_var))

  modal_scores %>%
    summarise(
      across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      names_prefix = "mc_",
      values_to = "modal_confidence"
    )
}
