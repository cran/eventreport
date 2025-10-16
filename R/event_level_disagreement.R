#' Calculate event-level disagreement scores by variable (wide format)
#'
#' This function calculates the level of disagreement across event reports for each event and variable.
#' For a given event and variable, it computes 1 minus the proportion of reports that agree with the modal value.
#' A score of 0 indicates full agreement, while higher scores indicate greater disagreement.
#'
#' The result is a wide-format tibble with one row per event and one column per variable.
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to check for disagreement.
#'
#' @return A wide-format tibble where each row is an event and each column is a disagreement score for a variable.
#'
#' @importFrom dplyr group_by summarise ungroup across mutate everything any_of
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
#' df <- data.frame(
#'   event_id = c(1, 1, 2, 2, 3),
#'   actor1 = c("Actor A", "Actor B", "Actor B", "Actor B", "Actor C"),
#'   deaths_best = c(10, 10, 5, 15, 10)
#' )
#' event_level_disagreement(
#'   df,
#'   group_var = "event_id",
#'   variables = c("actor1", "deaths_best")
#' )

event_level_disagreement <- function(data, group_var, variables) {
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  if (!is.character(group_var) || !(group_var %in% names(data))) {
    stop("group_var must be a character string and exist in the dataframe.")
  }

  if (!is.character(variables) || !all(variables %in% names(data))) {
    stop("All elements of variables must exist in the dataframe.")
  }

  # Convert to character for consistent modal comparison
  data <- data %>%
    mutate(across(all_of(variables), as.character))

  # For each event, compute disagreement score for each variable
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(
        all_of(variables),
        ~ {
          x <- na.omit(.)
          if (length(x) == 0) {
            NA_real_
          } else {
            1 - (max(table(x)) / length(x))
          }
        },
        .names = "{.col}"
      ),
      .groups = "drop"
    )
}
