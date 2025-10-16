#' Calculate the share of events with any disagreement across event reports
#'
#' This function calculates the proportion of events for which two or more distinct values
#' are reported for each specified variable. It is useful for identifying which variables
#' are most commonly inconsistent across event reports describing the same event.
#'
#' For each event and variable, the function checks whether all values reported across event
#' reports are identical. It then calculates the share of events for which at least two different
#' values are reported. The result is a long-format dataframe that highlights which variables
#' most frequently exhibit inter-source disagreement.
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to check for disagreement.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{variable}{The name of each variable.}
#'   \item{share_disagreement}{The proportion of events with disagreement for that variable.}
#' }
#'
#' @importFrom dplyr group_by summarise ungroup across select everything any_of
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
#' share_disagreement(
#'   df,
#'   group_var = "event_id",
#'   variables = c("actor1", "deaths_best")
#' )

share_disagreement <- function(data, group_var, variables) {
  # Ensure the input data is a dataframe
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  # Check if group_var is a character and exists in the dataframe
  if (!is.character(group_var) || !(group_var %in% names(data))) {
    stop("group_var must be a character string and exist in the dataframe.")
  }

  # Check if variables is a character vector and all elements exist in the dataframe
  if (!is.character(variables) || !all(variables %in% names(data))) {
    stop("All elements of variables must exist in the dataframe.")
  }

  # Convert variables to character for consistent comparison
  data <- data %>%
    mutate(across(all_of(variables), as.character))

  # For each group, flag whether there is more than one unique value per variable
  disagreement_flags <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(
        all_of(variables),
        ~ length(unique(.)) > 1,
        .names = "disagree_{.col}"
      ),
      .groups = "drop"
    ) %>%
    select(-any_of(group_var))  # Drop group_var before computing means

  # Calculate the share of events with disagreement for each variable
  disagreement_flags %>%
    summarise(
      across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      names_prefix = "disagree_",
      values_to = "share_disagreement"
    )
}
