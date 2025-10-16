#' Calculate the mean within-event standard deviation across event reports for numeric variables
#'
#' This function calculates the mean standard deviation for one or more numeric variables
#' grouped by an event identifier. It is useful for diagnosing aggregation sensitivity by
#' assessing how much variation exists in numeric values reported across event reports concerning
#' the same event.
#'
#' For each variable and event, the function computes the standard deviation of values
#' reported across event reports These values are then averaged across all events to produce
#' a single score per variable. The result is a long-format dataframe that shows which
#' numeric variables exhibit the most event report level disagreement
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to compute standard deviations for. All specified variables must be numeric.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{variable}{The name of each variable.}
#'   \item{mean_sd}{The mean standard deviation across events for that variable.}
#' }
#'
#' @importFrom dplyr group_by summarise ungroup across select starts_with everything any_of
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect any_of
#' @export
#'
#' @examples
#' df <- data.frame(
#'   event_id = c(1, 1, 2, 2, 3),
#'   country = c("US", "US", "UK", "UK", "CA"),
#'   actor1 = c("Actor A", "Actor B", "Actor B", "Actor C", "Actor D"),
#'   deaths_best = c(10, 20, 5, 15, 10)
#' )
#' mean_sd(
#'   df,
#'   group_var = "event_id",
#'   variables = c("deaths_best")
#' )

mean_sd <- function(data, group_var, variables) {
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

  # Check that all variables are numeric
  non_numeric_vars <- variables[!sapply(data[variables], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop(paste("The following variables are not numeric:", paste(non_numeric_vars, collapse = ", ")))
  }

  # Compute mean standard deviation per variable
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(
        all_of(variables),
        ~ sd(., na.rm = TRUE),
        .names = "sd_{.col}"
      ),
      .groups = "drop"
    ) %>%
    select(-any_of(group_var)) %>%  # drop group_var before computing means
    summarise(
      across(
        everything(),
        function(x) mean(x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      names_prefix = "sd_",
      values_to = "mean_sd"
    )
}
