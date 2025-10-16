#' Calculate discrepancy score
#'
#' This function computes the mean number of unique values minus one for each specified variable
#' within each group specified by the group_var. It is designed to provide insights into the
#' variability of each variable while adjusting for the minimum possible unique count.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param group_var A character string specifying the column name used for grouping the data.
#' @param variables A character vector of column names in `data` for which the mean number of
#' unique values minus one is calculated.
#'
#' @return A tibble with each specified variable showing the mean of (unique values - 1)
#' for each group. The data is grouped by the `group_var` and returns the results in a wide format,
#' where each variable is prefixed with "dscore_" to indicate the calculation.
#'
#' @export
#' @importFrom dplyr group_by summarise across ungroup
#' @examples
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "B"),
#'   age = c(25, 25, 30, 35, 30),
#'   gender = c("Male", "Male", "Female", "Female", "Female"),
#'   income = c(50000, 50000, 60000, 65000, 60000)
#' )
#' result <- dscore(df, "group", c("age", "gender", "income"))
#' print(result)

dscore <- function(data, group_var, variables) {
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

  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(across(all_of(variables), ~ mean(length(unique(.))) - 1, .names = "dscore_{.col}")) %>%
    ungroup()
}
