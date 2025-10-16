#' Calculate the mean divergence scores across event reports
#'
#' This function calculates the mean divergence score for one or more variables grouped by an event identifier.
#' The divergence score captures how often values for a given variable differ across event reports describing the same event.
#'
#' For each variable and event, the function computes the number of unique values reported, subtracts one, and averages these
#' values across all events. This reflects how much inconsistency exists across sources. Optionally, the scores can be
#' normalized by the total number of unique values observed for each variable across the dataset. The result is a long-format
#' dataframe showing which variables are most sensitive to aggregation. A plotting option is also available.
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to compute divergence scores for.
#' @param normalize Logical, indicating whether to normalize the scores by the total number of unique values for each variable.
#' @param plot Logical, indicating whether to return a ggplot object visualizing the scores.
#'
#' @return Either a tibble or a ggplot object, depending on the value of \code{plot}.
#' If \code{plot = FALSE}, returns a tibble with two columns:
#' \describe{
#'   \item{variable}{The name of each variable.}
#'   \item{dscore}{The mean divergence score or normalized score.}
#' }
#' If \code{plot = TRUE}, returns a lollipop-style plot showing divergence scores by variable.
#'
#' @importFrom dplyr group_by summarise ungroup across mutate left_join select starts_with
#' @importFrom tidyr pivot_longer everything
#' @importFrom ggplot2 ggplot aes geom_segment geom_point coord_flip theme_bw labs theme element_text margin element_blank scale_y_continuous
#' @importFrom scales percent
#' @export
#'
#' @examples
#' df <- data.frame(
#'   event_id = c(1, 1, 2, 2, 3),
#'   country = c("US", "US", "UK", "UK", "CA"),
#'   actor1 = c("Actor A", "Actor B", "Actor B", "Actor C", "Actor D"),
#'   deaths_best = c(10, 20, 5, 15, 10)
#' )
#' mean_dscore(df, "event_id", c("country", "actor1", "deaths_best"), normalize = TRUE, plot = TRUE)

mean_dscore <- function(data, group_var, variables, normalize = FALSE, plot = FALSE) {
  # Ensure the input data is a dataframe
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  # Check if group_var and variables are correct
  if (!is.character(group_var) || !(group_var %in% names(data))) {
    stop("group_var must be a character string and exist in the dataframe.")
  }
  if (!is.character(variables) || !all(variables %in% names(data))) {
    stop("All elements of variables must exist in the dataframe.")
  }

  # Convert all specified variables to character to treat them uniformly
  data <- data %>%
    mutate(across(all_of(variables), as.character))

  # Calculate the dscore per group and variable
  result <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(across(all_of(variables), ~ mean(length(unique(., na.rm = FALSE))) - 1, .names = "dscore_{.col}")) %>%
    ungroup() %>%
    summarise(across(starts_with("dscore"), mean))

  if (normalize) {
    # Calculate total unique values for normalization, including NAs as unique values
    total_uniques <- data %>%
      summarise(across(all_of(variables), ~ length(unique(., na.rm = FALSE)), .names = "total_{.col}")) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "total",
                   names_prefix = "total_")

    # Normalize the mean scores
    result <- result %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "mean",
                   names_prefix = "dscore_") %>%
      left_join(total_uniques, by = c("variable" = "variable")) %>%
      mutate(dscore = mean / total) %>%
      select(variable, dscore)
  } else {
    # Convert results to the required long format
    result <- result %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "dscore",
                   names_prefix = "dscore_")
  }

  # Clean the variable names by removing prefixes
  result$variable <- sub("dscore_", "", result$variable)

  # Return the result or plot based on the plot argument
  if (plot) {
    y_label <- ifelse(normalize, "Share of maximum possible divergence", "Mean divergence")

    return(
      ggplot(result, aes(x = variable, y = dscore)) +
        geom_segment(
          aes(x = variable, xend = variable, y = 0, yend = dscore),
          color = "black"
        ) +
        geom_point(size = 4, color = "black", fill = "black", shape = 21) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() +
        theme_bw() +
        labs(
          title = "Divergence score by variable",
          x = NULL,
          y = y_label
        ) +
        theme(
          plot.title = element_text(size = 14),
          plot.margin = margin(1, 1, 1, 1, "cm"),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )

    )
  } else {
    return(result)
  }
}
