#' Aggregate event report data
#'
#' This function aggregates event report data based on a specified grouping variable and various aggregation criteria.
#'
#' @param data A data frame containing the data to be aggregated.
#' @param group_var A string specifying the variable to group by. Default is "event_id".
#' @param find_mode A vector of variable names for which to find the mode.
#' @param find_mode_na_ignore A vector of variable names for which to find the mode, ignoring NAs.
#' @param find_mode_bin A vector of variable names for which to find the binary mode.
#' @param find_mode_date A vector of variable names for which to find the mode for dates.
#' @param find_mode_numeric A vector of variable names for which to find the mode for numeric values.
#' @param find_least_precise A list of lists, each containing a variable name and its corresponding precision variable, to find the least precise value.
#' @param find_most_precise A list of lists, each containing a variable name and its corresponding precision variable, to find the most precise value.
#' @param combine_strings A vector of variable names for which to combine strings.
#' @param find_max A vector of variable names for which to find the maximum value.
#' @param find_min A vector of variable names for which to find the minimum value.
#' @param summarize_vars A vector of variable names for which to sum all values.
#' @param aggregation_name A string specifying the name of the aggregation.
#' @param tie_break A string specifying the tie break column name. Default is "default_tie_break".
#' @param second_tie_break A string specifying the second tie break column name. Default is "default_tie_break".
#'
#' @return A data frame with the aggregated results.
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr all_of
#' @importFrom dplyr n
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom rlang syms
#' @importFrom rlang quo
#' @importFrom rlang expr
#' @importFrom stats setNames
#' @examples
#' small_maverick_event_report %>%
#'   aggregateData(group_var = "event_id", find_mode = "city") %>%
#'   utils::head(10)

aggregateData <- function(
    data, group_var = "event_id",
    find_mode = NULL, find_mode_na_ignore = NULL, find_mode_bin = NULL, find_mode_date = NULL,
    find_mode_numeric = NULL, find_least_precise = NULL, find_most_precise = NULL,
    combine_strings = NULL, find_max = NULL, find_min = NULL, summarize_vars = NULL, aggregation_name = NULL,
    tie_break = "default_tie_break", second_tie_break = "default_tie_break"
) {
  suppressWarnings({
    if (!"default_tie_break" %in% names(data)) {
      data$default_tie_break <- 1
    }

    summarise_list <- list()

    if (!is.null(find_mode)) {
      cat_vars <- syms(find_mode)
      summarise_list[find_mode] <- purrr::map(cat_vars, ~quo(calc_mode(!!.x, !!sym(tie_break), !!sym(second_tie_break))))
    }

    if (!is.null(find_mode_na_ignore)) {
      na_ignore_vars <- syms(find_mode_na_ignore)
      summarise_list[find_mode_na_ignore] <- purrr::map(na_ignore_vars, ~quo(calc_mode_na_ignore(!!.x, !!sym(tie_break), !!sym(second_tie_break))))
    }

    if (!is.null(find_mode_bin)) {
      bin_vars <- syms(find_mode_bin)
      summarise_list[find_mode_bin] <- purrr::map(bin_vars, ~quo(calc_mode_binary(!!.x)))
    }

    if (!is.null(find_mode_date)) {
      date_vars <- syms(find_mode_date)
      summarise_list[find_mode_date] <- purrr::map(date_vars, ~quo(calc_mode_date(!!.x)))
    }

    if (!is.null(find_mode_numeric)) {
      numeric_vars <- syms(find_mode_numeric)
      summarise_list[find_mode_numeric] <- purrr::map(numeric_vars, ~quo(calc_mode_numeric(!!.x)))
    }

    if (!is.null(combine_strings)) {
      string_vars <- syms(combine_strings)
      summarise_list[combine_strings] <- purrr::map(string_vars, ~quo(aggregate_strings(!!.x)))
    }

    if (!is.null(find_max)) {
      max_vars <- syms(find_max)
      summarise_list[find_max] <- purrr::map(max_vars, ~quo(max(!!.x, na.rm = TRUE)))
    }

    if (!is.null(find_min)) {
      min_vars <- syms(find_min)
      summarise_list[find_min] <- purrr::map(min_vars, ~quo(min(!!.x, na.rm = TRUE)))
    }

    if (!is.null(find_least_precise)) {
      summarise_list <- c(summarise_list, setNames(map(find_least_precise, ~ {
        var_name <- .x[["var"]]
        precision_var_name <- .x[["precision_var"]]
        var <- sym(var_name)
        precision_var <- sym(precision_var_name)
        quo(calc_min_precision(.data[[!!var]], .data[[!!precision_var]], .data[[!!sym(tie_break)]], .data[[!!sym(second_tie_break)]]))
      }), sapply(find_least_precise, function(x) x$var)))
    }

    if (!is.null(find_most_precise)) {
      summarise_list <- c(summarise_list, setNames(map(find_most_precise, ~ {
        var_name <- .x[["var"]]
        precision_var_name <- .x[["precision_var"]]
        var <- sym(var_name)
        precision_var <- sym(precision_var_name)
        quo(calc_max_precision(.data[[!!var]], .data[[!!precision_var]], .data[[!!sym(tie_break)]], .data[[!!sym(second_tie_break)]]))
      }), sapply(find_most_precise, function(x) x$var)))
    }

    if (!is.null(summarize_vars)) {
      sum_vars <- syms(summarize_vars)
      summarise_list[summarize_vars] <- purrr::map(sum_vars, ~quo(sum(!!.x, na.rm = TRUE)))
    }

    result <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        !!!summarise_list,
        number_of_sources = n(),
        .groups = "drop"
      ) %>%
      mutate(
        unit_of_analysis = "Event",
        aggregation = aggregation_name
      )
  })

  return(result)
}
