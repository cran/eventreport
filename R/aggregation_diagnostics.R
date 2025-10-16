#' Compute multiple aggregation diagnostics for a set of variables
#'
#' This convenience function runs all six diagnostic functions in the package,
#' mean divergence, normalized divergence, mean standard deviation, mean range,
#' share of events with disagreement, and modal confidence, and returns a combined
#' tibble with one row per variable.
#'
#' The function handles mixed-type input: each diagnostic is only run on the subset
#' of variables for which it is valid. Variables that do not apply to a particular
#' diagnostic will have `NA` in that column.
#'
#' @param data A data frame containing event report level data.
#' @param group_var A character string naming the column that uniquely identifies events (e.g., "event_id").
#' @param variables A character vector of column names to include in the diagnostics.
#'
#' @return A tibble with one row per variable and columns:
#' \describe{
#'   \item{variable}{The name of each variable.}
#'   \item{dscore}{Mean divergence score.}
#'   \item{dscore_normalized}{Normalized divergence score.}
#'   \item{mean_sd}{Mean within-event standard deviation (numeric variables only).}
#'   \item{mean_range}{Mean within-event range (numeric variables only).}
#'   \item{share_disagreement}{Share of events with any disagreement.}
#'   \item{modal_confidence}{Average modal confidence per variable.}
#' }
#'
#' #' @importFrom dplyr full_join
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom tidyselect where
#'
#' @examples
#' small_maverick_event_report %>%
#'   aggregation_diagnostics(
#'     group_var = "event_id",
#'     variables = c("city", "deaths_best", "actor1")
#'    )
#'
#' @export

aggregation_diagnostics <- function(data, group_var, variables) {
  # Ensure input validity
  if (!is.data.frame(data)) {
    stop("Input data must be a dataframe.")
  }

  if (!is.character(group_var) || !(group_var %in% names(data))) {
    stop("group_var must be a character string and exist in the dataframe.")
  }

  if (!is.character(variables) || !all(variables %in% names(data))) {
    stop("All elements of variables must exist in the dataframe.")
  }

  # Identify numeric variables
  numeric_vars <- variables[sapply(data[variables], is.numeric)]

  # Run all diagnostics (suppress warnings for partial input)
  dscore_raw <- tryCatch(
    mean_dscore(data, group_var, variables, normalize = FALSE, plot = FALSE) %>%
      dplyr::rename(`Mean divergence` = dscore),
    error = function(e) tibble::tibble(variable = character(), `Mean divergence` = numeric())
  )

  dscore_norm <- tryCatch(
    mean_dscore(data, group_var, variables, normalize = TRUE, plot = FALSE) %>%
      dplyr::rename(`Normalized divergence` = dscore),
    error = function(e) tibble::tibble(variable = character(), `Normalized divergence` = numeric())
  )

  sdscore <- if (length(numeric_vars) > 0) {
    mean_sd(data, group_var, numeric_vars)
  } else {
    tibble::tibble(variable = character(), mean_sd = numeric())
  }

  rangescore <- if (length(numeric_vars) > 0) {
    mean_range(data, group_var, numeric_vars)
  } else {
    tibble::tibble(variable = character(), mean_range = numeric())
  }

  share_disagree <- tryCatch(
    share_disagreement(data, group_var, variables),
    error = function(e) tibble::tibble(variable = character(), share_disagreement = numeric())
  )

  modal_conf <- tryCatch(
    modal_confidence(data, group_var, variables),
    error = function(e) tibble::tibble(variable = character(), modal_confidence = numeric())
  )

  # Join all diagnostics
  full_join_all <- purrr::reduce(
    list(dscore_raw, dscore_norm, sdscore, rangescore, share_disagree, modal_conf),
    ~ dplyr::full_join(.x, .y, by = "variable")
  )

  # Rename columns to human-readable labels
  full_join_all <- full_join_all %>%
    dplyr::rename(
      Variable = variable,
      `Mean standard deviation` = mean_sd,
      `Mean range` = mean_range,
      `Share of events with disagreement (%)` = share_disagreement,
      `Modal confidence (%)` = modal_confidence
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    ) %>%
    # Replace NA values with empty strings (and coerce to character)
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(is.na(.x), "", formatC(.x, digits = 2, format = "f"))
      )
    )

  return(full_join_all)
}
