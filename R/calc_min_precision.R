#' Calculate the mode value at minimum precision
#'
#' This function determines the mode of a variable `x`, filtered to entries with the minimum value of a specified precision vector `precision_var`.
#' It optionally resolves ties using one or two additional vectors for tie-breaking.
#'
#' @param x A vector of values for which to find the mode.
#' @param precision_var A vector of precision values corresponding to `x`, used to filter to minimum values.
#' @param tie_break Optional; a vector used as the first tie-break criterion.
#' @param second_tie_break Optional; a vector used as the second tie-break criterion.
#'
#' @return Returns the mode of `x` for entries with minimum `precision_var` value. If no valid entries exist, returns an empty string.
#' @export
#'
#' @examples
#' x = c("apple", "apple", "banana", "banana")
#' precision_var = c(1, 2, 1, 2)
#' tie_break = c(1, 2, 1, 2)
#' second_tie_break = c(1, 1, 2, 1)
#' calc_min_precision(x, precision_var, tie_break, second_tie_break)

calc_min_precision <- function(x, precision_var, tie_break = NULL, second_tie_break = NULL) {
  # Check for all NA precision values
  if (all(is.na(precision_var))) {
    return(NA_character_)
  }

  # Filter to minimum precision values
  min_precision <- min(precision_var, na.rm = TRUE)
  valid_indices <- precision_var == min_precision

  x_filtered <- x[valid_indices]
  tie_break_filtered <- if (!is.null(tie_break)) tie_break[valid_indices] else NULL
  second_tie_break_filtered <- if (!is.null(second_tie_break)) second_tie_break[valid_indices] else NULL

  # Return empty string if no entries left after filtering
  if (length(x_filtered) == 0) {
    return("")
  }

  # Use calc_mode to determine the mode with potential tie-breaking
  mode_value <- calc_mode(x_filtered, tie_break_filtered, second_tie_break_filtered)

  # Ensure the mode_value has the same type as the input x
  if (is.numeric(x)) {
    return(as.numeric(mode_value))
  } else {
    return(as.character(mode_value))
  }
}
