#' Calculate mode with optional tie-breaks ignoring NA and empty strings
#'
#' This function calculates the mode of a given vector, ignoring `NA` and empty strings, and optionally resolves ties using one or two levels of tie-breaks.
#' If all values are `NA` or empty, the function returns `NA`.
#'
#' @param x A character vector for which to find the mode.
#' @param tie_break An optional numeric vector used as the first tie-break criterion.
#' @param second_tie_break An optional numeric vector used as the second tie-break criterion when the first is insufficient.
#'
#' @return Returns the mode of `x` ignoring `NA` and empty strings. If the filtered vector is empty or all elements are `NA` or empty, returns `NA`.
#' @export
#'
#' @examples
#' data <- c("apple", "", "banana", NA)
#' tie_break <- c(1, NA, 1, NA)
#' second_tie_break <- c(1, NA, 2, NA)
#' calc_mode_na_ignore(data)  # Expect: "apple"
#' calc_mode_na_ignore(data, tie_break)  # Expect: "banana"
#' calc_mode_na_ignore(data, tie_break, second_tie_break)  # Expect: "banana"

calc_mode_na_ignore <- function(x, tie_break = NULL, second_tie_break = NULL) {
  # Filter out NA and empty strings
  valid_indices <- which(x != "" & !is.na(x))
  x <- x[valid_indices]

  # Adjust tie-breaks if they are provided
  if (!is.null(tie_break)) {
    tie_break <- tie_break[valid_indices]
  }
  if (!is.null(second_tie_break)) {
    second_tie_break <- second_tie_break[valid_indices]
  }

  # Return empty string if all elements are NA or empty
  if (length(x) == 0) {
    return("")
  }

  # Calculate frequencies of each element
  freq <- table(x)

  # Find the maximum frequency
  max_freq <- max(freq)

  # Identify potential modes
  modes <- names(freq)[freq == max_freq]

  # Helper function to apply tie-breaking logic
  apply_tie_break <- function(modes, tie_break) {
    if (is.null(tie_break)) return(modes)

    # Filter tie_break values that correspond to current modes
    modes_indices <- which(x %in% modes)
    tie_values <- tie_break[modes_indices]

    # Find the index(es) of the highest tie-break value
    highest_indices <- which(tie_values == max(tie_values))

    # Select mode(s) corresponding to the highest tie-break value
    selected_modes <- unique(x[modes_indices[highest_indices]])
    return(selected_modes)
  }

  # Resolve ties with the first tie-break
  if (!is.null(tie_break) && length(tie_break) == length(x) && length(modes) > 1) {
    modes <- apply_tie_break(modes, tie_break)
  }

  # If still tied, apply second tie-break
  if (!is.null(second_tie_break) && length(second_tie_break) == length(x) && length(modes) > 1) {
    modes <- apply_tie_break(modes, second_tie_break)
  }

  # Final check on number of modes
  if (length(modes) == 1) {
    return(modes)
  } else {
    return("Indeterminate")
  }
}
