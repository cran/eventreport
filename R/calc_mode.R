#' Calculate mode with optional tie-breaks
#'
#' This function calculates the mode of a given vector and optionally resolves ties using one or two levels of tie-breaks.
#'
#' @param x A character vector for which to find the mode.
#' @param tie_break An optional numeric vector used as the first tie-break criterion.
#' @param second_tie_break An optional numeric vector used as the second tie-break criterion when the first is insufficient.
#'
#' @return Returns the mode of `x`. If there are multiple modes and no tie-breaks are specified or they do not resolve the ties, returns "Indeterminate".
#' @export
#'
#' @examples
#' data <- c("apple", "apple", "banana", "banana")
#' tie_break <- c(1, 2, 1, 2)
#' second_tie_break <- c(1, 1, 2, 1)
#' calc_mode(data)  # Expect: "Indeterminate"
#' calc_mode(data, tie_break)  # Expect: "Indeterminate"
#' calc_mode(data, tie_break, second_tie_break)  # Expect: "banana"

calc_mode <- function(x, tie_break = NULL, second_tie_break = NULL) {
  # Calculate frequencies of each element
  freq <- table(x, useNA = "ifany")

  # Find the maximum frequency
  max_freq <- max(freq)

  # Identify potential modes
  modes <- names(freq)[freq == max_freq]

  # Convert modes to their original types
  modes <- ifelse(modes == "NA", NA, modes)

  # Helper function to apply tie-breaking logic
  apply_tie_break <- function(modes, tie_break) {
    if (is.null(tie_break)) return(modes)

    # Filter tie_break values that correspond to current modes
    modes_indices <- which(x %in% modes | is.na(x) & is.na(modes))
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
    if (is.na(modes)) {
      if (is.numeric(x)) {
        return(NA_real_)
      } else if (is.character(x)) {
        return(NA_character_)
      } else if (is.logical(x)) {
        return(NA)
      } else {
        return(NA)  # Default to logical NA for other types
      }
    }
    return(modes)
  } else {
    if (is.numeric(x)) {
      return(NA_real_)
    } else {
      return("Indeterminate")
    }
  }
}
