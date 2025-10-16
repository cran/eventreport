#' Calculate mode of numeric vector
#'
#' This function calculates the mode of a given numeric vector, and returns the smallest mode value if multiple modes exist.
#'
#' @param x A numeric vector.
#' @return Returns a numeric vector representing the mode value. Returns the smallest mode value if multiple modes exist, and NA if the vector is empty or contains non-numeric elements.
#' @examples
#' calc_mode_numeric(c(1, 2, 2, 3, 4, 4))
#' @export

calc_mode_numeric <- function(x) {
  if (!is.numeric(x)) {
    warning("Input 'x' must be a numeric variable.")
    return(NA)
  }

  x_filtered <- x[!is.na(x)]
  if(length(x_filtered) == 0) return(NA)

  tbl <- table(x_filtered, useNA = "no")
  max_count <- max(tbl)
  mode_values <- as.numeric(names(tbl)[tbl == max_count])

  return(min(mode_values, na.rm = TRUE))
}
