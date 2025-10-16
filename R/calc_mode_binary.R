#' Calculate mode of a binary numeric vector
#' @param x A numeric vector consisting only of binary values (0 and 1).
#' @return Returns a numeric vector representing the mode value. Returns 1 if there is a tie. Returns `NA` if the vector is empty.
#' @examples
#' calc_mode_binary(c(0, 1, 1, 0, 1))
#' @export

calc_mode_binary <- function(x) {
    if (!is.numeric(x) || any(!x %in% c(0, 1))) {
      warning("Input 'x' must be a numeric binary variable consisting only of 0 and 1.")
      return(NA)
    }

    x_filtered <- x[!is.na(x)]
    if(length(x_filtered) == 0) return(NA)

    tbl <- table(x_filtered, useNA = "no")
    max_count <- max(tbl)
    mode_values <- as.numeric(names(tbl)[tbl == max_count])

    if(length(mode_values) > 1) return(1)
    return(mode_values[1])
}
