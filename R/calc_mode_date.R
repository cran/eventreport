#' Calculate mode of date vector
#' @param x A character vector where each element is a date in "YYYY-MM-DD" format.
#' @return Returns a date vector representing the modal date, or the mean of the modal dates if there is a tie.
#' @importFrom lubridate ymd
#' @importFrom stats na.omit
#' @examples
#' calc_mode_date(c("2021-01-01", "2021-01-02", "2021-01-01"))
#' @export

calc_mode_date <- function(x) {
    if (!is.character(x)) {
      warning("All 'x' inputs must be a character variable representing a date in the format YYYY-MM-DD.")
      return(rep(as.Date(NA), length(x)))
    }

    x_dates <- lubridate::ymd(x, quiet = TRUE)
    x_dates <- na.omit(x_dates)

    if (length(x_dates) == 0) {
      return(as.Date(NA))
    }

    tbl <- table(x_dates)
    max_count <- max(tbl)
    mode_values <- as.Date(names(tbl)[tbl == max_count])

    if (length(mode_values) == 1) {
      return(mode_values)
    } else {
      numeric_dates <- as.numeric(mode_values)
      mean_numeric_date <- mean(numeric_dates)
      mean_date <- as.Date(floor(mean_numeric_date), origin = "1970-01-01")
      return(mean_date)
    }
}
