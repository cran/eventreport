#' Combine strings from a character variable
#'
#' This function combines strings from a character variable.
#'
#' @param str_var A character vector.
#' @return Returns a single character string with unique strings concatenated by semicolons.
#' @importFrom dplyr summarise group_by
#' @importFrom purrr possibly
#' @examples
#' aggregate_strings(c("apple", "banana", "apple", "Unknown", "orange", " "))
#' @export

aggregate_strings <- function(str_var) {
  safe_aggregate_strings <- possibly(function(str_var) {
    if (!is.character(str_var)) {
      warning("Input 'str_var' must be a character variable.")
      return("")
    }

    cleaned_strings <- unique(
      unlist(
        strsplit(
          paste(unique(str_var), collapse = "; "), ";[[:space:]]*"
        )
      )
    )
    cleaned_strings <- cleaned_strings[!grepl("^\\s*$", cleaned_strings) & cleaned_strings != "Unknown"]
    return(paste(unique(cleaned_strings), collapse = "; "))
  }, otherwise = "", quiet = FALSE)

  safe_aggregate_strings(str_var)
}
