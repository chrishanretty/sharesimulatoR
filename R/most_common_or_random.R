#' @title most_common_or_random
#'
#' @description Internal function.
#'
#' @return Returns and/or stores names from a table whose entries are sorted
#' in decreasing order.
#'
#' @noRd

most_common_or_random <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}
