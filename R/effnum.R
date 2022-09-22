#' @title effnum
#'
#' @description Internal function.
#'
#' @return Returns the calculated number of effective parties.
#'
#' @noRd

effnum <- function(x) {
  1 / sum(x ^ 2)
}
