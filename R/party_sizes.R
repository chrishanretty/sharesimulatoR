#' @title party_sizes
#'
#' @description Internal function.
#'
#' @return Returns the predicted shares for each party in a system of size N0
#' according to the logical model.
#'
#' @noRd

party_sizes <- function(n) {
  out <- vector(mode = "numeric", length = n)
  out[1] <- 1 / sqrt(n)
  for (i in 2:n) {
    out[i] <- 1 / sqrt(n - i + 1) * (1 - sum(out[1:(i - 1)]))
  }
  out
}
