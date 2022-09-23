#' @title Simulate party shares for a party system of a given size
#'
#' @description Simulates vote or seat shares for a party system of size N0, given the supplied concentration parameter.
#'
#' @param N0 Number of seat-winning/vote-winning parties (i.e., party system
#' size).
#' @param what What to simulate: seat or vote-winning parties. Affects the
#' choice of the concentration parameter if
#' \code{basis == "Posterior draws of alpha"}.
#' @param basis Specifies which concentration parameter will be used to
#' simulate party systems. \code{"Posterior draws of alpha"} uses 4000
#' posterior draws from the best-performing models from Cohen and Hanretty
#' (2022). \code{"Freely chosen deterministic alpha"} generates \code{n_sim}
#' draws using a fixed concentration parameter supplied per \code{alpha}.
#' \code{"Freely chosen sthochastic alpha"} generates \code{n_sim}
#' draws of alpha from a normal distribution with mean \code{alpha} and
#' standard deviation \code{sd_alpha} and uses each draw for one
#' corresponding party system simulation.
#' @param alpha User-supplied (mean) concentration parameter.
#' @param sd_alpha User-supplied standard deviation of the concentration
#' parameter.
#' @param n_sim Number of simulations. Fixed to the number of posterior draws
#' supplied by the package data unless users request freely chosen
#' deterministic/stochastic concentration parameters.
#' @param seed A seed set for reproducability.
#'
#' @return Returns a matrix which contains \code{n_sim} simulations (in rows) for N0 parties (in columns).
#'
#' @export

simulate_shares <- function(N0,
                            what = c("seat-winning parties",
                                     "vote-winning parties"),
                            basis = c(
                              "Posterior draws of alpha",
                              "Freely chosen deterministic alpha",
                              "Freely chosen stochastic alpha"
                            ),
                            alpha = NULL,
                            sd_alpha = 0.0,
                            n_sim = 1000L,
                            seed = 20220920L) {
  ## Set seed
  set.seed(seed)

  ## Set defaults
  if (length(what) > 1) {
      message("Multiple values passed to `what`. Picking first value. ")
      what <- what[1]
  }
  if (length(basis) > 1) {
      message("Multiple values passed to `basis`. Picking first value. ")
      basis <- basis[1]
  }

  ## Get alpha
  if (basis == "Posterior draws of alpha") {
    if (what == "seat-winning parties") {
      alpha_draws <-
        sharesimulatoR::posterior_draws$unordered_logical_seats
      n_sim <- length(alpha_draws)
    } else if (what == "vote-winning parties") {
      alpha_draws <-
        sharesimulatoR::posterior_draws$unordered_logical_votes
      n_sim <- length(alpha_draws)
    }

    ## Simulate elections
    simulated_elections <-
      sapply(alpha_draws, function (alpha) {
        gtools::rdirichlet(n = 1L,
                           alpha = alpha * sharesimulatoR:::party_sizes(N0))
      }) %>%
      t()

  } else if (basis == "Freely chosen deterministic alpha") {
    simulated_elections <- gtools::rdirichlet(n = n_sim,
                                              alpha = alpha * sharesimulatoR:::party_sizes(N0))
  } else if (basis == "Freely chosen stochastic alpha") {
    alpha_draws <- rnorm(n_sim, alpha, sd_alpha)
    simulated_elections <-
      sapply(alpha_draws, function (alpha) {
        gtools::rdirichlet(n = 1L,
                           alpha = alpha * sharesimulatoR:::party_sizes(N0))
      }) %>%
      t()
  }

  ## Value
  return(simulated_elections)
}
