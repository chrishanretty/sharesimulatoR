#' @title Plot simulated party shares for a party system of a given size
#'
#' @description Plots simulated vote or seat shares for a party system of size
#' N0, given the supplied concentration parameter.
#'
#' @param N0 Number of seat-winning/vote-winning parties (i.e., party system
#' size).
#' @param main Main title (optional).
#' @param max_parties Maximum number of (top-ranking) parties to include in the
#' plot.
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
#' deterministic/stochastic concentration parameters."
#' @param seed A seed set of reproducability.
#' @param combine Logical; if \code{TRUE}, histograms for up to
#' \code{max_parties} parties will be shown in a single plot.
#'
#' @return Returns a matrix which contains \code{n_sim} simulations (in rows) for N0 parties (in columns).
#'
#' @export

plot_simulated_shares <- function(N0,
                                  main = NULL,
                                  max_parties = 3L,
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
                                  seed = 20220920L,
                                  combine = FALSE) {

    ## Config
    the_pal <- c("#1b9e77b3", "#d95f02b3", "#7570b3b3")
    
  ## Simulate
  simulated_shares <- sharesimulatoR::simulate_shares(N0,
                                                      what,
                                                      basis,
                                                      alpha,
                                                      sd_alpha,
                                                      n_sim,
                                                      seed)

  ## Plot
  if (!combine) {
    par(mfrow = c(min(c(max_parties, N0)), 1),
        mar = c(5, 2, 1, 1))
  } else {
    par(mar = c(5, 2, ifelse(is.null(main), 1, 3), 1))
  }
  for (i in 1:min(c(max_parties, N0))) {
    if (i == 1) {
      hist(
        100 * simulated_shares[, i],
        main = ifelse(combine, main, NULL),
        col = the_pal[i],
        xlab = ifelse(
          what == "vote-winning parties",
          "Vote share (%)",
          "Seat share (%)"
        ),
        xlim = c(0, 100 * max(simulated_shares)),
        breaks = 50,
        border = "#ffffff80"
      )
    } else {
      hist(
        100 * simulated_shares[, i],
        col = the_pal[i],
        breaks = 50,
        border = "#ffffff80",
        add = combine
      )
    }
  }
}
