#' @title Interactively simulate realistic party systems in a Shiny GUI
#'
#' @description Runs a ShinyApp that guides users through the steps
#' required for simulate party shares of a given size.
#'
#'
#' @export

shiny_sharesimulatoR <- function() {
  appDir <-
    system.file("shiny", package = "sharesimulatoR")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `sharesimulatoR`.",
         call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
