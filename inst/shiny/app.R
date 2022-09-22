####################################################################
### start the user interface code
####################################################################

ui <- fluidPage(
  shiny::titlePanel("Simulating party systems of a given size"),
  shiny::fluidRow(
    shiny::column(
      3,
      shiny::selectInput(
        "what",
        label = "Simulate... ",
        choices = c("seat-winning parties", "vote-winning parties")
      )
    ),
    shiny::column(
      2,
      shiny::numericInput(
        "n0",
        label = "Number of parties",
        value = 2,
        min = 2,
        max = 20
      )
    ),
    shiny::column(
      4,
      selectInput(
        "basis",
        label = "Concentration parameter",
        choices = c(
          "Posterior draws of alpha",
          "Freely chosen deterministic alpha",
          "Freely chosen stochastic alpha"
        ),
        selected = "Posterior draws (main text)"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.basis == 'Freely chosen deterministic alpha'",
      shiny::column(
        3,
        shiny::numericInput(
          "n_sim",
          label = "Number of simulations",
          value = 1000L,
          min = 1000L,
          max = 10000L
        ),
        shiny::numericInput(
          "alpha",
          label = "alpha",
          value = 39.14,
          min = 5,
          max = 80
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.basis == 'Freely chosen stochastic alpha'",
      shiny::column(
        3,
        shiny::numericInput(
          "n_sim",
          label = "Number of simulations",
          value = 1000L,
          min = 1000L,
          max = 10000L
        ),
        shiny::numericInput(
          "alpha",
          label = "alpha",
          value = 39.14,
          min = 5,
          max = 80
        ),
        shiny::numericInput(
          "sd_alpha",
          label = "SD of alpha",
          value = 0.72,
          min = 0.0,
          max = 10.0
        )
      )
    ),
    shiny::column(
      2,
      shiny::actionButton("sim", "Simulate Elections", class = "btn-lg btn-success")
    )
  ),

  shiny::tabsetPanel(
    shiny::tabPanel("Visualize top three shares",
                    shiny::plotOutput("hist")),
    shiny::tabPanel("See the simulated shares",
                    shiny::tableOutput("realizations"))
  ),
  shiny::htmlOutput("effnum"),
  shiny::textOutput("legtype"),
  shiny::HTML(
    "<h4>Notes</h4>
<dl>
<dt>What does this web page do?</dt>
<dd>It generates seat or vote shares for a party system with a given number of seat- or vote-winning parties.</dd>
<dt>What is it based on?</dt>
<dd>It's based on the working paper, 'Simulating party shares', by Denis Cohen and Chris Hanretty, currently under review at <em>Political Analysis</em>.</dd>
<dt>What is the 'concentration parameter'?</dt>
<dd>The concentration parameter (alpha) affects how closely the seat or vote shares match the theoretical pattern. We (Cohen and Hanretty) find that a concentration value of roughly 40 is best for seat shares, and a value of roughly 50 is best for vote shares. We provide users with three options for choosing alpha: You can use the exact posterior draws from the best-performing models of our paper, freely choose a deterministic (i.e., fixed) value for alpha, or freely choose a stochastic value for alpha. In the latter case, alpha will be generated from a normal distribution for which you must set the standard deviation. By and large: If you want more variability (less concentration), choose lower values for alpha. If you want less variability, choose higher values. </dd>
</dl>
"
  )
)



####################################################################
### start the server code
####################################################################

server <- function(input, output, session) {
  ## Backend operations
  x <- shiny::eventReactive(input$sim,
                            {
                              sharesimulatoR::simulate_shares(
                                N0 = input$n0,
                                what = input$what,
                                basis = input$basis,
                                alpha = input$alpha,
                                sd_alpha = input$sd_alpha,
                                n_sim = input$n_sim,
                                seed = 20220920L
                              )
                            })


  output$hist <- shiny::renderPlot({
    par(mfrow = c(min(c(3, input$n0)), 1),
        mar = c(5, 2, 1, 1))
    for (i in 1:min(c(3, input$n0))) {
      hist(
        100 * x()[, i],
        main = NULL,
        col = RColorBrewer::brewer.pal(3, "Dark2")[i],
        xlab = ifelse(
          input$what == "vote-winning parties",
          "Vote share (%)",
          "Seat share (%)"
        ),
        xlim = c(0, 100 * max(x())),
        breaks = 20
      )
    }
  })

  output$realizations <- shiny::renderTable({
    stats::setNames(as.data.frame(100 * x(),
                                  row.names = NULL),
                    paste0("Party ", 1:ncol(x())))
  })

  output$effnum <- shiny::renderUI({
    shiny::HTML(
      glue::glue(
        "The <a href = 'https://en.wikipedia.org/wiki/Effective_number_of_parties'>effective number</a> of {input$what} is {round(mean(apply(x(), 1, sharesimulatoR:::effnum)), 2)}. "
      ),
      ifelse(
        input$what == "seat-winning parties",
        glue::glue(
          "The probability of a single party having a majority of seats is {round(100 * mean(apply(x(), 1, function(s) any(s >= 0.5))))}%. "
        ),
        glue::glue("")
      ),
      glue::glue(
        "In 90% of elections, the share of the largest party should fall between {round(100 * quantile(apply(x(), 1, max), probs = 0.1))} and  {round(100 * quantile(apply(x(), 1, max), probs = 0.9))} percent. "
      ),
      ifelse(
        input$what == "seat-winning parties",
        glue::glue(
          "In the largest number of simulations, this party system fell into <a href = 'https://www.jstor.org/stable/24363566'>legislative type</a> {sharesimulatoR:::most_common_or_random(apply(x(), 1, sharesimulatoR:::leg_type))}. "
        ),
        glue::glue("")
      )
    )


  })

}

shiny::shinyApp(ui = ui, server = server)
