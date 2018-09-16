### npred_tab ----
npred_tab <- tabItem(
  tabName = "npred_tab",
  p("What proportion of variance is accounted for by chance alone in linear regressions with different numbers of uninformative predictors? Set the number of observations and number of predictors below. This app will simulate linear regressions predicting a random number with your set number of random predictors (and all possible interactions)."),
  fluidRow( # start row 1
    column( # start column 1
      width = 4,
      box(
        title = "Parameters",
        width = NULL,
        sliderInput("npred_n", "Observations:", min = 25, max = 500, value = 100, step = 25),
        sliderInput("npred_vars", "Variables:", min = 0, max = 10, value = 5, step = 1),
        sliderInput("npred_reps", "Number of runs in this simulation", 
                    min = 100, max = 1000, step = 100, value = 500),
        actionButton("npred_resim", "Re-Simulate")
      )
    ), # end column 1
    column( # start column 2
      width = 8,
      box(
        title = "Variance predicted by chance alone",
        width = 12,
        plotOutput(outputId = "npred_plot", height = "auto")
      )
    ) # end column 2
  ) # end row 1
)
