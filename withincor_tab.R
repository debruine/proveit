### withincor_tab ----
withincor_tab <- tabItem(
  tabName = "withincor_tab",
  p("Describe this stuff"),
  fluidRow( # start row 1
    column( # start column 1
      width = 4,
      box(
        title = "Parameters",
        width = NULL,
        sliderInput("withincor_n", "N:", min = 0, max = 1e4, value = 1000, step = 100),
        #sliderInput("withincor_es", "Effect Size:", min = -3, max = 3, value = 0, step = .1),
        sliderInput("withincor_m1", "Mean 1:", min = 50, max = 150, value = 100, step = 1),
        sliderInput("withincor_sd1", "SD 1:", min = 0, max = 20, value = 10, step = 1),
        sliderInput("withincor_m2", "Mean 2:", min = 50, max = 150, value = 110, step = 1),
        sliderInput("withincor_sd2", "SD 2:", min = 0, max = 20, value = 10, step = 1),
        sliderInput("withincor_r", "Correlation:", min = -1, max = 1, value = 0, step = 0.01),
        actionButton("withincor_resim", "Re-Simulate")
      )
    ), # end column 1
    column( # start column 2
      width = 8,
      box(
        title = "Plot 1",
        width = NULL,
        plotOutput(outputId = "withincor_plot1", height = "auto")
      ),
      box(
        title = "Plot 2",
        width = NULL,
        plotOutput(outputId = "withincor_plot2", height = "auto")
      ),
      box(
        title = "Plot 3",
        width = NULL,
        plotOutput(outputId = "withincor_plot3", height = "auto")
      )
    ) # end column 2
  ) # end row 1
)
