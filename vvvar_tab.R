### vvvar_tab ----
vvvar_tab <- tabItem(
  tabName = "vvvar_tab",
  p("Visualise data with different Ns, means, and SDs 
    using violin plots, boxplots, and the raw data."),
  fluidRow( # start row 1
    column( # start column 1
      width = 4,
      box(
        title = "Parameters",
        width = NULL,
        sliderInput("vvvar_n", "N:", min = 0, max = 1e3, value = 100, step = 10),
        sliderInput("vvvar_m", "Mean:", min = -10, max = 10, value = 0, step = .1),
        sliderInput("vvvar_sd", "SD:", min = 0, max = 10, value = 5, step = 0.1)
      ),
      box(
        title = "Visualisation",
        width = NULL,
        checkboxGroupInput("vvvar_view", "View:",
                           c("points" = "points",
                             "violin" = "violin",
                             "boxplot" = "boxplot",
                             "lines" = "lines"),
                           selected = c("points", "violin", "boxplot", "lines")
        ),
        p("Purple line = sample mean; grey lines = ± 0, 1, 2 & 3 SDs"),
        actionButton("vvvar_resim", "Re-Simulate")
      )
    ), # end column 1
    column( # start column 2
      width = 8,
      box(
        title = "",
        width = 12,
        plotOutput(outputId = "vvvar_plot", height = "auto")
      )
    ) # end column 2
  ) # end row 1
)