### lmer_tab ----
lmer_tab <- tabItem(
  tabName = "lmer_tab",
  p(""),
  fluidRow( # start row 1
    column(
      width = 12,
      box(
        title = "Fixed Effects",
        width = 12,
        tableOutput("lmer_coef")
      ),
      box(
        title = "VarCor",
        width = 12,
        tableOutput("lmer_varcor")
      )
    )
  ), # end row 1
  fluidRow(
    column(
      width = 4,
      plotOutput(outputId = "ranef_sub_plot", height = "auto")
    ),
    column(
      width = 8,
      plotOutput(outputId = "ranef_stim_plot", height = "auto")
    )
  )
)
