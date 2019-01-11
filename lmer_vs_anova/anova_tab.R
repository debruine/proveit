### anova_tab ----
anova_tab <- tabItem(
  tabName = "anova_tab",
  p(""),
  fluidRow( # start row 1
    column(
      width = 12,
      box(
        title = "By-Subjects ANOVA",
        width = 12,
        tableOutput("sub_coef")
      ),
      box(
        title = "By-Stimuli ANOVA",
        width = 12,
        tableOutput("stim_coef")
      )
    )
  ) # end row 1
)
