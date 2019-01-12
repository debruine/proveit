### fixef_tab ----
fixef_tab <- tabItem(
  tabName = "fixef_tab",
  actionButton("calc_fixed", "Calculate Fixed Effects"),
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
      ),
      box(
        title = "LMER",
        width = 12,
        tableOutput("lmer_coef")
      )
    )
  ) # end row 1
)
