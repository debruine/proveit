### plot_tab ----
plot_tab <- tabItem(
  tabName = "plot_tab",
  p("Set the parameters below for a a Stroop task where people (subjects) say the colour of colour words (stimuli) shown in each of two versions (congruent and incongruent). Subjects are in one of two conditions (hard and easy). The dependent variable (DV) is reaction time."),
  plotOutput(outputId = "dat_plot", height = "auto"),
  checkboxGroupInput("dat_plot_view", "View:",
                     c("violin" = "violin",
                       "boxplot" = "boxplot"),
                     selected = c("violin", "boxplot")
  )
)
