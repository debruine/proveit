### plot_tab ----
plot_tab <- tabItem(
  tabName = "plot_tab",
  p("Set the parameters below for a a Stroop task where people (subjects) say the colour of colour words (stimuli) shown in each of two versions (congruent and incongruent). Subjects are in one of two conditions (hard and easy). The dependent variable (DV) is reaction time."),
  fluidRow( # start row 1
    column(
      width = 4,
      box(
        title = "Sample Size",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        sliderInput("sub_n", "Subjects:", min = 10, max = 100, value = 20, step = 10),
        sliderInput("stim_n", "Items:", min = 5, max = 100, value = 10, step = 5)
      ),
      box(
        title = "Fixed Effects",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        sliderInput("grand_i", "Grand Intercept",
                    min = 400, max = 800, value= 400, step = 10),
        sliderInput("sub_cond_eff", "Condition", 
                    min = -50, max = 50, value = 0, step = 5),
        sliderInput("stim_version_eff", "Version", 
                    min = -50, max = 50, value = 0, step = 5),
        sliderInput("cond_version_ixn", "Condition:Version", 
                    min = -50, max = 50, value = 0, step = 5)
      ),
      # box(
      #   title = "Effects",
      #   solidHeader = TRUE,
      #   collapsible = TRUE,
      #   width = NULL,
      #   sliderInput("grand_i", "Grand Intercept",
      #               min = 200, max = 600, value= 400, step = 10),
      #   sliderInput("hard_congr", "Mean for Hard Congruent",
      #               min = -100, max = +100, value = 0, step = 5),
      #   sliderInput("hard_incon", "Mean for Hard Incongruent",
      #               min = -100, max = +100, value = 0, step = 5),
      #   sliderInput("easy_congr", "Mean for Easy Congruent",
      #               min = -100, max = +100, value = 0, step = 5),
      #   sliderInput("easy_incon", "Mean for Easy Incongruent",
      #               min = -100, max = +100, value = 0, step = 5)
      # ),
      box(
        title = "Random Intercept SDs",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        sliderInput("sub_sd", "Subject:", min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_sd", "Stimulus:", min = 0, max = 100, value = 50, step = 10),
        sliderInput("error_sd", "Residual:", min = 0, max = 100, value = 50, step = 10)
      ),
      box(
        title = "Random Slope SDs",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        sliderInput("sub_version_sd", "Subject (version):", 
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_version_sd", "Stimulus (version):",
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_cond_sd", "Stimulus (condition):",
                    min = 0, max = 100, value = 50, step = 10),
        sliderInput("stim_cond_version_sd", "Stimulus (condition:version):",
                    min = 0, max = 100, value = 50, step = 10)
      ),
      box(
        title = "Random Effect Correlations",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        sliderInput("sub_i_version_cor", "Subject Intercept and Slope (version):",
                    min = -1, max = 1, value = -0.2, step = 0.05),
        sliderInput("stim_i_cor", "Stimulus Intercept and Slopes:",
                    min = -1, max = 1, value = -0.2, step = 0.05),
        sliderInput("stim_s_cor", "Stimulus Slopes:",
                    min = -1, max = 1, value = 0.2, step = 0.05)
      )
    ),
    column(
      width = 8,
      box(
        title = "Plot",
        width = 12,
        plotOutput(outputId = "dat_plot", height = "auto")
      )
    )
  )
)
