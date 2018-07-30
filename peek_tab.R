### peek_tab ----
peek_tab <- tabItem(
  tabName = "peek_tab",
  p("Just how bad is it to peek at your data every few observations and stop 
      collecting data once you have a significant result? 
    Simulate the false positive rate below."),
  fluidRow( # start row 1
    column( # start column 1
      width = 4,
      box(
        title = "Parameters",
        width = NULL,
        sliderInput("peek_n", "Observations at first and last peek", 
                    min = 0, max = 200, step = 10, value = c(10, 50)),
        sliderInput("peek_by", "New observations before next peek", 
                    min = 0, max = 50, step = 1, value = 5),
        sliderInput("peek_alpha", "Significant p-value (alpha)", 
                    min = 0, max = .10, step = .001, value = .05),
        sliderInput("peek_reps", "Number of runs in this simulation", 
                    min = 1000, max = 10000, step = 1000, value = 1000),
        actionButton("peek_resim", "Re-Simulate")
      )
    ), # end column 1
    column( # start column 2
      width = 8,
      box(
        title = "",
        width = 12,
        plotOutput(outputId = "peek_plot", height = "auto"),
        p(id="intro", 
          "This simulated a normal distribution with no effect and ran a 
          1-sample t-test on the first 10 observations, then collected 5 new 
          observations and peeked again until 50 observations. This procedure was run 
          100 times and reported the percent of runs that produced a significant result 
          (p < .05) and plotted the N at which the first significant result was found.")
      )
    ) # end column 2
  ), # end row 1
  p("There are several papers and blogs discussing this issue:"),
  tags$ul(
    tags$li(a(href="https://t.co/51fDX07hQr", 
              "Performing High-Powered Studies Efficiently With Sequential Analyses; Daniël Lakens")),
    tags$li(a(href="https://neuroneurotic.net/2016/08/25/realistic-data-peeking-isnt-as-bad-as-you-thought-its-worse/",
              "Realistic data peeking isn’t as bad as you* thought – it’s worse; Sam Schwarzkopf"))
  )
)