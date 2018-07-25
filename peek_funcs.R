peek <- function(start_n, max_n, by = 1, alpha = 0.05, dist = "normal") {
  # set up null effect for max n
  if (dist == "normal") {
    a <- rnorm(max_n) 
  }
  
  peeks <- seq(start_n, max_n, by)
  
  for (n in peeks) {
    t <- t.test(a[1:n], mu = 0)
    # return n for first sig p-value
    if (t$p.value < alpha) return(n) 
  }
  
  0 # if none sig by end
}

peek_render_plot <- function(input) {
  r <- replicate(input$peek_reps, 
                 peek(input$peek_n[1], 
                      input$peek_n[2], 
                      input$peek_by, 
                      input$peek_alpha))
  
  pcnt_sig <- round(100*sum(r > 0)/input$peek_reps, 1)
  
  html("intro", paste0("This simulated a normal distribution with with no effect and ran a 
      1-sample t-test on the first ",input$peek_n[1]," observations, then collected ",input$peek_by," new 
      observations and peeked again until ",input$peek_n[2]," observations. This procedure was run 
      ",input$peek_reps," times and reported the percent of runs that produced a significant result 
      (p < ",input$peek_alpha,")  and plotted the N at which the first significant result was found."))
  
  ggplot() +
    geom_histogram(aes(r, y=..density..), binwidth = 1,
                   color = "black", fill="red") +
    coord_cartesian(xlim = c(input$peek_n[1]-0.5, input$peek_n[2]+0.5), ylim = c(0, 0.1)) +
    theme_minimal() +
    labs(x = "Number of observations at first significant p-value",
         title = paste0(pcnt_sig, "% of ", input$peek_reps, " runs obtained a significant result"))
}