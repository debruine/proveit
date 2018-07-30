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

peek_render_plot <- function(n, by, alpha, reps, session) {
  if (by == 0) { 
    by = 1
    updateSliderInput(session, "peek_by", value = 1)
  }
  if (alpha == 0) { 
    alpha = .001
    updateSliderInput(session, "alpha", value = .001)
  }
  if (n[1] == 0) { 
    n[1] = 10
  }
  if (n[2] == 0) {
    n[2] = 10
  }
  updateSliderInput(session, "peek_n", value = n)
  
  
  r <- replicate(reps, 
                 peek(n[1], 
                      n[2], 
                      by, 
                      alpha))
  
  pcnt_sig <- round(100*sum(r > 0)/reps, 1)
  
  html("intro", paste0("This simulated a normal distribution with with no effect and ran a 
      1-sample t-test on the first ",n[1]," observations, then collected ",by," new 
      observations and peeked again until ",n[2]," observations. This procedure was run 
      ",reps," times and reported the percent of runs that produced a significant result 
      (p < ",alpha,")  and plotted the N at which the first significant result was found."))
  
  ggplot() +
    geom_histogram(aes(r, y=..density..), binwidth = 1,
                   color = "black", fill="red") +
    coord_cartesian(xlim = c(n[1]-0.5, n[2]+0.5), ylim = c(0, 0.1)) +
    theme_minimal() +
    labs(x = "Number of observations at first significant p-value",
         title = paste0(pcnt_sig, "% of ", reps, " runs obtained a significant result"))
}