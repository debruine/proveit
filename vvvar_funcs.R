vvvar_render_plot <- function(input) {
  # input error handling
  n <- ifelse(is.numeric(input$vvvar_n), max(1, round(input$vvvar_n)), 1e3)
  m <- ifelse(is.numeric(input$vvvar_m), input$vvvar_m, 0)
  sd <- ifelse(is.numeric(input$vvvar_sd), input$vvvar_sd, 5)
  
  data <- data.frame(
    dn = 1:n,
    dv = rnorm(n, m, sd)
  )
  
  cols <- c("mean" = "purple", 
            "±1SD" = "blue", 
            "±2SD" = "darkgreen", 
            "±3SD" = "goldenrod")
  
  g <- ggplot(data)
  
  if ("lines" %in% input$vvvar_view) {
    g <- g + 
      geom_hline(color = "grey20", yintercept = m) +
      geom_hline(color = "grey40", yintercept = m - sd) +
      geom_hline(color = "grey40", yintercept = m + sd) +
      geom_hline(color = "grey60", yintercept = m - 2*sd) +
      geom_hline(color = "grey60", yintercept = m + 2*sd) +
      geom_hline(color = "grey80", yintercept = m - 3*sd) +
      geom_hline(color = "grey80", yintercept = m + 3*sd)
  }
  
  if ("points" %in% input$vvvar_view) {
    g <- g + geom_point(aes(dn, dv), alpha = 1/log(n+2))
  }
  
  if ("violin" %in% input$vvvar_view) {
    g <- g + 
      geom_violin(aes(n/2, dv), 
                  width = n/2, 
                  fill = "purple", 
                  alpha = 0.25) +
      geom_hline(color = "purple", 
                 yintercept = mean(data$dv), 
                 alpha = 0.5)
  }
  
  if ("boxplot" %in% input$vvvar_view) {
    g <- g + geom_boxplot(aes(dn, dv, group = "dv"), 
                          width = 0.25, alpha = 0.5)
  }
  
  g + scale_colour_manual(name="Lines",values=cols) + 
    theme(legend.position="bottom") +
    coord_cartesian(ylim = c(-40, 40)) +
    xlab("") +
    ylab("") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}
