npred_sim <- function(sub_n = 100, var_n = 5) {
  if (var_n > 0) {
    vars <- paste0("V", rep(1:var_n, sub_n))
    vals <- rnorm(var_n*sub_n)
    # vals <- sample(0:1, var_n*sub_n, replace = T)
    eq <- paste("dv ~", paste(paste0("V", 1:var_n), collapse = "*"))
  } else {
    vars <- "1"
    vals = 1
    eq <- "dv ~ 1"
  }
  
  dat <- tibble(
    id = rep(1:sub_n, each = max(1, var_n)),
    var = vars,
    val = vals
  ) %>%
    spread(var, val) %>%
    mutate(dv = rnorm(sub_n))
  
  mod <- lm(eq, data = dat)
  
  summary(mod)$r.squared
}

npred_plot <- function(n, vars, reps) {
  expand.grid(rep = 1:reps,
              obs_n = n,
              var_n = vars) %>% 
    mutate(pv = map2_dbl(obs_n, var_n, npred_sim)) %>%
    ggplot() +
    geom_freqpoly(aes(pv), bins = 21) +
    geom_vline(aes(xintercept = mean(pv)), color = "red") +
    xlim(0, 1) +
    xlab("Proportion Variance Predicted")
}

npred_plot(25, 5, 100)
