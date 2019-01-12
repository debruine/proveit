rnorm_multi <- function(n, vars = 3, cors = 0, mu = 0, sd = 1, 
                        varnames = NULL, empirical = FALSE) {
  # error handling
  if ( !is.numeric(n) || n %% 1 > 0 || n < 3 ) {
    stop("n must be an integer > 2")
  }
  
  if (!(empirical  %in% c(TRUE, FALSE))) {
    stop("empirical must be TRUE or FALSE")
  }
  
  if (length(mu) == 1) {
    mu <- rep(mu, vars)
  } else if (length(mu) != vars) {
    stop("the length of mu must be 1 or vars");
  }
  
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  }
  
  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (class(cors) == "matrix") { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else if (!matrixcalc::is.positive.definite(cors)) {
      stop("cors matrix not positive definite")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    # generate full matrix from vector of upper right triangle
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagonal
          cor_mat[row, col] = 1
        } else if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        } else {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
  }
  
  sigma <- (sd %*% t(sd)) * cor_mat
  bvn <- MASS::mvrnorm(n, mu, sigma, empirical = empirical)
  df <- data.frame(bvn)
  
  if (length(varnames) == vars) {
    names(df) <- varnames
  }
  
  df
}

simple_means_to_effects <- function(hard_congr = 0,
                                    hard_incon = 0,
                                    easy_congr = 0,
                                    easy_incon = 0) {
    # mean of all conditions
    grand_i <- (easy_congr + easy_incon + hard_congr + hard_incon)/4
    # mean difference between easy and hard conditions
    sub_cond_eff     <- (hard_congr + hard_incon)/2 -
                        (easy_congr + easy_incon)/2
                        
    # mean difference between incongruent and congruent versions
    stim_version_eff <- (hard_incon + easy_incon)/2 - 
                        (hard_congr + easy_congr)/2  
    # interaction between version and condition
    cond_version_ixn <- (hard_incon - hard_congr) -
                        (easy_incon - easy_congr)
                        
    
    list(
      "grand_i" = grand_i,
      "sub_cond_eff" = sub_cond_eff,
      "stim_version_eff" = stim_version_eff,
      "cond_version_ixn" = cond_version_ixn
    )
}

effects_to_simple_means <- function(grand_i = 400,
                                    sub_cond_eff = 0,
                                    stim_version_eff = 0,
                                    cond_version_ixn = 0,
                                    # coding
                                    hard = 0.5,
                                    easy = -0.5,
                                    congr = -0.5,
                                    incon = 0.5) {
  
  
  hard_congr <- grand_i + 
    hard * sub_cond_eff + 
    congr * stim_version_eff + 
    hard * congr * cond_version_ixn
  easy_congr <- grand_i + 
    easy * sub_cond_eff + 
    congr * stim_version_eff + 
    easy * congr * cond_version_ixn
  hard_incon <- grand_i + 
    hard * sub_cond_eff + 
    incon * stim_version_eff + 
    hard * incon * cond_version_ixn
  easy_incon <- grand_i + 
    easy * sub_cond_eff + 
    incon * stim_version_eff + 
    easy * incon * cond_version_ixn
  
  list(
    "hard_congr" = hard_congr,
    "easy_congr" = easy_congr,
    "hard_incon" = hard_incon,
    "easy_incon" = easy_incon
  )
}

sim_trials  <- function(sub_n = 100,
                      sub_sd = 50,
                      sub_version_sd = 50, 
                      sub_i_version_cor = -0.2,
                      stim_n = 50,
                      stim_sd = 50,
                      stim_version_sd = 50,
                      stim_cond_sd = 50,
                      stim_cond_version_sd = 50,
                      stim_i_cor = -0.2,
                      stim_s_cor = +0.2,
                      error_sd = 50) {
  sub <- rnorm_multi(
    n = sub_n, 
    vars = 2, 
    cors = sub_i_version_cor,
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(sub_sd, sub_version_sd),
    varnames = c("sub_i", "sub_version_slope")
  ) %>%
    mutate(
      sub_id = 1:sub_n,
      sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
    )
  
  stim_cors <- c(stim_i_cor, stim_i_cor, stim_i_cor,
                 stim_s_cor, stim_s_cor,
                 stim_s_cor)
  stim <- rnorm_multi(
    n = stim_n, 
    vars = 4, 
    cors = stim_cors, 
    mu = 0, # means of random intercepts and slopes are always 0
    sd = c(stim_sd, stim_version_sd, stim_cond_sd, stim_cond_version_sd),
    varnames = c("stim_i", "stim_version_slope", "stim_cond_slope", "stim_cond_version_slope")
  ) %>%
    mutate(
      stim_id = 1:stim_n
    )
  
  trials <- expand.grid(
    sub_id = sub$sub_id, # get subject IDs from the sub data table
    stim_id = stim$stim_id, # get stimulus IDs from the stim data table
    stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
  ) %>%
    left_join(sub, by = "sub_id") %>% # includes the intercept, slope, and conditin for each subject
    left_join(stim, by = "stim_id") %>%   # includes the intercept and slopes for each stimulus
    mutate(err = rnorm(nrow(.), 0, error_sd))
  
  return(trials)
}

dat_code <- function(trials,
                     grand_i = 400,
                     sub_cond_eff = 0,
                     stim_version_eff = 0,
                     cond_version_ixn = 0,
                     hard = 0.5,
                     easy = -0.5,
                     congr = -0.5,
                     incon = 0.5) {
  dat <- trials %>%
    mutate(
      # code subject condition and stimulus version
      sub_cond.code = recode(sub_cond, "hard" = hard, "easy" = easy),
      stim_version.code = recode(stim_version, "congruent" = congr, "incongruent" = incon),
      sub_cond.e = recode(sub_cond, "hard" = 0.5, "easy" = -0.5),
      stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = 0.5),
      # calculate trial-specific effects by adding overall effects and slopes
      version_eff = stim_version_eff + stim_version_slope + sub_version_slope,
      cond_eff = sub_cond_eff + stim_cond_slope,
      cond_version_eff = cond_version_ixn + stim_cond_version_slope,
      # calculate DV from intercepts, effects, and error
      dv = grand_i + sub_i + stim_i + err +
        (sub_cond.e * cond_eff) + 
        (stim_version.e * version_eff) + 
        (sub_cond.e * stim_version.e * cond_version_eff)
    )
  
  return(dat)
}

sim_lmer <- function(dat) {
  mod <- lmer(dv ~ sub_cond.code * stim_version.code +
                (1 + stim_version.code | sub_id) + 
                (1 + stim_version.code*sub_cond.code | stim_id),
              data = dat)
  
  return(mod)
}

sim_sub_anova <- function(dat) {
  dat_sub <- dat %>%
    group_by(sub_id, sub_cond, sub_cond.code, stim_version, stim_version.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ sub_cond.code + (stim_version.code| sub_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_sub)
  
  mod.sum <- anova(mod)
  
  return(mod.sum)
}

sim_stim_anova <- function(dat) {
  dat_stim <- dat %>%
    group_by(stim_id, sub_cond, sub_cond.code, stim_version, stim_version.code) %>%
    summarise(dv = mean(dv))
  
  mod <- afex::aov_4(dv ~ (sub_cond.code * stim_version.code | stim_id),
                     factorize = FALSE, check_contrasts = FALSE,
                     data = dat_stim)
  
  mod.sum <- anova(mod)
  
  return(mod.sum)
}

sim_power <- function(rep = 0,
                      sub_n = 100,
                      sub_sd = 50,
                      sub_version_sd = 50, 
                      sub_i_version_cor = -0.2,
                      stim_n = 50,
                      stim_sd = 50,
                      stim_version_sd = 50,
                      stim_cond_sd = 50,
                      stim_cond_version_sd = 50,
                      stim_i_cor = -0.2,
                      stim_s_cor = +0.2,
                      grand_i = 400,
                      hard_congr = 0,
                      hard_incon = 0,
                      easy_congr = 0,
                      easy_incon = 0,
                      sub_cond_eff = NULL,
                      stim_version_eff = NULL,
                      cond_version_ixn = NULL,
                      error_sd = 50) {
  
  dat <- sim_dat(
    sub_n = sub_n,
    sub_sd = sub_sd,
    sub_version_sd = sub_version_sd, 
    sub_i_version_cor = sub_i_version_cor,
    stim_n = stim_n,
    stim_sd = stim_sd,
    stim_version_sd = stim_version_sd,
    stim_cond_sd = stim_cond_sd,
    stim_cond_version_sd = stim_cond_version_sd,
    stim_i_cor = stim_i_cor,
    stim_s_cor = stim_s_cor,
    grand_i = grand_i,
    hard_congr = hard_congr,
    hard_incon = hard_incon,
    easy_congr = easy_congr,
    easy_incon = easy_incon,
    sub_cond_eff = sub_cond_eff,
    stim_version_eff = stim_version_eff,
    cond_version_ixn = cond_version_ixn,
    error_sd = error_sd
  )
  
  mod.lmer <- sim_lmer(dat)
  mod.sub <- sim_sub_anova(dat)
  mod.stim <- sim_stim_anova(dat)
  
  
  table.lmer <- mod.lmer$coefficients %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = Estimate, p = 6) %>%
    mutate(analysis = "lmer")
  
  table.sub <- mod.sub %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = ges, p = 7) %>%
    mutate(analysis = "anova_sub")
  
  table.stim <- mod.stim %>%
    as_tibble(rownames = "effect") %>%
    select(effect, es = ges, p = 7) %>%
    mutate(analysis = "anova_stim")
  
  bind_rows(table.lmer, table.sub, table.stim) %>%
    mutate(rep = rep)
}

plot_dat <- function(dat, grand_i = 0, view = c("violin", "boxplot")) {
  plot <- ggplot(dat, aes(sub_cond, dv, color = stim_version)) +
    geom_hline(yintercept = grand_i) +
    xlab("Subject Condition") +
    ylab("Reaction Time") +
    scale_color_discrete(name = "Stimulus Version")
  
  if ("violin" %in% view) {
    plot <- plot + geom_violin(alpha = 0.5)
  }
  
  if ("boxplot" %in% view) {
    plot <- plot + geom_boxplot(width = 0.2, position = position_dodge(width = 0.9))
  }
    
  return(plot)
}

plot_ranef_sub <- function(mod, dat) {
  sub <- dat %>%
    group_by(sub_id, sub_i, sub_version_slope) %>%
    summarise()
  
  ranef(mod)$sub_id %>%
    as_tibble(rownames = "sub_id") %>%
    rename(mod_i = `(Intercept)`,
           mod_version_slope = stim_version.code) %>%
    mutate(sub_id = as.integer(sub_id)) %>%
    left_join(sub, by = "sub_id") %>%
    select(mod_i, sub_i, mod_version_slope,  sub_version_slope) %>%
    GGally::ggpairs(lower = list(continuous = "smooth"),
                    progress = FALSE)
}

plot_ranef_stim <- function(mod, dat) {
  stim <- dat %>%
    group_by(stim_id, stim_i, stim_version_slope, stim_cond_slope, stim_cond_version_slope) %>%
    summarise()
  
  ranef(mod)$stim_id %>%
    as_tibble(rownames = "stim_id") %>%
    rename(mod_i = `(Intercept)`,
           mod_version_slope = stim_version.code,
           mod_cond_slope = sub_cond.code,
           mod_cond_version_slope = `stim_version.code:sub_cond.code`) %>%
    mutate(stim_id = as.integer(stim_id)) %>%
    left_join(stim, by = "stim_id") %>%
    select(mod_i, stim_i, 
           mod_version_slope, stim_version_slope, 
           mod_cond_slope, stim_cond_slope, 
           mod_cond_version_slope, stim_cond_version_slope) %>%
    GGally::ggpairs(lower = list(continuous = "smooth"),
                    progress = FALSE)
}