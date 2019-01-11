## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(afex)
library(faux)
library(GGally)
options("scipen"=10, "digits"=4)

## Functions ----

source("lmer_funcs.R")

## Interface Tab Items ----

source("plot_tab.R")
source("lmer_tab.R")
source("anova_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About This Apps"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "LMER vs ANOVA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parameters", tabName = "plot_tab"),
      menuItem("LMER", tabName = "lmer_tab"),
      menuItem("ANOVA", tabName = "anova_tab"),
      menuItem("About", tabName = "about_tab"),
      actionButton("resim", "Re-Simulate")
    )
  ),
  dashboardBody(
    tabItems(
      plot_tab,
      lmer_tab,
      anova_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) { 
  
  dat <- reactive({
    print("dat()")
    resim <- input$resim
    
    sim_dat(sub_n = input$sub_n,
            sub_sd = input$sub_sd,
            sub_version_sd = input$sub_version_sd, 
            sub_i_version_cor = input$sub_i_version_cor,
            stim_n = input$stim_n,
            stim_sd = input$stim_sd,
            stim_version_sd = input$stim_version_sd,
            stim_cond_sd = input$stim_cond_sd,
            stim_cond_version_sd = input$stim_cond_version_sd,
            stim_i_cor = input$stim_i_cor,
            stim_s_cor = input$stim_s_cor,
            grand_i = input$grand_i,
            #hard_congr = input$hard_congr,
            #hard_incon = input$hard_incon,
            #easy_congr = input$easy_congr,
            #easy_incon = input$easy_incon,
            sub_cond_eff = input$sub_cond_eff,
            stim_version_eff = input$stim_version_eff,
            cond_version_ixn = input$cond_version_ixn,
            error_sd = input$error_sd)
  })
  
  lmer_mod <- reactive({
    print("lmer_mod()")
    sim_lmer(dat())
  })
  
  lmer_text <- reactive({
    print("lmer_text()")
    summary(lmer_mod())
  })
  
  output$dat_plot <- renderPlot({
    plot_dat(dat(), input$grand_i)
  }, height = function() {
    session$clientData$output_dat_plot_width
  })
  
  output$ranef_sub_plot <- renderPlot({
    plot_ranef_sub(lmer_mod(), dat())
  }, height = function() {
    session$clientData$output_ranef_sub_plot_width
  })
  
  output$ranef_stim_plot <- renderPlot({
    plot_ranef_stim(lmer_mod(), dat())
  }, height = function() {
    session$clientData$output_ranef_stim_plot_width
  })
  
  output$lmer_varcor <- renderTable({
    lmer_text()$varcor %>% 
      as_tibble(rownames = "Effect") %>%
      select(-vcov) %>%
      mutate(type = ifelse(is.na(var2), "sd", "cor")) %>%
      spread(type, sdcor) %>%
      mutate(var1 = ifelse(is.na(var1), "", var1),
             var2 = ifelse(is.na(var2), "", var2)) %>%
      arrange(grp, var1, var2)
  })
  
  output$lmer_coef <- renderTable({
    lmer_text()$coefficients %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>|t|)`) %>%
      mutate_if(is.numeric, round, 3)
  })
  
  output$sub_coef <- renderTable({
    sim_sub_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`) %>%
      mutate_if(is.numeric, round, 3)
  })
  
  output$stim_coef <- renderTable({
    sim_stim_anova(dat()) %>% 
      as_tibble(rownames = "Effect") %>%
      rename(`p-value` = `Pr(>F)`) %>%
      mutate_if(is.numeric, round, 3)
  })
  
} # end server()

shinyApp(ui, server)