## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(viridis)

## Functions ----

source("withincor/withincor_funcs.R")
source("vvvar_funcs.R")
source("peek_funcs.R")
source("npred/npred_funcs.R")

## Interface Tab Items ----

source("withincor/withincor_tab.R")
source("vvvar_tab.R")
source("peek_tab.R")
source("npred/npred_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About These Apps"),
  p("Thanks Phil McAleer for the app request and name for Va-va-variance!")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "ProveIt"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Within Cor", tabName = "withincor_tab"),
      menuItem("Peek", tabName = "peek_tab"),
      menuItem("Va-va-variance!", tabName = "vvvar_tab"),
      menuItem("N Predictors", tabName = "npred_tab"),
      menuItem("About", tabName = "about_tab")
    )
  ),
  dashboardBody(
    tabItems(
      withincor_tab,
      peek_tab,
      vvvar_tab,
      npred_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) { 
  
  withincor_plots <- reactive({
    resim <- input$withincor_resim
    withincor_render_plots(input$withincor_r,
                           input$withincor_m1,
                           input$withincor_m2,
                           input$withincor_sd1,
                           input$withincor_sd2,
                           input$withincor_n, 
                           session)
  })
  
  output$withincor_plot1 <- renderPlot({
    withincor_plots()[1]
  }, height = function() {
    session$clientData$output_withincor_plot1_width/2
  })
  
  output$withincor_plot2 <- renderPlot({
    withincor_plots()[2]
  }, height = function() {
    session$clientData$output_withincor_plot2_width/2
  })
  
  output$withincor_plot3 <- renderPlot({
    withincor_plots()[3]
  }, height = function() {
    session$clientData$output_withincor_plot3_width
  })
  
  vvvar_data <- reactive({
    resim <- input$vvvar_resim
    
    vvvar_get_data(input$vvvar_n, input$vvvar_m, input$vvvar_sd)
  })
  
  output$vvvar_plot <- renderPlot({
    vvvar_render_plot(vvvar_data(), 
                      input$vvvar_n, 
                      input$vvvar_m, 
                      input$vvvar_sd, 
                      input$vvvar_view)
  }, height = function() {
    session$clientData$output_vvvar_plot_width
  })
  
  output$peek_plot <- renderPlot({
    resim <- input$peek_resim
    peek_render_plot(input$peek_n,
                     input$peek_by,
                     input$peek_alpha,
                     input$peek_reps,
                     session)
  }, height = function() {
    session$clientData$output_peek_plot_width
  })
  
  output$npred_plot <- renderPlot({
    resim <- input$npred_resim
    
    # keep track of progress, this function is long
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making plot", value = 0)
    
    npred_plot(input$npred_n, input$npred_vars, input$npred_reps, progress)
  }, height = function() {
    session$clientData$output_npred_plot_width/1.62
  })
  
} # end server()

shinyApp(ui, server)
