## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(viridis)

## Functions ----

source("within_correlation_shiny_app/withincor_funcs.R")
source("vvvar_funcs.R")
source("peek_funcs.R")

## Interface Tab Items ----

source("within_correlation_shiny_app/withincor_tab.R")
source("vvvar_tab.R")
source("peek_tab.R")

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
      menuItem("About", tabName = "about_tab")
    )
  ),
  dashboardBody(
    tabItems(
      withincor_tab,
      peek_tab,
      vvvar_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) { 
  
  withincor_plots <- reactive({
    withincor_render_plots(input$withincor_r,
                           input$withincor_m1,
                           input$withincor_m2,
                           input$withincor_sd1,
                           input$withincor_sd2,
                           input$withincor_n, 
                           session)
  })
  
  output$withincor_plot1 <- renderPlot({
    # change this to use reactive
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
  
  output$vvvar_plot <- renderPlot({
    resim <- input$vvvar_resim
    vvvar_render_plot(input)
  }, height = function() {
    session$clientData$output_vvvar_plot_width
  })
  
  output$peek_plot <- renderPlot({
    resim <- input$peek_resim
    peek_render_plot(input)
  }, height = function() {
    session$clientData$output_peek_plot_width
  })
  
} # end server()

shinyApp(ui, server)