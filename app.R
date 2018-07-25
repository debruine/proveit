## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

## Functions ----

source("vvvar_funcs.R")
source("peek_funcs.R")

## Interface Tab Items ----

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
      menuItem("Peek", tabName = "peek_tab"),
      menuItem("Va-va-variance!", tabName = "vvvar_tab"),
      menuItem("About", tabName = "about_tab")
    )
  ),
  dashboardBody(
    tabItems(
      peek_tab,
      vvvar_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) { 
  
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