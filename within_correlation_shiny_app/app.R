## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

## Functions ----

source("withincor_funcs.R")

## Interface Tab Items ----

source("withincor_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About This Apps"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Within"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Within Cor", tabName = "withincor_tab"),
      menuItem("About", tabName = "about_tab")
    )
  ),
  dashboardBody(
    tabItems(
      withincor_tab,
      about_tab
    )
  )
)

## server ----
server <- function(input, output, session) { 
  
  output$withincor_plot1 <- renderPlot({
    # stupid; change this
    plots <- withincor_render_plots(input)
    
    output$withincor_plot2 <- renderPlot({
      plots[2]
    }, height = function() {
      session$clientData$output_withincor_plot2_width/2
    })
    
    output$withincor_plot3 <- renderPlot({
      plots[3]
    }, height = function() {
      session$clientData$output_withincor_plot3_width
    })
    
    plots[1]
  }, height = function() {
    session$clientData$output_withincor_plot1_width/2
  })
  
} # end server()

shinyApp(ui, server)