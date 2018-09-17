## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## Functions ----

source("npred_funcs.R")

## Interface Tab Items ----

source("npred_tab.R")

about_tab <- tabItem(
  tabName = "about_tab",
  h3("About This Apps"),
  p("Stuff about this")
)

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "N Predictors"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("N Predictors", tabName = "npred_tab")
    )
  ),
  dashboardBody(
    tabItems(
      npred_tab
    )
  )
)

## server ----
server <- function(input, output, session) {

  output$npred_plot <- renderPlot({
    resim <- input$npred_resim

    npred_plot(input$npred_n, input$npred_vars, input$npred_reps)
  }, height = function() {
    session$clientData$output_npred_plot_width/1.62
  })

} # end server()

shinyApp(ui, server)
