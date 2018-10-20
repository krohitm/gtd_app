#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Global Terrorism Database Visualizations"),
  
  #Sidebar with a drop down list
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "graphType", label="Graph Type", 
                  choices=c('Global Attacks Over Years'),
                  selected = 'Global Attacks Over Years')
    ),
    
    # Show a plot of the map
    mainPanel(
      plotOutput("singleVar")
    )
  )
))
