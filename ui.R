library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Global Terrorism Database Visualizations"),
  
  #Sidebar with a drop down list
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "graphType", label="Graph Type", 
                  choices=c('Global Attacks Over Years', 'Global Casualties Over Years',
                            'Attacks with Highest Casualties', 
                            'Groups Responsible for Highest Casualties',
                            'Most Attacked Countries'),
                  selected = 'Global Attacks Over Years')
    ),
    
    # Show a plot of the variable
    mainPanel(
      plotOutput("singleVar")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "mapParameter", label="Parameter", 
                  choices=c('Number of Attacks around the world'),
                  selected = 'Number of Attacks around the world')
    ),
    
    # Show a plot of the map
    mainPanel(
      plotOutput("mapPlot")
    )
  )
))
