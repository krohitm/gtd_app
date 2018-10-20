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
                            'Attacks with Highest Casualties'),
                  selected = 'Global Attacks Over Years'),
      sliderInput()
    ),
    
    # Show a plot of the map
    mainPanel(
      plotOutput("singleVar")
    )
  )
))
