library(tidyr)
library(dplyr)
#library(plotly)
library(rworldmap)
library(maps)
library(ggmap)
library(reshape2)
library(raster)
library(rgdal)
library(rgeos)
library(ggmosaic)
library(shiny)

#source scripts
source ('dataImport.R', local = T)
source ('attacksOverYears.R', local = T)
source ('casualtiesOverYears.R', local = T)
source ('highCasualtyAttacks.R', local = T)
source ('highCasualtyGroups.R', local = T)
source('mostAttackedCountries.R', local = T)
source('visualizeCountMap.R', local = T)

terr <- dataImport()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$singleVar <- renderPlot({
    #render plot of global attacks over years
    
    #filler for later input parameter
    n = 10
    
    #globalAttacksOverYears(terr)
    if (input$graphType == "Global Attacks Over Years")
      globalAttacksOverYears(terr)
    else
      if (input$graphType == "Global Casualties Over Years")
        casualtiesOverYears(terr)
    else
      if (input$graphType == "Attacks with Highest Casualties")
        highCasualtyAttacks(terr, n)
    else
      if (input$graphType == 'Groups Responsible for Highest Casualties')
        highCasualtyGroups(terr, n)
    else
      if (input$graphType == 'Most Attacked Countries')
        mostAttackedCountries(terr, n)
  })
  
  output$mapPlot <- renderPlot({
    if (input$mapParameter == 'Number of Attacks around the world')
      terr %>%
      group_by(nation) %>%
      summarise(Total = n()) %>%
      visualizeCountMap("Number of terrorist attacks around the world")
  })
})
