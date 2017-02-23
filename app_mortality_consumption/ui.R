#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Death-Consumption Relationship: 2010-2014"),
  
  # Sidebar with controls to select year
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Select the year:"),
      uiOutput("YearSelector")
  
    ),
    
    
    #Main Panel contains the plot/s
    mainPanel(
      plotlyOutput("RegPlot")
      # verbatimTextOutput("ff")
    )
  )    
))     

