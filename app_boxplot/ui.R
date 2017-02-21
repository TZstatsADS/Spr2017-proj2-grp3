#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mortality Rate Boxplot: 2010-2014"),
  
  # Sidebar with controls to select diseases and gender
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Select the year:"),
      uiOutput("YearSelector"),
      
      helpText("Select one or more gender:"),
      uiOutput("GenderSelector")
    ),
    
    
    #Main Panel contains the plot/s
    mainPanel(
      plotlyOutput("RegPlot")
      # verbatimTextOutput("ff")
    )
  )    
))     

