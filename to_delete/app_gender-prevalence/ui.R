#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Prevalence Boxplot: 2012-2014"),
  
  # Sidebar with controls to select diseases and gender
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Select the year:"),
      uiOutput("YearSelector"),
      
      helpText("Select one or more gender:"),
      uiOutput("GenderSelector"),
      
      helpText("Note that : For year 2012, there is only data for overall gender. From year 2013 to 2014, data for female and male are available.")
    ),
    
    
    #Main Panel contains the plot/s
    mainPanel(
      plotlyOutput("RegPlot")
      # verbatimTextOutput("ff")
    )
  )    
))     

