#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mortality Rate (Prevalence): Gender Comparison (2010-2014)"),
  
  # Sidebar with controls to select diseases and gender
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Select one or more diseases:"),
      uiOutput("DiseaseSelector"),
      
      helpText("Select one or more genders:"),
      uiOutput("GenderSelector")
    ),
    
    
    #Main Panel contains the plot/s
    mainPanel(
      plotOutput("RegPlot")
      # verbatimTextOutput("ff")
    )
  )    
))     
      
