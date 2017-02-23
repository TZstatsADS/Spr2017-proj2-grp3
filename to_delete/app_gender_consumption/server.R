# Install and load packages
packages.used=c("shiny", "ggplot2", "dplyr", "plotly", "tidyr")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,
                   repos='http://cran.us.r-project.org')
}
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)



# load data
smoker <- read.csv("../output/smokers_proportion.csv") %>% 
  filter(YEAR!=2015) %>%
  select(YEAR, LocationDesc, Data_Value, Gender) %>%
  rename(Year=YEAR, States=LocationDesc, Percentage=Data_Value)

shinyServer(function(input, output){
  
  # read the year name
  YearName <- c(2010,2011,2012,2013,2014)

  # read the gender type
  GenderType <- unique(smoker$Gender)
  
  # year name list
  output$YearSelector <- renderUI({
    selectInput('Year', label = 'Year',
                YearName,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2010) #default value
  })
  
  # gender type list
  output$GenderSelector <- renderUI({
    selectInput('Gender', label = 'Gender',
                GenderType,
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female","Male")) #default value
  })
  
  
  # get the selected year
  SelectedYear <- reactive({
    
    if (is.null(input$Year) || length(input$Year)==0)
      return()
    as.vector(input$Year)
  })
  
  
  # get the selected gender
  SelectedGender <- reactive({
    
    if (is.null(input$Gender) || length(input$Gender)==0)
      return()
    as.vector(input$Gender)
  })
  
  #filter the data according to the selected year and gender
  smokerDF <- reactive({
    smoker %>%
      filter(Year %in% SelectedYear()) %>%
      filter(Gender %in% SelectedGender()) 
  })
  
  output$ff <- renderPrint({
    names(smokerDF())
  })  
  
  
  
  ############PLOT#########
  output$RegPlot<-renderPlotly({
    #check if city and month are not null
    if (length(SelectedYear())>0 & length(SelectedGender())>0){
      p <- plot_ly(smokerDF(),y=~Percentage, color=~Gender, 
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Smoker proportion from All States")
      p
    } 
  })    
})     
