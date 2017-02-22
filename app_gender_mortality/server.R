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
mortality <- read.csv("../output/states mortality.csv")

shinyServer(function(input, output){
  
  # read the year name
  YearName <- unique(mortality$year)

  # read the gender type
  GenderType <- unique(mortality$class)
  
  # year name list
  output$YearSelector <- renderUI({
    selectInput('year', label = 'Year',
                YearName,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2010) #default value
  })
  
  # gender type list
  output$GenderSelector <- renderUI({
    selectInput('class', label = 'Gender',
                GenderType,
                multiple = TRUE,
                selectize = TRUE,
                selected = "Overall") #default value
  })
  
  
  # get the selected year
  SelectedYear <- reactive({
    
    if (is.null(input$year) || length(input$year)==0)
      return()
    as.vector(input$year)
  })
  
  
  # get the selected gender
  SelectedGender <- reactive({
    
    if (is.null(input$class) || length(input$class)==0)
      return()
    as.vector(input$class)
  })
  
  #filter the data according to the selected states, disease and gender
  mortalityDF <- reactive({
    mortality %>%
      filter(year %in% SelectedYear()) %>%
      filter(class %in% SelectedGender()) %>%
      rename(percentage = ratio) %>%
      select(year, disease, states, class, percentage)
  })
  
  output$ff <- renderPrint({
    names(mortalityDF())
  })  
  
  
  
  ############PLOT#########
  output$RegPlot<-renderPlotly({
    #check if city and month are not null
    if (length(SelectedYear())>0 & length(SelectedGender())>0){
      p <- plot_ly(mortalityDF(),x=~disease, y=~percentage, color=~class, 
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Death Percentage from All States")
      p
    } 
  })    
})     
