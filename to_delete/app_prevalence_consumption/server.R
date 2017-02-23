# Install and load packages
packages.used=c("shiny", "broom", "dplyr", "plotly", "tidyr")

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
library(broom)
library(plotly)
library(tidyr)



# load data
prevalence <- read.csv("../output/states prevalence.csv") %>%
  filter(class=="Overall") %>%
  rename(prevalence_percentage = ratio) %>%
  group_by(states) %>%
  mutate(avg_prev_percentage = mean(prevalence_percentage, na.rm=TRUE)) %>%
  distinct(year,avg_prev_percentage)
  

consumption <- read.csv("../output/smokers_proportion.csv") %>% 
  filter(YEAR != 2015, Gender=="Overall") %>%
  rename(year=YEAR, states=LocationDesc, smoker_percentage = Data_Value) 

prevalence_consumption <- left_join(prevalence, consumption) %>% 
  select(year, states, avg_prev_percentage, smoker_percentage)

shinyServer(function(input, output){
  
  # read the year name
  YearName <- unique(prevalence_consumption$year)
  
  # year name list
  output$YearSelector <- renderUI({
    selectInput('year', label = 'Year',
                YearName,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2012) #default value
  })
  
  # get the selected year
  SelectedYear <- reactive({
    
    if (is.null(input$year) || length(input$year)==0)
      return()
    as.vector(input$year)
  })
  
  
  #filter the data according to the selected year
  prev_conspDF <- reactive({
    prevalence_consumption %>%
      filter(year %in% SelectedYear()) %>%
      as.data.frame()
  })
  
  output$ff <- renderPrint({
    names(prev_conspDF())
  })  
  
  
  
  ############PLOT#########
  output$RegPlot<-renderPlotly({
    #check if city and month are not null
    if (length(SelectedYear())>0){
      
      p <- plot_ly((prev_conspDF()),x=~smoker_percentage, color=I("black")) %>%
        add_markers(y = ~avg_prev_percentage, text = prev_conspDF()$states, showlegend = FALSE) %>%
        add_lines(y = ~fitted(loess(avg_prev_percentage~smoker_percentage)),
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "Loess Smoother") %>%
        add_ribbons(data = augment(loess(avg_prev_percentage~smoker_percentage,data = prev_conspDF())),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error") %>%
        layout(xaxis = list(title = 'Smoker Proportion'),
               yaxis = list(title = 'Prevalence Percentage'),
               legend = list(x = 0.80, y = 0.90),
               title="Disease Prevalence-Consumption Relationship of All States")
      p
    } 
  })    
})     
