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
mortality <- read.csv("../output/states mortality.csv") %>%
  filter(class=="Overall") %>%
  rename(death_percentage = ratio) %>%
  group_by(states) %>%
  mutate(avg_death_percentage = mean(death_percentage, na.rm=TRUE)) %>%
  distinct(year,avg_death_percentage)
  

consumption <- read.csv("../output/smokers_proportion.csv") %>% 
  filter(YEAR != 2015, Gender=="Overall") %>%
  rename(year=YEAR, states=LocationDesc, smoker_percentage = Data_Value) 

mortality_consumption <- left_join(mortality, consumption) %>% select(year, states, avg_death_percentage, smoker_percentage)

shinyServer(function(input, output){
  
  # read the year name
  YearName <- unique(mortality_consumption$year)
  
  # year name list
  output$YearSelector <- renderUI({
    selectInput('year', label = 'Year',
                YearName,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2010) #default value
  })
  
  # get the selected year
  SelectedYear <- reactive({
    
    if (is.null(input$year) || length(input$year)==0)
      return()
    as.vector(input$year)
  })
  
  
  #filter the data according to the selected year
  mort_conspDF <- reactive({
    mortality_consumption %>%
      filter(year %in% SelectedYear()) %>%
      as.data.frame()
  })
  
  output$ff <- renderPrint({
    names(mort_conspDF())
  })  
  
  
  
  ############PLOT#########
  output$RegPlot<-renderPlotly({
    #check if city and month are not null
    if (length(SelectedYear())>0){
      
      p <- plot_ly((mort_conspDF()),x=~smoker_percentage, color=I("black")) %>%
        add_markers(y = ~avg_death_percentage, text = mort_conspDF()$states, showlegend = FALSE) %>%
        add_lines(y = ~fitted(loess(avg_death_percentage~smoker_percentage)),
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "Loess Smoother") %>%
        add_ribbons(data = augment(loess(avg_death_percentage~smoker_percentage,data = mort_conspDF())),
                    ymin = ~.fitted - 1.96 * .se.fit,
                    ymax = ~.fitted + 1.96 * .se.fit,
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error") %>%
        layout(xaxis = list(title = 'Smoker Proportion'),
               yaxis = list(title = 'Death Percentage'),
               legend = list(x = 0.80, y = 0.90),
               title="Death-Consumption Relationship of All States")
      p
    } 
  })    
})     
