# Install and load packages
packages.used=c("shiny", "ggplot2", "dplyr", "purrr", "tidyr")

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
library(purrr)
library(tidyr)



# load data
mortality_nation <- read.csv("../output/national_mortality.csv")

shinyServer(function(input, output){
  
  # read the disease name
  DiseaseName <- unique(mortality_nation$disease)
  
  # read the gender type
  GenderType <- unique(mortality_nation$class)
  
  # disease name list
  output$DiseaseSelector <- renderUI({
    selectInput('disease', label = 'Diseases',
                DiseaseName,
                multiple = TRUE,
                selectize = TRUE,
                selected = "Mortality from cerebrovascular disease (stroke)") #default value
  })
  
  # gender type list
  output$GenderSelector <- renderUI({
    selectInput('class', label = 'Gender',
                GenderType,
                multiple = TRUE,
                selectize = TRUE,
                selected = "Overall") #default value
  })
  
  
  # get the selected disease
  SelectedDisease <- reactive({
    
    if (is.null(input$disease) || length(input$disease)==0)
      return()
    as.vector(input$disease)
  })
  
  
  # get the selected gender
  SelectedGender <- reactive({
    
    if (is.null(input$class) || length(input$class)==0)
      return()
    as.vector(input$class)
  })
  
  #filter the data according to the selected states, disease and gender
  mortalityDF <- reactive({
    mortality_nation %>%
      filter(disease %in% SelectedDisease()) %>%
      filter(class %in% SelectedGender())
    })
  
  output$ff <- renderPrint({
    names(mortalityDF())
  })  
  
  
  
  ############PLOT#########
  output$RegPlot<-renderPlot({
    #check if city and month are not null
    if ((length(SelectedDisease())>0) & (length(SelectedGender())>0)){
      g <- ggplot(mortalityDF(),
                  aes(x=year, y=ratiototal, col=factor(class))) +
        labs(x="Year", y="Mortality rate") +
        facet_wrap(~disease, ncol = 2) + scale_color_discrete(name="Gender")
      g <- g + geom_point(size=3) + geom_line(size=1) 
      g
    } 
  })    
})     
  