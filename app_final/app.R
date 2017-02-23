## app.R ##

#### Install Libraries ####
packages.used=c("shiny", "shinydashboard", "ggplot2", "dplyr",
                "purr", "tidyr", "plotly", "reshape2", "RColorBrewer",
                "rgdal")
# Check packages taht need to be installed
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# Install packages 
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,
                   repos='http://cran.us.r-project.org')
}

#### Loading Libraries & Functions ####
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(plotly)
library(reshape2)
library(RColorBrewer)
library(rgdal)

source("./lib/helper_functions_plots.R")
source("./lib/helper_functions_computations.R")

#### One time Computations ####
# First Row: Map
sta <- read.csv("./output/smokers_proportion.csv",as.is=T)
states <- readOGR("./doc/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", verbose = FALSE) # map
mor <- read.csv("./output/states mortality.csv")
pre <- read.csv("./output/states prevalence.csv")
df <- states_coordinates(states)

# Second Row: Advertising data
advertising <- read.csv("./output/advertising.csv", stringsAsFactors = FALSE, sep=",")
advertising <- advertising[!advertising$media=="Total",]
advertising <- advertising %>%
  group_by(year, media) %>%
  summarize(spendings = sum(spendings))

# Third Row: Mortality & Gender


# Fourth Row: Mortality & Gender


#### Header of the Dashboard ####
header <- dashboardHeader(
  title = "Tobacco - Health issue in the US",
  titleWidth = 400
)

#### Body of the Dashboard ####
body <- dashboardBody(
  
  ## First Row: Map
  fluidRow(
    tabBox(
      title = "",
      width = 8,
      height = 550,
      tabPanel(
        title = "Map",
        leafletOutput("map", height = 490)
      ),
      tabPanel(
        title = "Analysis"
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    ),
    box(
      title = "Legend", width = 4, height = 550, status="warning", solidHeader = TRUE,
      sliderInput("year", label = h4("Year"),
                  min = 2010, max = 2014, value = 2012, 
                  step = 1, ticks = FALSE, sep=""),
      selectInput("gender", label = h4("Gender"), 
                  choices = list("Overall"= "Overall", "Male"="Male", "Female"= "Female"), 
                  selected = "Overall"),
      radioButtons("cate", label = h4("Category"),
                   choices = list("Mortality Reason"="Mortality",
                                  "Disease"="Prevalence"),
                   inline = T),
      conditionalPanel("input.cate=='Mortality'",
                       selectInput("mortality",label = h4("Mortality Reason"),
                                   choices = list("Heart failure"="Heart failure",
                                                  "Chronic obstructive pulmonary disease"="Chronic obstructive pulmonary disease",
                                                  "Coronary heart disease"="Coronary heart disease",
                                                  "Cerebrovascular disease"="Cerebrovascular disease",
                                                  "Cardiovascular disease"="Cardiovascular disease"),
                                   selected = "Heart failure")),
      conditionalPanel("input.cate=='Prevalence'",
                       selectInput("prevalence",
                                   label = h4("Disease"),
                                   choices = list("Chronic obstructive pulmonary disease prevalence"="Chronic obstructive pulmonary disease prevalence",
                                                  "Arthritis"="Arthritis",
                                                  "Asthma"="Asthma"),
                                   selected="Asthma"))
    )
  ),
  
  ## Second Row: Commercial Spendings
  fluidRow(
    tabBox(
      title = "Commercial Spendings",
      height = 500,
      selected = "Total",
      width = 12,
      tabPanel("Total", 
               plotlyOutput("hist_advertising_total", 
                          height= 420)
               ),
      
      tabPanel("Per Media",
               box(
                 selectInput("media", label = h5("Media"), 
                             choices = list("Outdoor" = "Outdoor", 
                                            "Newspapers" = "Newspapers", 
                                            "Magazines" = "Magazines",
                                            "Transit" = "Transit",
                                            "Point-of-Sale" = "Point-of-Sale",
                                            "Promotional Allowances" = "Promotional Allowances",
                                            "Sampling Distribution" = "Sampling Distribution",
                                            "Direct Mail" = "Direct Mail",
                                            "Coupons & Retail-Value-Added" = "Coupons & Retail-Value-Added",
                                            "Internet" = "Internet",
                                            "Price Discounts" = "Price Discounts",
                                            "Specialty Item Distribution" = "Specialty Item Distribution",
                                            "Public Entertainment" ="Public Entertainment",
                                            "Sponsorships" = "Sponsorships",
                                            "Telephone" = "Telephone",
                                            "Other" = "Other"
                                            ),
                             
                             selected = 1),
                 width = 4
               ),
               box(
                 plotlyOutput("hist_advertising_media", 
                            height= 400),
                 width = 8
               )
               ),
      tabPanel("Analysis", 
               "Promotional Allowances spendings has decreased.
               Missing Values are due to privacy.
               "),
      tabPanel("Methodology and Sources", 
               "Promotional Allowances spendings has decreased.
               Missing Values are due to privacy.
               ")
    )
  ),
  
  ## Third Row: 
  fluidRow(
    tabBox(
      title = "Mortality Rate - Gender Comparison",
      height = 500,
      selected = "Histogram",
      width = 12,
      tabPanel(
        title = "Histogram",
        box(

          ),
        box(


          )
      ),
      tabPanel(
        title = "Analysis"
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    )
  ),
  
  ## Fourth Row:
  fluidRow(
    tabBox(
      title = "Mortality Rate - Gender Comparison",
      height = 500,
      selected = "Histogram",
      width = 12,
      tabPanel(
        title = "Histogram",
        box(
          
        ),
        box(

        )
      ),
      tabPanel(
        title = "Analysis"
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    )
  )
  
)


#### UI ####
ui <- dashboardPage(
  header,
  dashboardSidebar(disable=TRUE),
  body
)

#### Server ####
server <- function(input, output) { 
  
  #### FIRST ROW: MAP
  output$map <- renderLeaflet({
    map_leaflet(input$cate, input$mortality, input$year, input$gender, input$prevalence,
                df, mor, pre, sta, states )
  })
  
  #### SECOND ROW: COMMERCIAL SPENDINGS
  # Histogram Total Commercial Spendings
  output$hist_advertising_total <- renderPlotly({
    hist_advertising_total()
  })
  # Histogram Total Commercial Spendings per media
  output$hist_advertising_media <- renderPlotly({
    hist_advertising_media(input$media)
  })

  #### THIRD ROW: 

  #### FOURTH ROW:
  
}

#### Loading App ####
shinyApp(ui, server)

