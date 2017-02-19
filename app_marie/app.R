## app.R ##

#### Loading Libraries & Functions ####
library(shiny)
library(shinydashboard)
library(ggplot2)

source("./lib/helper_functions.R")

library(maps)
library(mapproj)
source("./lib/helpers.R")
counties <- readRDS("./data/counties.rds")

#### One time Computations ####
# Advertising data
advertising <- read.csv("./output/advertising.csv", stringsAsFactors = FALSE, sep=",")
advertising <- advertising[!advertising$media=="Total",]
advertising <- group_by()


#### Hearder of the Dashboard ####
header <- dashboardHeader(
  title = "Tobacco - Health issue in the US",
  titleWidth = 400
)

#### Body of the Dashboard ####
body <- dashboardBody(
  
  fluidRow(
    tabBox(
      title = "",
      width = 8,
      height = 400,
      tabPanel(
        title = "Map",
        plotOutput("map2", height = 300)
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    ),
    box(
     title = "Legend", width = 4, height = 400, status="primary", solidHeader = TRUE,
     sliderInput("year", label = h5("Year"),
                 min = 2010, max = 2014, value = 2012, 
                 step = 1, ticks = FALSE, sep=""),
     selectInput("select", label = h3("Select box"), 
                 choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                 selected = 1)
    )
  ),
  
  fluidRow(
    tabBox(
      title = "Commercial Spendings",
      height = 500,
      selected = "Total",
      width = 12,
      tabPanel("Total", 
               plotOutput("hist_advertising_total", 
                          height= 350)
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
                 plotOutput("hist_advertising_media", 
                            height= 350),
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
  
  fluidRow(
    box(title="TEST2",
        h3("Year Slider"),
        textOutput("year_value"),
        h3("List"),
        verbatimTextOutput("value")
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
  
  # Year Selection
  output$year_value <- renderPrint({ input$year })
  # Disease Selection
  output$value <- renderPrint({ input$select })
  # Histogram Total Commercial Spendings
  output$hist_advertising_total <- renderPlot({
    ggplot(advertising, aes(x=advertising$year, y=advertising$spendings/1000, fill=advertising$media)) +
    geom_bar(stat="identity") +
    xlab("Year") + 
    ylab("Spendings on advertising (in thousands)") +
      scale_fill_discrete(name = "Media")
  })
  # Histogram Total Commercial Spendings per media
  output$hist_advertising_media <- renderPlot({
    hist_advertising_media(input$media)
  })

  
  # Test
  output$map2 <- renderPlot({
    percent_map(counties$white, "darkgreen", "% White")
  })
  
}

#### Loading App ####
shinyApp(ui, server)