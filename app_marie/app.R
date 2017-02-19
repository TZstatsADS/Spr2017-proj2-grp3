## app.R ##
library(shiny)
library(shinydashboard)

library(maps)
library(mapproj)
source("./lib/helpers.R")
counties <- readRDS("./data/counties.rds")


header <- dashboardHeader(
  title = "Tobacco - Very Long Title",
  titleWidth = 400
)

body <- dashboardBody(
  
  fluidRow(
    box(
      title="Map", width = 8, height = 400, status="primary", solidHeader = TRUE,
      plotOutput("map2", height = 300)
    ),
    box(
     title = "Legend", width = 4, height = 400, status="warning", solidHeader = TRUE,
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
      title = "Title",
      height = 400,
      selected = "Tab1",
      width = 12,
      tabPanel("Tab1", "Tab content 1"),
      tabPanel("Tab2", "Tab content 2"),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
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


ui <- dashboardPage(
  header,
  dashboardSidebar(disable=TRUE),
  body
)


server <- function(input, output) { 
  
  output$year_value <- renderPrint({ input$year })
  output$value <- renderPrint({ input$select })
  output$map2 <- renderPlot({
    percent_map(counties$white, "darkgreen", "% White")
  })
  
}


shinyApp(ui, server)