library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgdal)
source("../lib/helpers.R")
counties <- readRDS("../data/counties.rds")
sta<-read.csv("../output/smokers_proportion.csv",as.is=T)


header <- dashboardHeader(
  title = "Tobacco - Very Long Title",
  titleWidth = 400
)

body <- dashboardBody(
  
  fluidRow(
    box(
      title="Map", width = 8, height = 600, status="primary", solidHeader = TRUE,
      leafletOutput("map2", height = 500)
    ),
    box(
      title = "Legend", width = 4, height = 600, status="warning", solidHeader = TRUE,
      sliderInput("year", label = h3("Year"),
                  min = 2010, max = 2014, value = 2012, 
                  step = 1, ticks = FALSE, sep=""),
      selectInput("Gender", label = h3("gender"), 
                  choices = list("Overall"= "Overall", "Male"="Male", "Female"= "Female"), 
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
  
  
  #draw the map
  sta<-read.csv("../output/smokers_proportion.csv")
  states <- readOGR("../doc/cb_2013_us_state_20m.shp",
                    layer = "cb_2013_us_state_20m", verbose = FALSE)
  
  
  output$year_value <- renderPrint({ input$year })
  output$value <- renderPrint({ input$select })
  output$map2 <- renderLeaflet({
    sta1<-sta[sta$YEAR==input$year& sta$LocationDesc!="Guam"& sta$Gender==input$Gender& sta$LocationDesc!="Virgin Islands",]
    var<-sta1$Data_Value
    max<-ceiling(max(var)/10)*10
    min<-floor(min(var)/10)*10
    var <- pmax(var, min)
    var <- pmin(var, max)
    inc <- (max - min) / 4
    legend.text <- c(paste0(min, " % or less"),
                     paste0(min + inc, " %"),
                     paste0(min + 2 * inc, " %"),
                     paste0(min + 3 * inc, " %"),
                     paste0(max, " % or more"))
    shades <- colorRampPalette(c("#fee6ce", "#ff5300"))(100)
    percents <- as.integer(cut(var, 100, 
                               include.lowest = TRUE, ordered = TRUE))
    fills <- shades[percents]
    s<-as.character(states$STUSPS)
    fills1<-rep(NA,length(fills))
    for (i in 1:length(s)){
      fills1[i]<-fills[which(as.character(sta1$LocationAbbr)==s[i])]
    }
    leaflet(states) %>% addTiles() %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
        color = fills1)%>% 
      addLegend(
        position = 'bottomright',
        colors = shades[c(1, 25, 50, 75, 100)],
        labels = legend.text, opacity = 1,
        title = 'Smokers Proportion'
      )%>%setView(lng=-97,lat=40,zoom=4)
  })
  
}


shinyApp(ui, server)