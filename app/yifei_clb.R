library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgdal)

## Load dataset
sta<-read.csv("./output/smokers_proportion.csv",as.is=T)
states <- readOGR("./doc/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", verbose = FALSE)
mor_dis <- read.csv("./output/states disease prevalence and mortality.csv")


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
      #selectInput("Disease",label = h3("disease"),
                  #choices = list(unique(mor_dis$disease)))
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
  
  #Obtain coordinates of different states
  
  coords_list <- list()
  for (i in 1:52) {
    coords_list[[i]]<- slot(states@polygons[[i]]@Polygons[[1]],"coords")
  }
  f <- function(x){apply(x,2,mean)}
  mean_coords_list<-lapply(coords_list,f)
  name<- as.character(states$STUSPS)
  long<- c()
  lat<-c()
  for (i in 1:52) {
    long[i]<- mean_coords_list[[i]][1]
    lat[i]<- mean_coords_list[[i]][2]
  }
  df<- data.frame(name,long,lat)
  df[df$name=="AK",c(2,3)] <- c(-150,66) #change AK coordinates
  df[df$name=="WA",c(2,3)] <- c(-120,47.2) #change WA coordinates
  df[df$name=="CA",c(2,3)] <- c(-118.4541,37) #change CA coordinates
  df[df$name=="WI",c(2,3)] <- c(-88.5,44) #change WI coordinates
  df[df$name=="FL",c(2,3)] <- c(-82,28) #change FL coordinates
  df[df$name=="OH",c(2,3)] <- c(-82.5,40.5) #change OH coordinates
  df[df$name=="NY",c(2,3)] <- c(-74.5,42.59) #change OH coordinates
  
  
  #draw the map
  
  
  output$year_value <- renderPrint({ input$year })
  output$value <- renderPrint({ input$select })
  output$map2 <- renderLeaflet({
    
    # shade of map
    sta1<-sta[sta$YEAR==input$year & sta$LocationDesc!="Guam"& sta$Gender==input$Gender & sta$LocationDesc!="Virgin Islands",]
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
    
    
    # points
    point <- mor_dis[mor_dis$disease=="Mortality from cerebrovascular disease (stroke)" & mor_dis$year==input$year & mor_dis$class=="Overall",c("statesAbbr","ratio")]
  point$statesAbbr <- as.character(point$statesAbbr)
  for (i in 1:52) {
    if(sum(df$name[i]==point$statesAbbr)==1) {
      df$ratio[i] <- point[point$statesAbbr==df$name[i],"ratio",]
    }
    else {
      df$ratio[i] <-0
    }
  }
  df$radius <- rank(df$ratio)*20/52+5
  
  
  
  leaflet(states) %>% addTiles() %>% 
    addCircleMarkers(lat=df$lat,lng=df$long,color="black",radius=df$radius,stroke=F,fillOpacity = 0.9) %>%
    addPolygons(
      stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
      color = fills1)%>%
    addLegend(
      position = 'bottomright',
      colors = shades[c(1, 25, 50, 75, 100)],
      labels = legend.text, opacity = 1,
      title = 'Smokers Proportion'
    ) %>%
    setView(lng=-97,lat=40,zoom=4)
  
  })
  
}


shinyApp(ui, server)
