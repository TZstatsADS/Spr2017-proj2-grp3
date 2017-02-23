library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgdal)

## Load dataset
sta<-read.csv("./output/smokers_proportion.csv",as.is=T)
states <- readOGR("./doc/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", verbose = FALSE)
mor <- read.csv("./output/states mortality.csv")
pre <- read.csv("./output/states prevalence.csv")

## layout
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
      selectInput("gender", label = h3("Gender"), 
                  choices = list("Overall"= "Overall", "Male"="Male", "Female"= "Female"), 
                  selected = 1),
      radioButtons("cate", label = h3("Category"),
                   choices = list("Mortality of Disease"="Mortality",
                                  "Disease"="Prevalence"),
                   inline = T),
      selectInput("mortality",label = h3("Mortality of Disease"),
                   choices = list("Heart failure"="Heart failure",
                                                  "Chronic obstructive pulmonary disease"="Chronic obstructive pulmonary disease",
                                                  "Coronary heart disease"="Coronary heart disease",
                                                  "Cerebrovascular disease"="Cerebrovascular disease",
                                                  "Cardiovascular disease"="Cardiovascular disease"),
                                   selected = 1),
      selectInput("prevalence",label = h3("Disease"),
                  choices = list("Chronic obstructive pulmonary disease prevalence"="Chronic obstructive pulmonary disease prevalence",
                                                  "Arthritis"="Arthritis",
                                                  "Asthma"="Asthma"),
                                   selected=1)
  )),
  
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
  df[df$name=="LA",c(2,3)] <- c(-91,30) #change LA coordinates
  df[df$name=="MS",c(2,3)] <- c(-89.5,32.5) #change MS coordinates
  df[df$name=="AL",c(2,3)] <- c(-86.5,32.5) #change AL coordinates
  
  ##
  
  output$year_value <- renderPrint({ input$year })
  output$value <- renderPrint({ input$select })
  output$mortality <- renderPrint({ input$mortality })
  output$cate <- renderPrint({ input$cate })
  output$prevalence <- renderPrint({ input$pervalence })
  output$map2 <- renderLeaflet({
    
    #different if
    if (input$cate=="Mortality") {  
      ds<-mor
      point <- ds[ds$disease==input$mortality & ds$year==input$year & ds$class==input$gender,c("statesAbbr","ratio")]
      point$statesAbbr <- as.character(point$statesAbbr)
      for (i in 1:52) {
        if(sum(df$name[i]==point$statesAbbr)==1) {
          df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
          
        }
        else {
          df[i,"ratio"] <-0
        }
      }
      df$radius <- rank(df$ratio)*20/52+5
      df[df$ratio==0,]$radius <- 0
      legend_t<-paste0(round(quantile(df$ratio)[-1]*100,2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
      legend_size <- quantile(df$radius)[-1]
      color<-rep("black",4)
    } else { if(input$year==2010 | input$year==2011) {
      df$radius<-0
      legend_t<-NULL
      legend_size<-NULL
      color<- NULL
    } else { if(input$year==2012 & (input$gender=="Female" | input$gender=="Male")) {
      df$radius<-0
      legend_t<-NULL
      legend_size<-NULL
      color<- NULL
    }else{
      ds<-pre
      point <- ds[ds$disease==input$prevalence & ds$year==input$year & ds$class==input$gender,c("statesAbbr","ratio")]
      point$statesAbbr <- as.character(point$statesAbbr)
      for (i in 1:52) {
        if(sum(df$name[i]==point$statesAbbr)==1) {
          df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
          
        }
        else {
          df[i,"ratio"] <-0
        }
      }
      df$radius <- rank(df$ratio)*20/52+5
      if (sum(df$ratio==0)!=0) {
        df[df$ratio==0,]$radius <- 0}
      legend_t<-paste0(round(quantile(df$ratio)[-1],2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
      legend_size <- quantile(df$radius)[-1]
      color<-rep("black",4)}
    }
    }
    
    # shade of map
    sta1<-sta[sta$YEAR==input$year & sta$LocationDesc!="Guam"& sta$Gender==input$gender & sta$LocationDesc!="Virgin Islands",]
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
    order<-rep(NA,length(fills))
    for (i in 1:length(s)){
      order[i]<-which(as.character(sta1$LocationAbbr)==s[i])
    }
    fills1<-fills[order]
    dv<-paste(sta1$LocationDesc[order], "<br/>","Consumption:",as.character(sta1$Data_Value[order]),"%")
    
 if ((input$year == 2010 | input$year == 2011) & input$cate == "Disease")  { 
    clb <- data.frame("name"=sta1$LocationAbbr[order],"consumption"=sta1$Data_Value[order])
    clb$name <- as.character(clb$name)
    df$name <- as.character(df$name)
    for (i in 1:nrow(clb)) {
      if (sum(clb$name[i]==df$name)==1) {
        clb$ratio[i] <- df[which(df$name==clb$name[i]),"ratio"]
      } else {clb$ratio[i]<-NULL}
      
    }  
    clb$ratio<- round(clb$ratio*100,2)
 } else {clb$ratio = "NA"}
    

  
   
    
  leaflet(states) %>% addTiles() %>% 
    addPolygons(
      stroke = FALSE, fillOpacity = 0.6, smoothFactor = 0.5,
      color = fills1,
      popup=paste(dv,"<br/>","Ratio:",clb$ratio,"%"))%>%
    addCircleMarkers(lat=df$lat,lng=df$long,color="red",radius=df$radius,stroke=F,fillOpacity = 1) %>%
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
