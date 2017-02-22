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

source("./lib/helper_functions.R")

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
      height = 600,
      tabPanel(
        title = "Map",
        leafletOutput("map", height = 550)
      ),
      tabPanel(
        title = "Analysis"
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    ),
    box(
      title = "Legend", width = 4, height = 600, status="warning", solidHeader = TRUE,
      sliderInput("year", label = h4("Year"),
                  min = 2010, max = 2014, value = 2012, 
                  step = 1, ticks = FALSE, sep=""),
      selectInput("gender", label = h4("Gender"), 
                  choices = list("Overall"= "Overall", "Male"="Male", "Female"= "Female"), 
                  selected = 1),
      radioButtons("cate", label = h4("Category"),
                   choices = list("Mortality of Disease"="Mortality",
                                  "Disease"="Prevalence"),
                   inline = T),
      selectInput("prevalence",label = h4("Disease"),
                  choices = list("Chronic obstructive pulmonary disease prevalence"="Chronic obstructive pulmonary disease prevalence",
                                 "Arthritis"="Arthritis",
                                 "Asthma"="Asthma"),
                  selected=1),
      selectInput("mortality",label = h4("Mortality Reason"),
                  choices = list("Heart failure"="Heart failure",
                                 "Chronic obstructive pulmonary disease"="Chronic obstructive pulmonary disease",
                                 "Coronary heart disease"="Coronary heart disease",
                                 "Cerebrovascular disease"="Cerebrovascular disease",
                                 "Cardiovascular disease"="Cardiovascular disease"),
                  selected=1)
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
  # Get the inputs
  output$year_value <- renderPrint({ input$year })
  output$value <- renderPrint({ input$select })
  output$mortality <- renderPrint({ input$mortality })
  output$cate <- renderPrint({ input$cate })
  output$prevalence <- renderPrint({ input$pervalence })
  output$map <- renderLeaflet({
    if (input$cate=="Mortality") {  
      ds <- mor
      point <- ds[ds$disease==input$mortality & ds$year==input$year & ds$class==input$gender,c("statesAbbr","ratio")]
      point$statesAbbr <- as.character(point$statesAbbr)
      for (i in 1:52) {
        if(sum(df$name[i]==point$statesAbbr)==1) {
          df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
        } else {
            df[i,"ratio"] <-0
        }
      }
      df$radius <- rank(df$ratio)*20/52+5
      df[df$ratio==0,]$radius <- 0
      legend_t<-paste0(round(quantile(df$ratio)[-1]*100,2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
      legend_size <- quantile(df$radius)[-1]
      color<-rep("black",4)
    } else { 
        if(input$year==2010 | input$year==2011) {
          df$radius<-0
          legend_t<-NULL
          legend_size<-NULL
          color<- NULL
        } else { 
            if(input$year==2012 & (input$gender=="Female" | input$gender=="Male")) {
              df$radius <- 0
              legend_t <- NULL
              legend_size <- NULL
              color <- NULL
            } else {
                ds<-pre
                point <- ds[ds$disease==input$prevalence & ds$year==input$year & ds$class==input$gender,c("statesAbbr","ratio")]
                point$statesAbbr <- as.character(point$statesAbbr)
                for (i in 1:52) {
                  if(sum(df$name[i]==point$statesAbbr)==1) {
                    df[i,"ratio"] <- point[point$statesAbbr==df$name[i],"ratio"]
                  } else {
                      df[i,"ratio"] <-0
                    }
                }
                df$radius <- rank(df$ratio)*20/52+5
                if (sum(df$ratio==0)!=0) {
                  df[df$ratio==0,]$radius <- 0
                  }
                  legend_t <- paste0(round(quantile(df$ratio)[-1],2),"% -- ",c("25 Percentile","50 Percentile","75 Percentile","100 Percentile"))
                  legend_size <- quantile(df$radius)[-1]
                  color <- rep("black",4)
              }
            }
    }
    
    # Shade of map
    sta1 <- sta[sta$YEAR==input$year & sta$LocationDesc!="Guam"& sta$Gender==input$gender & sta$LocationDesc!="Virgin Islands",]
    var <- sta1$Data_Value
    max <- ceiling(max(var)/10)*10
    min <- floor(min(var)/10)*10
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
    s <- as.character(states$STUSPS)
    order <- rep(NA,length(fills))
    for (i in 1:length(s)){
      order[i] <- which(as.character(sta1$LocationAbbr)==s[i])
    }
    fills1 <- fills[order]
    dv <- paste(sta1$LocationDesc[order], "<br/>","Consumption:",as.character(sta1$Data_Value[order]),"%")
    clb <- data.frame("name"=sta1$LocationAbbr[order],"consumption"=sta1$Data_Value[order])
    clb$name <- as.character(clb$name)
    df$name <- as.character(df$name)
    for (i in 1:nrow(clb)) {
      if (sum(clb$name[i]==df$name)==1) {
        clb$ratio[i] <- df[which(df$name==clb$name[i]),"ratio"]
      } else {clb$ratio[i]<-NULL}
      
    }  
    clb$ratio<- round(clb$ratio*100,2)
    

    leaflet(states) %>% 
      addTiles() %>% 
      addPolygons(
        stroke = FALSE, fillOpacity = 0.6, smoothFactor = 0.5,
        color = fills1,
        popup=paste(dv,"<br/>","Ratio:",clb$ratio,"%")) %>%
      addCircleMarkers(lat=df$lat, lng=df$long, color="red", 
                       radius=df$radius,stroke=F,fillOpacity = 1) %>%
      addLegend(
        position = 'bottomright',
        colors = shades[c(1, 25, 50, 75, 100)],
        labels = legend.text, opacity = 1,
        title = 'Smokers Proportion'
      ) %>%
      setView(lng=-97,lat=40,zoom=4)
    
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

}

#### Loading App ####
shinyApp(ui, server)

