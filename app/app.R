## app.R ##

#### Install Libraries ####
packages.used=c("shiny", "shinydashboard", "ggplot2", "dplyr",
                "purr", "tidyr", "plotly", "reshape2", "RColorBrewer",
                "rgdal", "broom", "htmltools", "leaflet", "Rcpp",
                "ggrepel")
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
library(Rcpp)
library(dplyr)
library(purrr)
library(tidyr)
library(plotly)
library(reshape2)
library(RColorBrewer)
library(rgdal)
library(broom)
library(htmltools)
library(leaflet)
library(ggrepel)

source("../lib/helper_functions_plots.R")
source("../lib/helper_functions_computations.R")

#### One time Computations ####
# First Row: Map
sta <- read.csv("../output/smokers_proportion.csv",as.is=T)
states <- readOGR("../data/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", verbose = FALSE) # map
mor <- read.csv("../output/states mortality.csv")
pre <- read.csv("../output/states prevalence.csv")
df <- states_coordinates(states)

# Second Row: States Comparison
mortality_avg <- read.csv("../output/states mortality.csv") %>%
  filter(class=="Overall") %>%
  rename(death_percentage = ratio) %>%
  group_by(states) %>%
  mutate(avg_death_percentage = mean(death_percentage, na.rm=TRUE)) %>%
  distinct(year,avg_death_percentage)
consumption <- read.csv("../output/smokers_proportion.csv") %>% 
  filter(YEAR != 2015, Gender=="Overall") %>%
  rename(year=YEAR, states=LocationDesc, smoker_percentage = Data_Value)
mortality_consumption <- left_join(mortality_avg, consumption) %>% select(year, states, avg_death_percentage, smoker_percentage)

prevalence_avg <- read.csv("../output/states prevalence.csv") %>%
  filter(class=="Overall") %>%
  rename(prevalence_percentage = ratio) %>%
  group_by(states) %>%
  mutate(avg_prev_percentage = mean(prevalence_percentage, na.rm=TRUE)) %>%
  distinct(year,avg_prev_percentage)
prevalence_consumption <- left_join(prevalence_avg, consumption) %>% 
  select(year, states, avg_prev_percentage, smoker_percentage)

# Third Row: Mortality & Gender
mortality <- read.csv("../output/states mortality.csv")
prevalence <- read.csv("../output/states prevalence.csv") %>%
  filter(year!=2012)
smoker <- read.csv("../output/smokers_proportion.csv") %>% 
  filter(YEAR!=2015) %>%
  select(YEAR, LocationDesc, Data_Value, Gender) %>%
  rename(Year=YEAR, States=LocationDesc, Percentage=Data_Value)

# Fourth Row: Advertising data
advertising <- read.csv("../output/advertising.csv", stringsAsFactors = FALSE, sep=",")
advertising <- advertising[!advertising$media=="Total",]
advertising[is.na(advertising)] <- 0
advertising <- advertising %>%
  group_by(year, media) %>%
  summarize(spendings = sum(spendings))
advertising_total <-
  advertising %>%
  group_by(year) %>%
  summarize(spendings=sum(spendings)) %>%
  subset(year>1975)
consumption_us <- read.csv("../data/consumption_tobacco_us.csv", sep=";")
consumption_us <- 
  consumption_us %>%
  subset(year>1975)
advertising_consumption <- cbind(advertising_total, consumption_us)
advertising_consumption[,1] <- NULL
advertising_consumption <- advertising_consumption[!is.na(advertising_consumption$percentage),]

# Fifth Row: Consumption
df1 <- read.csv("../data/Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
df2 <- read.csv("../data/Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")
consumption_states <- consumption_subset(df1, df2)


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
        title = "Tobacco Map",
        leafletOutput("map", height = 490)
      ),
      tabPanel(
        title = "Methodology and Sources",
        strong("Methodology"),
        div("We are aiming to establish a correlation between tobacco consumption and some diseases. The diseases we are displaying on this visualization have been proved to be related to tobacco consumption by the US Surgeon General Report of 2014."),
        strong("Sources"),
        div("Diplayed data come from three main sources:"),
        div("- Data.gov: The home of the U.S. Government’s open data"),
        div("- CDC: Centers for Disease Control and Prevention"),
        div("- Federal Trade Commission Report Cigarette Report for 2014"),
        strong("Definitions"),
        div("You can find the exact definitions of the different medias shown on the Commercial Spendings tab in the Federal Trade Commission Report Cigarette Report for 2014.")
      )
    ),
    box(
      title = "Legend", width = 4, height = 550, status="primary", solidHeader = TRUE,
      sliderInput("year", label = h3("Year"),
                  min = 2010, max = 2014, value = 2014, 
                  step = 1, ticks = FALSE, sep=""),
      selectInput("gender", label = h3("Gender"), 
                  choices = list("Overall"= "Overall", "Male"="Male", "Female"= "Female"), 
                  selected = 1),
      radioButtons("cate", label = h3("Category"),
                   choices = list("Mortality of Disease"="Mortality",
                                  "Prevalence of Disease"="Prevalence"),
                   inline = T),
      conditionalPanel("input.cate=='Mortality'",
                       selectInput("mortality",label = h3("Mortality of Disease"),
                                   choices = list("Heart failure"="Heart failure",
                                                  "Chronic obstructive pulmonary disease"="Chronic obstructive pulmonary disease",
                                                  "Coronary heart disease"="Coronary heart disease",
                                                  "Cerebrovascular disease"="Cerebrovascular disease",
                                                  "Cardiovascular disease"="Cardiovascular disease"),
                                   selected = 1)),
      conditionalPanel("input.cate=='Prevalence'",
                       selectInput("prevalence",label = h3("Disease"),
                                   choices = list("Chronic obstructive pulmonary disease prevalence"="Chronic obstructive pulmonary disease prevalence",
                                                  "Arthritis"="Arthritis",
                                                  "Asthma"="Asthma"),
                                   selected=1))
    )
  ),
  
  ## Second Row:
  fluidRow(
    tabBox(
      title = "States Comparison",
      height = 500,
      selected = "Mortality",
      width = 12,
      tabPanel(
        title = "Mortality",
        box(
          width = 9,
          plotlyOutput("RegPlot.m.s")
        ),
        box(
          width = 3,
          helpText("Select the year:"),
          uiOutput("YearSelector.m.s"),
          strong("Note:"),
          div("Each point represents one state of the US.")
        )
      ),
      tabPanel(
        title = "Prevalence",
        box(
          width = 9,
          plotlyOutput("RegPlot.p.s")
        ),
        box(
          width = 3,
          helpText("Select the year:"),
          uiOutput("YearSelector.p.s"),
          strong("Note:"),
          div("Each point represents one state of the US.")
        )
      )
    )
  ),
  
  ## Third Row: Gender Analysis
  fluidRow(
    tabBox(
      title = "Gender Comparison",
      height = 550,
      selected = "Consumption",
      width = 12,
      tabPanel(
        title = "Consumption",
        box(
          width = 9,
          plotlyOutput("RegPlot.c.g", height=450)
        ),
        box(
          width = 3,
          helpText("Select the year:"),
          uiOutput("YearSelector.c.g"),
          helpText("Select one or more gender:"),
          uiOutput("GenderSelector.c.g"),
          strong("Note:"),
          div("Each point represents one state of the US.")
        )
      ),
      tabPanel(
        title = "Mortality",
        box(
          width = 8,
          plotlyOutput("RegPlot.m.g", height=450)
        ),
        box( 
          width = 4,
          helpText("Select the year:"),
          uiOutput("YearSelector.m.g"),
          helpText("Select one or more gender:"),
          uiOutput("GenderSelector.m.g"),
          strong("Mortality Reasons:"),
          div("A: Cerebrovascular disease"),
          div("B: Chronic obstructive pulmonary disease"),
          div("C: Cardiovascular disease"),
          div("D: Coronary heart disease"),
          div("E: Heart failure"),
          strong("Note:"),
          div("Each point represents one state of the US.")
        )
      ),
      tabPanel(
        title = "Prevalence",
        box(
          width = 9,
          plotlyOutput("RegPlot.p.g", height=450)
        ),
        box( 
          width = 3,
          helpText("Select the year:"),
          uiOutput("YearSelector.p.g"),
          helpText("Select one or more gender:"),
          uiOutput("GenderSelector.p.g"),
          strong("Diseases"),
          div("B: Chronic obstructive pulmonary disease"),
          div("F: Asthma"),
          div("G: Arthrisis"),
          strong("Note:"),
          div("Each point represents one state of the US.")
        )
      )
    )
  ),
  
  ## Fourth Row: Commercial Spendings
  fluidRow(
    tabBox(
      title = "Commercial Spendings",
      height = 500,
      selected = "Consumption vs Spendings",
      width = 12,
      tabPanel("Consumption vs Spendings",
               plotlyOutput("scatter_adv_consumption",
                             height=450)
               ),
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
                 width = 3
               ),
               box(
                 plotlyOutput("hist_advertising_media", 
                            height= 400),
                 width = 9
               )
               )
    )
  ),
  
  fluidRow(
    tabBox(
      title = "Consumption Analysis",
      height = 650,
      selected = "Ages",
      width = 12,
      tabPanel("Ages",
        box(
          width =12,
          sliderInput("year.c.a", label = h5("Year"),
                      min = 1996, max = 2014, value = 2014, 
                      step = 1, ticks = FALSE, sep=""),
          div("Note: Each point represents one state of the US.")
        ),
        box(
          width=12,
          plotlyOutput("consumption_ages", 
                       height= 400)
        )
      ),
      tabPanel("Smoker Frequency",
        box(
          width=12,
          sliderInput("year.c.t", label = h5("Year"),
                      min = 1996, max = 2014, value = 2014, 
                      step = 1, ticks = FALSE, sep=""),
          div("Note: Each point represents one state of the US.")
        ),
        box(
          width=12,
          plotlyOutput("consumption_type", 
                       height= 400)
        )
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
  
  #### SECOND ROW: STATES COMPARISON
  ##Mortality
  # Read year name
  YearName.m.s <- unique(mortality_consumption$year)
  # Year name list
  output$YearSelector.m.s <- renderUI({
    selectInput('year.m.s', label = 'Year',
                YearName.m.s,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2014) #default value
  })
  # Get selected year
  SelectedYear.m.s <- reactive({
    if (is.null(input$year.m.s) || length(input$year.m.s)==0)
      return()
    as.vector(input$year.m.s)
  })
  # Filter the data according to the selected year
  mort_conspDF <- reactive({
    mortality_consumption %>%
      filter(year %in% SelectedYear.m.s()) %>%
      as.data.frame()
  })
  # Plot
  output$RegPlot.m.s <- renderPlotly({
    p <- ggplotly(plotly_empty())
    if (length(SelectedYear.m.s())>0){
      p <- plot_ly((mort_conspDF()),x=~smoker_percentage, color=I("black")) %>%
        add_markers(y = ~ round(avg_death_percentage, 3), text = mort_conspDF()$states, showlegend = FALSE) %>%
        add_lines(y = ~fitted(loess(avg_death_percentage~smoker_percentage)),
                  line = list(color = 'rgba(7, 164, 181, 1)'),
                  name = "Loess Smoother") %>%
        add_ribbons(data = augment(loess(avg_death_percentage~smoker_percentage,data = mort_conspDF())),
                    ymin = ~round(.fitted - 1.96 * .se.fit, 3),
                    ymax = ~round(.fitted + 1.96 * .se.fit, 3), 
                    line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)',
                    name = "Standard Error") %>%
        layout(xaxis = list(title = 'Proportion of Smokers (%)'),
               yaxis = list(title = 'Average Mortality Rate of Diseases linked to Tobacco'),
               legend = list(x = 0.80, y = 0.90),
               title="Relation between Death & Consumption for All States")
    } 
    p
  })
  ##Prevalence
  # Read the year name
  YearName.p.s <- unique(prevalence_consumption$year)
  # Year name list
  output$YearSelector.p.s <- renderUI({
    selectInput('year.p.s', label = 'Year',
                YearName.p.s,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2014) #default value
  })
  # Get selected year
  SelectedYear.p.s <- reactive({
    if (is.null(input$year.p.s) || length(input$year.p.s)==0)
      return()
    as.vector(input$year.p.s)
  })
  # Filter data according to the selected year
  prev_conspDF <- reactive({
    prevalence_consumption %>%
      filter(year %in% SelectedYear.p.s()) %>%
      as.data.frame()
  })
  # Plot
  output$RegPlot.p.s <- renderPlotly({
    p <- ggplotly(plotly_empty())
    if (length(SelectedYear.p.s())>0){
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
    } 
    p
  })    
  
  #### THIRD ROW: Gender Comparison
  ## Second Tab: Mortality
  # Read year name & gender type
  YearName.m.g <- unique(mortality$year)
  GenderType.m.g <- unique(mortality$class)
  # Year & Gender name list
  output$YearSelector.m.g <- renderUI({
    selectInput('year.m.g', label = 'Year',
                YearName.m.g,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2014) #default value
  })
  output$GenderSelector.m.g <- renderUI({
    selectInput('gender.m.g', label = 'Gender',
                GenderType.m.g,
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female","Male")) #default value
  })
  # Get selected year & selected gender
  SelectedYear.m.g <- reactive({
    if (is.null(input$year.m.g) || length(input$year.m.g)==0)
      return()
    as.vector(input$year.m.g)
  })
  SelectedGender.m.g <- reactive({
    if (is.null(input$gender.m.g) || length(input$gender.m.g)==0)
      return()
    as.vector(input$gender.m.g)
  })
  # Filter data according to the selected year and gender
  mortalityDF.m.g <- reactive({
    mortality %>%
      filter(year %in% SelectedYear.m.g()) %>%
      filter(class %in% SelectedGender.m.g()) %>%
      rename(percentage = ratio) %>%
      select(year, disease, states, class, percentage)
  })
  # Plot
  output$RegPlot.m.g<-renderPlotly({
    p <- ggplotly(plotly_empty())
    if (length(SelectedYear.m.g())>0 & length(SelectedGender.m.g())>0){
      p <- plot_ly(mortalityDF.m.g(),x=~disease, y=~percentage, color=~class,
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Mortality due to specific diseases linked to Tobacco",
               xaxis=list(title="Mortality Reason",
                          categoryorder="array",
                          categoryarray=list("Cerebrovascular disease",
                                             "Chronic obstructive pulmonary disease",
                                             "Cardiovascular disease",
                                             "Coronary heart disease",
                                             "Heart failure"),
                          tickmode="array",
                          tickvals=list("Cardiovascular disease",
                                     "Cerebrovascular disease",
                                     "Chronic obstructive pulmonary disease",
                                     "Coronary heart disease",
                                     "Heart failure"),
                          ticktext=list("C","A","B","D","E")),
               yaxis=list(title="Mortality rate (%)"))
    }
    p
  })
  ## Third Tab: Prevalence
  # Read year name & gender type
  YearName.p.g <- unique(prevalence$year)
  GenderType.p.g <- unique(prevalence$class)
  # Year & Gender name list
  output$YearSelector.p.g <- renderUI({
    selectInput('year.p.g', label = 'Year',
                YearName.p.g,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2013) #default value
  })
  output$GenderSelector.p.g <- renderUI({
    selectInput('gender.p.g', label = 'Gender',
                GenderType.p.g,
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female","Male")) #default value
  })
  # Get selected year and gender
  SelectedYear.p.g <- reactive({
    if (is.null(input$year.p.g) || length(input$year.p.g)==0)
      return()
    as.vector(input$year.p.g)
  })
  SelectedGender.p.g <- reactive({
    if (is.null(input$gender.p.g) || length(input$gender.p.g)==0)
      return()
    as.vector(input$gender.p.g)
  })
  # Filter the data according to the selected states, disease and gender
  prevalenceDF.p.g <- reactive({
    prevalence %>%
      filter(year %in% SelectedYear.p.g()) %>%
      filter(class %in% SelectedGender.p.g()) %>%
      rename(percentage = ratio) %>%
      select(year, disease, states, class, percentage)
  })
  # Plot
  output$RegPlot.p.g <- renderPlotly({
    p <- ggplotly(plotly_empty())
    if (length(SelectedGender.p.g())>0){
      p <- plot_ly(prevalenceDF.p.g(),x=~disease, y=~percentage, color=~class,
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Prevalence for Diseases linked to Tobacco",
               xaxis=list(title="Diseases",
                          tickmode="array",
                          tickvals=c("Chronic obstructive pulmonary disease prevalence",
                                     "Asthma",
                                     "Arthritis"),
                          ticktext=c("B","F","G")), 
               yaxis=list(title="Prevalence (%)"))
    } 
    p
  })
  ## Consumption
  # Read year name & Gender Type
  YearName.c.g <- c(2010,2011,2012,2013,2014)
  GenderType.c.g <- unique(smoker$Gender)
  # Year name list & Gender type list
  output$YearSelector.c.g <- renderUI({
    selectInput('year.c.g', label = 'Year',
                YearName.c.g,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2014) #default value
  })
  output$GenderSelector.c.g <- renderUI({
    selectInput('gender.c.g', label = 'Gender',
                GenderType.c.g,
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Female","Male")) #default value
  })
  # Get selected year & selected gender
  SelectedYear.c.g <- reactive({
    if (is.null(input$year.c.g) || length(input$year.c.g)==0)
      return()
    as.vector(input$year.c.g)
  })
  SelectedGender.c.g <- reactive({
    if (is.null(input$gender.c.g) || length(input$gender.c.g)==0)
      return()
    as.vector(input$gender.c.g)
  })
  # Filter data according to the selected year and gender
  smokerDF <- reactive({
    smoker %>%
      filter(Year %in% SelectedYear.c.g()) %>%
      filter(Gender %in% SelectedGender.c.g()) 
  })
  # Plot
  output$RegPlot.c.g <- renderPlotly({
    p <- ggplotly(plotly_empty())
    if (length(SelectedYear.c.g())>0 & length(SelectedGender.c.g())>0){
      p <- plot_ly(smokerDF(),y=~Percentage, color=~Gender, 
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Proportion of Smokers",
               yaxis=list(title="Proportion of Smokers (%)"))
    } 
    p
  }) 
  
  #### FOURTH ROW: COMMERCIAL SPENDINGS
  # Histogram Total Commercial Spendings
  output$hist_advertising_total <- renderPlotly({
    hist_advertising_total(advertising)
  })
  # Histogram Total Commercial Spendings per media
  output$hist_advertising_media <- renderPlotly({
    hist_advertising_media(input$media, advertising)
  })
  # Consumption vs Commercial Spendings
  output$scatter_adv_consumption <- renderPlotly({
    t <- list(
      family = "sans serif",
      size = 14,
      color = toRGB("grey50"))
    p <- plot_ly(advertising_consumption, x=~percentage, y=~spendings,
                 text=~year) %>%
      add_markers() %>%
      add_text(textfont=t, textposition = "top right") %>%
      layout(title="Spendings vs Proportion of Smokers between 1975 and 2014",
             xaxis=list(title="Smorker Proportion(%)"),
             yaxis=list(title="Spendings on Tobacco Commercials ($)"),
             showlegend=FALSE)
    p
    # g <- ggplot(advertising_consumption, aes(percentage, spendings/1000)) +
    #   geom_point() +
    #   geom_text_repel(aes(label=year)) +
    #   xlab("Smorker Proportion(%)") +
    #   ylab("Spendings on Tobacco Commercials (thousands $)")
    # ggplotly(g)
  })
  
  #### FIFTH ROW: CONSUMPTION
  # Plot Consumption for different ages
  output$consumption_ages <- renderPlotly({
    consumption_ages(consumption_states, input$year.c.a)
  })
  output$consumption_type <- renderPlotly({
    consumption_type(consumption_states, input$year.c.t)
  })
  
}

#### Loading App ####
shinyApp(ui, server)

