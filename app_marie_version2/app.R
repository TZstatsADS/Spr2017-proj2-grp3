## app.R ##

#### Install Libraries ####
packages.used=c("shiny", "shinydashboard", "ggplot2", "dplyr",
                "purr", "tidyr", "plotly", "reshape2", "RColorBrewer")
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

source("./lib/helper_functions.R")

#Relative Data path
#Test - to remove
library(maps)
library(mapproj)
source("./lib/helpers.R")
counties <- readRDS("./data/counties.rds")


#### One time Computations ####
# Second Row: Advertising data
advertising <- read.csv("./output/advertising.csv", stringsAsFactors = FALSE, sep=",")
advertising <- advertising[!advertising$media=="Total",]
advertising <- advertising %>%
  group_by(year, media) %>%
  summarize(spendings = sum(spendings))

# Third Row: Mortality & Gender
mortality_nation <- read.csv("./output/national_mortality.csv")
mortality_nation <- mortality_nation %>%
  rename(percentage=ratiototal)

# Fourth Row: Mortality & Gender
mortality_states <- read.csv("./output/states disease prevalence and mortality.csv")
mortality <- filter(mortality_states, states != "United States",
                    disease != "Prevalence of chronic obstructive pulmonary disease among adults >= 18",
                    disease != "Current asthma prevalence among adults aged >= 18 years",
                    disease != "Prevalence of chronic obstructive pulmonary disease among adults >= 45 years")
mortality <- mortality %>% mutate(disease = gsub('Mortality (from|with) ','', disease))

#### Header of the Dashboard ####
header <- dashboardHeader(
  title = "Tobacco - Health issue in the US",
  titleWidth = 400
)

#### Body of the Dashboard ####
body <- dashboardBody(
  
  fluidRow(
    tabBox(
      title = "",
      width = 9,
      height = 400,
      tabPanel(
        title = "Map",
        plotOutput("map2", height = 300)
      ),
      tabPanel(
        title = "Analysis"
      ),
      tabPanel(
        title = "Methodology and Sources"
      )
    ),
    box(
     title = "Legend", width = 3, height = 400, status="primary", solidHeader = TRUE,
     sliderInput("year", label = h5("Year"),
                 min = 2010, max = 2014, value = 2012, 
                 step = 1, ticks = FALSE, sep=""),
     selectInput("select", label = h5("Disease or Mortality Reason"), 
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
  
  fluidRow(
    tabBox(
      title = "Mortality Rate - Gender Comparison",
      height = 500,
      selected = "Histogram",
      width = 12,
      tabPanel(
        title = "Histogram",
        box(
          helpText("Mortality Rate (Prevalence): Gender Comparison (2010-2014)"),
          width = 3,
          helpText("Select one or more diseases:"),
          uiOutput("DiseaseSelector"),
          helpText("Select one or more genders:"),
          uiOutput("GenderSelector")
          ),
        box(
          width = 9,
          plotlyOutput("RegPlot",
                       height = 400)
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
  
  fluidRow(
    tabBox(
      title = "Mortality Rate - Gender Comparison",
      height = 500,
      selected = "Histogram",
      width = 12,
      tabPanel(
        title = "Histogram",
        box(
          helpText("Mortality Rate Boxplot: 2010-2014"),
          width = 3,
          helpText("Select the year:"),
          uiOutput("YearSelector2"),
          helpText("Select one or more gender:"),
          uiOutput("GenderSelector2")
        ),
        box(
          width = 9,
          plotlyOutput("RegPlot2",
                       height = 400)
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
  
  #TEST
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
  # Read the disease name & Gender Name
  DiseaseName <- unique(mortality_nation$disease)
  GenderType <- unique(mortality_nation$class)
  
  # Disease name list
  output$DiseaseSelector <- renderUI({
    selectInput('disease', label = 'Disease',
                DiseaseName,
                multiple = TRUE,
                selectize = TRUE,
                selected = "stroke") #default value
  })
  # Gender type list
  output$GenderSelector <- renderUI({
    selectInput('class', label = 'Gender',
                GenderType,
                multiple = TRUE,
                selectize = TRUE,
                selected = "Overall") #default value
  })
  
  # Get the selected disease & Selected Gender
  SelectedDisease <- reactive({
    if (is.null(input$disease) || length(input$disease)==0)
      return()
    as.vector(input$disease)
  })
  SelectedGender <- reactive({
    if (is.null(input$class) || length(input$class)==0)
      return()
    as.vector(input$class)
  })
  
  # Filter data according to the selected disease and gender
  mortalityDF <- reactive({
    mortality_nation %>%
      filter(disease %in% SelectedDisease()) %>%
      filter(class %in% SelectedGender()) %>%
      select(year, disease, class, percentage)
  })
  
  output$ff <- renderPrint({
    names(mortalityDF())
  })

  output$RegPlot<-renderPlotly({
    if ((length(SelectedDisease())>0) & (length(SelectedGender())>0)){
      p <- ggplot(mortalityDF(), aes(x=year, y=percentage,color=factor(class)))+
        labs(x="Year", y="Mortality rate") +
        facet_wrap(~disease, ncol = 2) + scale_color_discrete(name="Gender")
      p <- p + geom_point(size=2) + geom_line(size=0.5) 
      p <- ggplotly(p)
    } 
  }) 
  
  #### FOURTH ROW:
  # Read year and gender type
  YearName <- unique(mortality$year)
  GenderType <- unique(mortality$class)
  
  # Year name list
  output$YearSelector2 <- renderUI({
    selectInput('year2', label = 'Year',
                YearName,
                multiple = FALSE,
                selectize = TRUE,
                selected = 2010) #default value
  })
  
  # Gender type list
  output$GenderSelector2 <- renderUI({
    selectInput('class2', label = 'Gender',
                GenderType,
                multiple = TRUE,
                selectize = TRUE,
                selected = "Overall") #default value
  })

  # Get selected year and selected gender
  SelectedYear2 <- reactive({
    if (is.null(input$year2) || length(input$year2)==0)
      return()
    as.vector(input$year2)
  })
  SelectedGender2 <- reactive({
    if (is.null(input$class2) || length(input$class2)==0)
      return()
    as.vector(input$class2)
  })
  
  # Filter the data according to the selected states, disease and gender
  mortalityDF2 <- reactive({
    mortality %>%
      filter(year %in% SelectedYear2()) %>%
      filter(class %in% SelectedGender2()) %>%
      rename(percentage = ratio) %>%
      select(year, disease, states, class, percentage)
  })

  # Plot
  output$RegPlot2<-renderPlotly({
    if (length(SelectedYear2())>0 & length(SelectedGender2())>0){
      p <- plot_ly(mortalityDF2(),x=~disease, y=~percentage, color=~class, 
                   type="box", boxpoints = "all", jitter = 0.3) %>%
        layout(title="Death Percentage from All States")
      p
    } 
  })    
  
  #### TEST
  output$map2 <- renderPlot({
    percent_map(counties$white, "darkgreen", "% White")
  })
}

#### Loading App ####
shinyApp(ui, server)

