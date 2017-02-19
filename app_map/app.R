## app.R ##
library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "Tobacco - Very Long Title",
  titleWidth = 400
)

body <- dashboardBody(

  fluidRow(

    )

)


ui <- dashboardPage(
  header,
  dashboardSidebar(disable=TRUE),
  body
)


server <- function(input, output) {
  

}


shinyApp(ui, server)
