#call relevant packages
library(shiny)
library(shinydashboard)

#define the ui
header <- dashboardHeader()
sidebar <- dashboardSidebar()
body <- dashboardBody()
ui <- dashboardPage(header, sidebar, body)
# Define server logic 
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
