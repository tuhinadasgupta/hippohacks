#call relevant packages
library(shiny)
library(shinydashboard)

#define the ui
header <- dashboardHeader()
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("plot",
             tabName = "plot"
    )
  )
)
body <- dashboardBody(
  tabItems(tabname = "plot",
  renderPlotly(plot_mapbox(mode = "scattermapbox", hoverlabel = list(
    bgcolor = "green", bordercolor = "white"
  )) %>%
    add_markers(data = station_BigData, x = ~ lon, y = ~ lat, fill = "blue",
                hoverinfo = "text",
                text = ~paste('Name: ', name,
                              '\n Available Bikes: ', num_bikes_available,
                              '\n Available Docks: ', num_docks_available)) %>%
    layout(mapbox = list(zoom = 11,
                         center = list(lat = ~median(station_BigData$lat),
                                       lon = ~median(station_BigData$lon)),
                         style = "streets")))
  )
)
ui <- dashboardPage(header, sidebar, body)
# Define server logic 
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
