#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(rgeos)
library(httr)
library(jsonlite)
library(dplyr)
library(ggmap)
library(sp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Tour de DC"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("location",
                        "Enter Address:",
                      value = "3230 7th st NE, washington dc"),
            tableOutput("nearest_loc")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           DT::dataTableOutput(outputId = "distTable")
        )
    )
)
get_closest <- function(location){
    library(rgeos) #load packages
    location <- geocode(location, output = c("latlon"), source = "google")
    set1sp <- SpatialPoints(location) #define location points
    station_sub <- station_BigData[, 6:5]#subset for lon lat
    station_subSP <- SpatialPoints(station_sub)
    blep <- gDistance(set1sp, station_subSP, byid = TRUE)
    n <- station_BigData %>% #cbind back into larger data frame
        cbind(blep)
    dock <- n %>%
        arrange(`1`) %>%
        head(1L) %>%
        select(name, lat, lon)
    return(dock)
}
get_Capital <- function(url) {
    doot <- fromJSON(url)
    doot2 <- doot$data
    doot3 <- do.call(what = "rbind",
                     args = lapply(doot2, as.data.frame))
    return(doot3)
}
station_DataDF <- get_Capital("https://gbfs.capitalbikeshare.com/gbfs/en/station_information.json")
station_statusDF <- get_Capital("https://gbfs.capitalbikeshare.com/gbfs/en/station_status.json")
station_BigData <- station_DataDF %>%
    left_join(station_statusDF, by = "station_id")
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        plot_mapbox(mode = "scattermapbox", hoverlabel = list(
            bgcolor = "green", bordercolor = "white"
        )) %>%
            add_markers(data = station_BigData, x = ~ lon, y = ~ lat, fill = "blue",
                        hoverinfo = "text",
                        text = ~paste('Name: ', name,
                                      '\n Available Bikes: ', num_bikes_available,
                                      '\n Available Docks: ', num_docks_available)) %>%
            layout(mapbox = list(zoom = 14,
                                 center = list(lat = ~(get_closest(input$location)$lat),
                                               lon = ~(get_closest(input$location)$lon)),
                                 style = "streets")) %>%
            add_text(data = get_closest(input$location), x = ~ lon, y = ~ lat+.0006, text = "Our Suggestion!")
    })
    output$distTable <- DT::renderDataTable({
        DT::datatable(data = station_BigData %>% select(name, num_bikes_available, num_docks_available))
    })
    output$nearest_loc <- renderTable({get_closest(input$location)})
}
# Run the application 
shinyApp(ui = ui, server = server)
