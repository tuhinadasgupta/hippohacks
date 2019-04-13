#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(ggmap)
library(shinyjs)
library(ggplot2)
#register google API key
register_google(key="AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo")

get_poi <- function(location,radius,type,return_n) {
  #Google API call 
  key <- "AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo"
  base <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
  POI <- GET(paste(base,location,"&radius=",radius,"&type=",type,"&key=",key,sep = ""))
  
  at <- rawToChar(POI$content)
  att <- fromJSON(at)
  length_of_list <- length(att[["results"]])
  if( length_of_list==0)
    list_of_names="None"
  else{
    list_of_names=list()
    for (i in c(1:min(length_of_list,return_n)))
    {
      place$name<-  att[["results"]][[i]][["name"]]
      list_of_names <- append(list_of_names,place$name)
    }
    return(list_of_names)
  }
}

get_poi_rating <- function(location,radius,type,return_n) {
  #Google API call 
  key <- "AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo"
  base <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
  POI <- GET(paste(base,location,"&radius=",radius,"&type=",type,"&key=",key,sep = ""))
  at <- rawToChar(POI$content)
  att <- fromJSON(at)
  length_of_list <- length(att[["results"]])
  if( length_of_list==0)
    list_of_ratings ="None"
  else{
    list_of_ratings =list()
    for (i in c(1:min(length_of_list,return_n)))
    {
      place$rating <-  att[["results"]][[i]][["rating"]]
      list_of_ratings <- append(list_of_ratings ,place$rating)
    }
    return(list_of_ratings)
  }
}


get_poi_loc <- function(location,radius,type,return_n) {
  #Google API call 
  key <- "AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo"
  base <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
  POI <- GET(paste(base,location,"&radius=",radius,"&type=",type,"&key=",key,sep = ""))
  
  at <- rawToChar(POI$content)
  att <- fromJSON(at)
  length_of_list <- length(att[["results"]])
  if( length_of_list==0)
  { list_of_lat="None"
  list_of_lng ="None"}
  else{
    list_of_lat=list()
    list_of_lng =list()
    for (i in c(1:min(length_of_list,return_n)))
    {
      place$lat<-  att[["results"]][[i]][["geometry"]][["location"]][["lat"]]
      place$lng <- att[["results"]][[i]][["geometry"]][["location"]][["lng"]]
      list_of_lat <- append(  list_of_lat,place$lat)
      list_of_lng <- append( list_of_lng,place$lng)
    }
    
    
    return(list(list_of_lat,list_of_lng))
  }
}


# Define UI for application that draws a histogram

ui <-  fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Hippo Hacks"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      # tags$iframe(id = "googleform",
      #                src = "https://docs.google.com/forms/d/e/1FAIpQLSf5hlOY_WlJICphLk_6EcPxeSQ_zKd0aUFgG8f-aBqUYQD_Jg/viewform?embedded=true",
      #                width = 1000,
      #                height = 1000,
      #                frameborder = 0,
      #                marginheight = 0)
      #    
      textInput("current", "Address", placeholder	 = "Enter start address", width = NULL,
      ),
      numericInput("radius", label = h3("Radius (in miles)"), value = 1),
      radioButtons("poi", label = h3("Places of interest"),
                   choices = list("Food" = 1, "Shopping" = 2, "Museums" = 3, "Leisure" = 4),
                   selected = NULL),
      
      
      uiOutput("choose")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Descriptives",tableOutput("rating")),
        tabPanel("Map")
      )
    )
  ),
  
  
  actionButton("button", "Go"),
  actionButton("button2", "Submit choice")
)




server <- function(input, output) {
  shinyjs::hide("button2")   
  observeEvent(input$button, {
    #printing out user input
    #defining type based on POI info from user
    if (input$poi == 1)
    {type =c("restaurant")
    n_type=1
    return_n=c(5)
    }
    else if (input$poi == 2)
    {type =c("shopping_mall","clothing_store")
    n_type=2
    return_n=c(2,3)
    }
    else if (input$poi == 3)
    {type= c("museum")
    n_type=1
    return_n=c(5)
    }
    else if (input$poi == 4)
    {type= c("zoo","amusement_park","aquarium","bowling_alley","movie_theater")
    n_type=5
    return_n=c(1,1,1,1,1)
    }
    location=geocode(input$current, output = c("latlon"), source = "google")
    lat=location$lat
    lon=location$lon
    geo_location=paste(lat,lon,sep=",")
    
    list_of_places=list()
    list_of_lat=list()
    list_of_lng=list()
    list_of_rating=list()
    
    for (i in 1:(n_type)){
      new_list_of_places <- get_poi(geo_location,input$radius*1609.34,type[i],return_n[i]) 
      new_list_of_loc <- get_poi_loc (geo_location,input$radius*1609.34,type[i],return_n[i]) 
      new_list_of_rating <- get_poi_rating (geo_location,input$radius*1609.34,type[i],return_n[i])
      
      list_of_lat <- append(list_of_lat,new_list_of_loc[1] )
      list_of_lng <- append(list_of_lng,new_list_of_loc[2] )
      
      list_of_places <- append(list_of_places,new_list_of_places )
      list_of_rating <- append(list_of_rating,new_list_of_rating )
      
    }
    rating =  as.data.frame(list_of_rating)
    colnames(rating) <-  list_of_places
    rownames(rating) <- "rating values"
    
    
    output$rating <- renderTable(rating)
    
    #list_of_poi[[2]]),(list_of_poi[[3]]),(list_of_poi[[4]]),(list_of_poi[[5]]))
    
    if (input$poi == 1)
    {header="Food"
    }
    else if (input$poi == 2)
    {
      header="Shopping"
    }
    else if (input$poi == 3)
    {header="Museums"
    }
    else if (input$poi == 4)
    {
      header="Leisure"
    }
    output$choose <- renderUI({
      radioButtons("chosen_place", label = h3(header),
                   choices = list_of_places
      )
    })
    shinyjs::hide("button")     
    shinyjs::show("button2") 
    
  })#end of observe for button
  
  observeEvent(input$button2, {  
    
    output$choice <- renderText(input$chosen_place)
    
    
  })
  
}  

# Run the application 
if (interactive()){shinyApp(ui = ui, server = server)}

