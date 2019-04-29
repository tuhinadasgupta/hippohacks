library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggmap)
library(shinyjs)
library(ggplot2)
library(plotly)
library(sp)
library(rgeos)
# get list of places that are relevant to user selection
get_poi <- function(location,radius,type,return_n) {
  #Google API call 
  key <- "AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo"
  base <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
  POI <- GET(paste(base,location,"&radius=",radius,"&type=",type,"&key=",key,sep = ""))
  place <- NULL
  at <- rawToChar(POI$content)
  att <- fromJSON(at)
  length_of_list <- length(att[["results"]])
  if(length_of_list == 0)
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
# get relevent ratings for the POI
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
#get locations of POI
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