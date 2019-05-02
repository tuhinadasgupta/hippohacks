#remapping the old functions with either vectorized or custom ones
library(googleway) #to be used for google places api
library(ggmap) #for the geocoding feature
library(dplyr)
get_poiVER2 <- function(location, radius, type, return_n) {
  key <- "AIzaSyDitfa2CtI_rpIbpJviZRey63D0m7N3ZMA" #set api key for google places
  doot <- geocode(location, output = c("latlon"), source = "google") #find location
  testSearch <- google_places(location = c(doot$lat, doot$lon), #commence search
                              keyword = type,
                              radius = radius*1609.344,
                              key = key)
  results <- cbind(testSearch$results$name, testSearch$results$rating, testSearch$results$geometry$location$lat, testSearch$results$geometry$location$lng) 
  results2 <- as.data.frame(results) #turn into dataframe
  colnames(results2) <- c("Name", "Rating", "Latitude", "Longitude") #clean names
  results3 <- results2 %>%
    head(return_n)
  return(results3)
}
bleep <- get_poiVER2("3230 7th street NE, Washington DC", 0.7, "Restaurant", 20) #example usage
bleep1 <- head(bleep, 5L) %>% select(Name)
bleep1$Name <- as.character(bleep1$Name)
bleep2 <- bleep1$Name
print(bleep2)
# Route from walking user entered point to first bikeshare
#production ver
walking <- route(from = input$current, to = get_closest(input$current), 
                      mode = "walking", structure = "route", output = "simple")
biking <- route(from = input$current, to = get_closest(input$current), 
                      mode = "bicycling", structure = "route", output = "simple")
#test ver
route1 <- route(from = "Lincoln Memorial", to = "3230 7th Street NE", 
      mode = "walking", structure = "route", output = "simple")
qmap(location = "Washington, DC", zoom = 13) +
  geom_path(data = route1, aes(x = lon, y = lat), 
            col = "red", size = 1.5, alpha = 0.7, lineend = "round")

doot <- geocode("3230 7th Street Ne", output = c("latlon"), source = "google") 
revgeocode(location = c(doot$lon, doot$lat))
