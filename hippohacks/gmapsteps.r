library(httr)
library(jsonlite)

#function the returns number of steps between 'start' and 'stop' for a particular 'mode' of transportation

gmapsteps <- function(mode, start, stop, api_key){
  
  preurl <- 'https://maps.googleapis.com/maps/api/directions/json?'
  
  mode <- paste('mode=', mode, sep = '')
  
  origin <- paste('origin=', gsub(' ','+', start, fixed = TRUE), sep = '')
  
  destination <- paste('destination=', gsub(' ','+', stop, fixed = TRUE), sep = '')
  
  key <- paste('key=', api_key, sep = '')
  
  dist_url <- paste(preurl, mode, '&', origin, '&', destination, '&', key, sep ='')
  
  response <- fromJSON(dist_url, flatten=TRUE)
  
  steps = dim(response[2][1]$routes[2]$legs[[1]]$steps[[1]])[1]
  
  return(steps)
}

  
#testing 
api_key = 'AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo'

start <- 'occidental restauraunt pensylvania ave'
 
stop <- 'white house'

#modes <- c('walking', 'bicycling')

mode <- 'walking'

ff = gmapsteps(mode, start, stop, api_key)

print(start)
print(stop)
print(ff)


