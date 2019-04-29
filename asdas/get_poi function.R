get_poi <- function(location,radius,type,return_n) {
  #Google API call 
  key <- "AIzaSyA-YsYmgsuegnG9ZWonhssUq-aQdBr8MHo" #key for access
  base <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=" 
  POI <- GET(paste(base,location,"&radius=",radius,"&type=",type,"&key=",key,sep = ""))
  place <- NULL #null string to fill stuff with
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
