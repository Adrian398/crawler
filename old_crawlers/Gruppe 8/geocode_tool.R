library(RJSONIO)
library(tidyverse)


addressToGeoLoc <- function(address)
{
  address = str_replace_all(address, " ", "")
  gurl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",address,"&key=AIzaSyAx0fpXEbPEYrKIQfv3so2TFU96-tUcaww")
  print(gurl)
  req <- fromJSON(gurl)
  
  if (req$status == "OK") {
    print("OK")
    location = req$results[[1]]$geometry$`location`
    lat = location[[1]]
    lon = location[[2]]
    #print(typeof(lat))
    #print(lon)
    print(location)
  } else {
    location <- NA
    print("Location not found")
    print(req$error_message)
  }
  Sys.sleep(0.3)
  return(location)
}


geoLocToAddress <- function(lat, lng)
{
  gurlc <- paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",lat, ",", lng,"&key=AIzaSyAx0fpXEbPEYrKIQfv3so2TFU96-tUcaww", sep="")
  print(gurlc)
  
  req <- fromJSON(gurlc, encoding = "UTF-8")
  #print(req)
  loctaion <- c(NA, NA, NA)
  if (req$status == "OK") {
    print("OK")
    adr = unlist(str_split(req$results[[1]]$formatted_address, ", "))
    
    street = adr[1]
    locsplit = unlist(str_split(adr[2], " "))
    zipcode <- locsplit[1]
    city <- locsplit[2]
    
    location <- c(street, zipcode, city)
    
  } else {
    location <- c(NA, NA, NA)
    print("Location not found")
    print(req$error_message)
  }
  Sys.sleep(0.3)
  return(location)
}

#geoLocToAddress(49.77505, 9.94391)



