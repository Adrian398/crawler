library(rvest)
library(tidyverse)
library(lubridate)
library(chron)

url1 = "https://www.franziskanerkloster-wuerzburg.de/startseite-links/veranstaltungen"
url1 %>%
  read_html() -> rawData1


rawData1 %>%
  html_nodes(".itemtitle a") %>% 
  html_text() -> title
title
length(title)->length


rawData1 %>%
  html_nodes(".datetime , .date") %>% 
  html_text() -> e
e

stringr::str_extract_all(e, "\\d+[.]\\d{2}[.]\\d{4}.+") -> date
unlist(date) -> date
stringr::str_extract_all(date, "^\\d{2}[:punct:]\\d{2}[:punct:]\\d{4}[,]") -> test1
unlist(test1) -> v
v
gsub(",", "", v) -> datum3
date <- as.POSIXct(datum3,format="%d.%m.%Y")
date

stringr::str_extract_all(e, "\\d+[:]\\d{2}") -> time
unlist(time) -> time
time <- times(paste0(time, ":00"))
time

rawData1 %>%
  html_nodes(".itemtitle a") %>% 
  html_attr("href") -> link
gsub("//", "", link) -> link
link

c(rep(url1, length)) -> url
gsub("https://", "", url) -> url
url

c(rep("Franziskaner-Minoritenkloster-Würzburg", length)) -> veranstalter

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
  }
  Sys.sleep(0.3)
  return(location)
}
data <- lapply(veranstalter, addressToGeoLoc)
unlist(data) -> data
stringr::str_extract_all(data, "\\d+[.]\\d+") -> data
unlist(data) -> data
stringr::str_extract_all(data, "^\\d{1}[.]\\d+") -> long
stringr::str_extract_all(data, "\\d{2}[.]\\d+") -> lat
unlist(long) -> long
unlist(lat) -> lat
link
df <- data.frame(title = title, url = link, description = NA, lng = long, lat = lat, city = veranstalter, street = NA, zip = NA, date_start = date, date_end = date, time_start = time, time_end = NA, price = NA, organizer = veranstalter)
