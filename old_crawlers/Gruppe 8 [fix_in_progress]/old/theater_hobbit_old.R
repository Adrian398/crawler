library(rvest)
library(tidyverse)
library(RJSONIO)
library(chron)

url1 = "http://www.theater-hobbit.de/"
url1 %>%
  read_html() -> rawData

url2 = "http://www.theater-hobbit.de/Kontakt.html"
url2 %>%
  read_html() -> rawData2

url3 = "http://www.theater-hobbit.de/preise.html"
url3 %>%
  read_html() -> rawData3

rawData %>% 
  html_nodes(".stacks_right .stacks_out+ .stacks_out span") %>%
  html_text(trim = T) -> beschreibung
beschreibung

rawData %>%
  html_nodes("strong+ span , .stacks_right a") %>% 
  html_attr("href") -> link
gsub("//www.","www.", link) -> link
link

rawData2 %>% 
  html_nodes(".com_yourhead_stack_header_stack span") %>%
  html_text(trim = T) -> ort

ort
stringr::str_extract_all(ort, "\\d{5}.*") -> city
stringr::str_extract_all(city, "\\D+") -> city
gsub(" ", "", city) -> city
c(rep(city, length)) -> city

stringr::str_extract_all(ort, "\\d{5}") -> zip
unlist(zip) -> zip
c(rep(zip, length)) -> zip

stringr::str_extract_all(ort, "[\n].*[\n]") -> street
unlist(street) -> street
gsub("\n", "", street) -> street
c(rep(street, length)) -> street

gsub("\n", "", ort) -> ort


rawData %>%
  html_nodes("strong") %>% 
  html_text(trim = T) -> daten
stringr::str_extract_all(daten, "\\d{2}[:]\\d{2}") -> Uhrzeit
unlist(Uhrzeit) -> time
time <- times(paste0(time, ":00"))
time


stringr::str_extract_all(daten, "\\d{2}[.]\\d{2}[.]") -> Datum
unlist(Datum) -> date
date <- as.POSIXct(date,format="%d.%m.")
date

rawData %>%
  html_nodes("strong+ span , .stacks_right a") %>% 
  html_text(trim = T) -> title
title

rawData3 %>%
  html_nodes("strong:nth-child(3)") %>% 
  html_text(trim = T) -> preis
preis

rawData %>%
  html_nodes("#stacks_in_974_page3 , span+ strong") %>% 
  html_text(trim = T) -> test
test
test[1] -> test2
stringr::str_extract_all(test2, "\\d{2}[.]\\d{2}[.]") -> Datum
unlist(Datum) -> datetest
length(datetest) -> length
length

c(rep(title[1],length),title[2], title[3], title[4])-> alltitles
c(rep(link[1],length), link[2], link[3], link[4]) -> alllinks
c(rep(beschreibung[1], length), beschreibung[2], beschreibung[3], beschreibung[4]) -> allbeschreibungen

length(alltitles) -> length1


gsub("http://", "", url) -> url
c(rep("Plastisches Theater HOBBIT",length1)) -> veranstalter
c(rep(preis, length1)) -> preis

city <- c(rep(city[1], length1))
street <- c(rep(street[1], length1))
zip <- c(rep(zip[1], length1))


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
data <- lapply(ort, addressToGeoLoc)
unlist(data) -> data
stringr::str_extract_all(data, "\\d+[.]\\d+") -> data
unlist(data) -> data
stringr::str_extract_all(data, "^\\d{1}[.]\\d+") -> long
stringr::str_extract_all(data, "\\d{2}[.]\\d+") -> lat
unlist(long) -> long
unlist(lat) -> lat

long <- c(rep(long, length1))
lat <- c(rep(lat, length1))

df <- data.frame(title = alltitles, url = alllinks, description= allbeschreibungen, lng = long, lat = lat, city = city, street = street, zip = zip, date_start = date, date_end = date, time_start = time, time_end = NA, price = preis, organizer = veranstalter)



