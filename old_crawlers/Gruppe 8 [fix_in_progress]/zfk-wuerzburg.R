library(rvest)
library(tidyverse)
library(chron)

url1 = "https://www.zfk-wuerzburg.de/tagesstaette/events/"
url1 %>%
  read_html() -> rawData1


rawData1 %>%
  html_nodes("h4") %>% 
  html_text() -> title

rawData1 %>%
  html_nodes("h4+ p") %>% 
  html_text() -> beschreibung

length(beschreibung) -> length

c(rep(url1, length)) -> url
gsub("https://", "", url) -> url

rawData1 %>%
  html_nodes("p:nth-child(9)") %>% 
  html_text() -> ort1
gsub("\n", " ", ort1) -> ort
c(rep(ort, length)) -> ort

c(rep("Zentrum für Körperbehinderte", length)) -> veranstalter

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

ort1
stringr::str_extract_all(ort1, "\\d{5}.*") -> city
stringr::str_extract_all(city, "\\D+") -> city
gsub(" ", "", city) -> city
c(rep(city, length)) -> city


stringr::str_extract_all(ort1, "\\d{5}") -> zip
unlist(zip) -> zip
c(rep(zip, length)) -> zip

stringr::str_extract_all(ort1, "[\n].*[\n]") -> street
unlist(street) -> street
gsub("\n", "", street) -> street
c(rep(street, length)) -> street

df <- data.frame(title = title, url = url, description = beschreibung, lng = long, lat = lat, city = city, street= street , zip= zip, date_start = NA, date_end = NA, time_start = NA, time_end = NA, price = NA, organizer = veranstalter)
df


