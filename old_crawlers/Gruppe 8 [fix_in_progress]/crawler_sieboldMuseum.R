#https://siebold-museum.byseum.de/de/home
# https://siebold-museum.byseum.de/de/--aktuelles-/aktuelles--veranstaltungen

library (tidyverse)
library (rvest)
library (stringr)
library(ggmap)
library (RJSONIO)
library(chron)

url <- "https://siebold-museum.byseum.de/de/--aktuelles-/aktuelles--veranstaltungen"

readLines(url) -> raw_text

# Events Crawlen und zur richtigen Teilung vorbereiten
raw_text %>%
  str_extract("h3(.*)?h3") %>% 
  paste(collapse=" ") -> try2

try2 %>%
  str_replace_all("</h3 NA h3>V", "</h3 NA NA h3>V") %>%
  str_replace_all("(</h3 NA h3>)", " ") -> try3
try3

# Events trennen 
str_split(try3, " NA ") -> try4

data <- data.frame(try4)
colnames(data)[1] <- "info"

subset(data, info != "NA") -> data
data %>% 
  mutate (info = str_remove_all(info, "NA|h3|<|>|/|br")) -> data

# extract Datum und Uhrzeit
data %>%
  mutate(date_start=paste(str_extract(info, "[0-9]{1,2}[\\./]{1}[0-9]{1,2}[\\./](20[1-2]{1}[0-9]{1})?"[[1]]))) %>%
  mutate(time_start=paste(str_extract(info, "[0-9]{2}[.][0-9]{2} |[0-9]{2}[:][0-9]{2}"))) %>%
  subset(grepl("2018", date_start)) %>%
  mutate(time_start = paste(str_replace_all(time_start, "NA", "00:00"))) %>%
  mutate(date_start= str_pad(date_start, width="10", side = "left", pad = 0)) %>%
  mutate(time_start = str_replace_all(time_start, "[.]", ":")) %>% 
  mutate(date_start = as.Date(date_start,"%d.%m.%Y")) %>%
  mutate(time_start= times(paste0(time_start, ":00")))  ->data2

as.Date (Sys.Date()) -> heute
subset(data2,data2$date_start >= heute) -> data3


# extract Enddatum und Uhrzeit 
default_lat = 49.79372
default_lng = 9.89444
  
data3 %>%
  mutate(organizer = paste("Siebold-Museum")) %>%
  mutate(url = paste(url)) %>%
  mutate(price = as.character(NA)) %>%
  mutate(info = trimws(info)) %>%
  mutate(description =as.character(paste (info))) %>%
  mutate(title= as.character(paste(info)))%>%
  mutate(Ort= as.character(paste(str_extract(title, "(?<=,)[^,]*$")))) %>%
  mutate(Ort = trimws(Ort)) %>% 
  mutate(lng = as.numeric(if_else(Ort=="Siebold-Museum", default_lng, as.numeric(NA)))) %>%
  mutate(lat = as.numeric(if_else(Ort=="Siebold-Museum", default_lat, as.numeric(NA)))) %>%
  mutate(date_end = as.Date(NA))%>%
  mutate(time_end = times(NA))%>%
  mutate(title = str_extract(title, ".+?(?=,)")) %>%
  mutate(city=as.character(NA)) %>%
  mutate(street=as.character(NA)) %>%
  mutate(zip= as.numeric (NA)) %>%
  .[-1] %>%
  .[-8]-> data4


#register_google (key="AIzaSyBhVIcrHbDcFz-TssiCGSRBUXUXbxApY60")
#locations <- distinct(data4, Ort) 
#locations_try <- mutate_geocode(locations, Ort)
# findet keine Ergebnisse, daher festgesetzt auf Ort des Hauses oder keine 


veranstaltungen_siebold <- data4[c(7,4,6,8,9,12,13,14,1,10,2,11,5,3)]

                       