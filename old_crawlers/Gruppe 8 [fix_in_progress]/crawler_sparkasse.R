#https://www.sparkasse-mainfranken.de/de/home.html
# https://www.sparkasse-mainfranken.de/de/home/ihre-sparkasse/termine-und-events.html?n=true&stref=hnav

#Test 

library (tidyverse)
library (rvest)
library(stringr)
library(dplyr)
library (ggmap)
library (RJSONIO)
library(chron)

"https://www.sparkasse-mainfranken.de/de/home/ihre-sparkasse/termine-und-events.html?n=true&stref=hnav" -> eventurl
eventurl%>%
  read_html() -> veranstaltungen_sparkasse

veranstaltungen_sparkasse %>%
  html_nodes (".left+ td") %>%
  html_text (trim=T) -> event

veranstaltungen_sparkasse %>%
  html_nodes (".table .left") %>%
  html_text (trim=T) -> datum

veranstaltungen_sparkasse %>%
  html_nodes (".left~ td+ td") %>%
  html_text (trim=T) -> ort



veranstaltungen_sparkasse_1 <- data.frame(title=event, Eventdatum=datum, Ort=ort, url = eventurl)

veranstaltungen_sparkasse_1 %>%
  mutate(`date_start` = paste(str_split(`datum`,"\\-", n=2, simplify=T)[,1], sep="\\-")) %>%
  mutate(`date_end` = paste(str_split(`datum`,"\\-", n=2, simplify=T)[,2], sep="\\-"))%>%
  .[-2] -> veranstaltungen_sparkasse_2

# Verarbeitung und Anpassung an Vorgaben
veranstaltungen_sparkasse_2 %>%
  mutate(organizer = if_else (str_detect(Ort, "Sparkasse|Geschäftsstellen") == TRUE, paste("Sparkasse Mainfranken"), as.character(NA))) %>%
  mutate(price = as.character(NA)) %>%
  mutate(description =as.character(NA)) %>%
  mutate(title= as.character(title))%>%
  mutate(url= as.character(url))%>%
  mutate(Ort= as.character(Ort))  %>%
  mutate(date_start= trimws(date_start)) %>%
  mutate(date_end= trimws(date_end)) %>%
  mutate(time_start= times(NA)) %>%
  mutate(time_end= times(NA)) %>%
  mutate(date_start=as.Date(date_start, "%d.%m.%Y"))%>%
  mutate(date_end=as.Date(date_end, "%d.%m.%Y")) %>%
  mutate(Ort = if_else (Ort == "Lohr", paste0("Lohr am Main", sep=" "), Ort)) -> veranstaltungen_sparkasse_3

#Lokation
default_lng <- 9.93496
default_lat <- 49.79417

register_google (key="AIzaSyBhVIcrHbDcFz-TssiCGSRBUXUXbxApY60")
veranstaltungen_sparkasse_4 <- mutate_geocode(veranstaltungen_sparkasse_3, Ort) %>% 
  rename(lng=lon) %>%
  mutate(city=as.character(NA)) %>%
  mutate(street=as.character(NA)) %>%
  mutate(zip= as.numeric (NA)) %>%
  .[-2] %>%
  mutate(lat = if_else(lng <=0, default_lat, lat)) %>%
  mutate(lng=if_else(lng <= 0, default_lng, lng)) 

veranstaltungen_sparkasse <- veranstaltungen_sparkasse_4[c(1,2,7, 10,11,12,13,14,3,4,8,9,6,5)]

