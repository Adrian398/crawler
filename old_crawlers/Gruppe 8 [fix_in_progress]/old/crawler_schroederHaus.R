#schroeder haus 
# http://www.schroeder-haus.de

## Funktion 

library (tidyverse)
library (rvest)
library(stringr)
library(dplyr)
library(ggmap)
library (RJSONIO)
library(chron)

getEvents <- function (url)
{
  
  url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes ("h3.tc-dynamic") %>%
    html_text (trim=T) -> event
  
  raw_data %>%
    html_nodes ("h4") %>%
    html_text (trim=T) -> event_zusatz
  
  raw_data %>% 
    html_nodes("#text") %>%
    html_text (trim=T) -> beschreibung
  
  raw_data %>% 
    html_nodes("b") %>%
    html_text (trim=T) %>%
    .[[5]] -> datum
  
  raw_data %>% 
    html_nodes(".ort") %>%
    html_text (trim=T) -> ort
  
  raw_data %>% 
    html_nodes("#infos") %>%
    html_text (trim=T) -> gebuhr
  
  url -> eventurl
  
  Events <- data.frame(title=event, Eventname_zusatz=event_zusatz, description=beschreibung, Datum=datum, Ort=ort, price=gebuhr, url=eventurl)
  
}

KalenderURL <- "https://www.schroeder-haus.de/programm/"

SchroederHausDetails <- KalenderURL %>%
  read_html() %>%
  html_nodes(".bg-dynamic") %>%
  html_attr("href")

paste0("https://www.schroeder-haus.de/", SchroederHausDetails) -> SchroederHausLinks

map_df(SchroederHausLinks, getEvents)-> raw_events

# Datum extraktion
raw_events %>% 
  mutate(`date_start` = paste(str_extract_all(Datum,"\\d{1,2}[.]\\d{1,2}[.](\\d{2})?", simplify=T)[,1])) %>%
  mutate(`date_end` = paste(str_extract_all(Datum,"\\d{1,2}[.]\\d{1,2}[.](\\d{2})?", simplify=T)[,2])) %>%
  mutate(`time_start` = paste(str_extract_all(Datum,"[^.]\\d{1,2}[.]\\d{1,2}[^.]", simplify=T)[,1])) %>% 
  mutate(`time_end` = paste(str_extract_all(Datum,"[^.]\\d{1,2}[.]\\d{1,2}[^.]", simplify=T)[,2]))->veranstaltungen_schroederHaus_1


Sys.Date() %>%
  format(format="%Y")->Jahr_heute

# Datum bereinigung
veranstaltungen_schroederHaus_1 %>%
  mutate(`Tag` = paste(str_extract_all(date_start,"\\d{1,2}[.]", simplify=T)[,1]))%>%
  mutate(`Monat` = paste(str_extract_all(date_start,"\\d{1,2}[.]", simplify=T)[,2])) %>%
  mutate(`Jahr` = paste(str_extract_all(date_start,"\\d{1,2}", simplify=T)[,3])) %>%
  mutate(Tag = str_pad(Tag, width="3", side = "left", pad = 0)) %>%
  mutate(Monat = str_pad(Monat, width="3", side = "left", pad = 0))%>%
  mutate(Jahr = paste(str_extract(Jahr, "\\d{2}"))) %>%
  mutate(Jahr =paste(str_replace(Jahr, "NA", Jahr_heute))) %>%
  mutate(Jahr = str_pad(Jahr, width="3", side = "left", pad = 0)) %>%
  mutate(Jahr = str_pad(Jahr, width="4", side = "left", pad = 2)) %>%
  mutate(`Tag1` = paste(str_extract_all(date_end,"\\d{1,2}[.]", simplify=T)[,1]))%>%
  mutate(`Monat1` = paste(str_extract_all(date_end,"\\d{1,2}[.]", simplify=T)[,2])) %>%
  mutate(`Jahr1` = paste(str_extract_all(date_end,"\\d{1,2}", simplify=T)[,3])) %>%
  mutate(Tag1 = str_pad(Tag1, width="3", side = "left", pad = 0)) %>%
  mutate(Monat1 = str_pad(Monat1, width="3", side = "left", pad = 0))%>%
  mutate(Jahr1 = paste(str_extract(Jahr1, "\\d{2}"))) %>%
  mutate(Jahr1 =paste(str_replace(Jahr1, "NA", Jahr_heute))) %>%
  mutate(Jahr1 = str_pad(Jahr1, width="3", side = "left", pad = 0)) %>%
  mutate(Jahr1 = str_pad(Jahr1, width="4", side = "left", pad = 2)) %>%
  mutate(time_start =paste(str_replace(time_start, "[.]", ":"))) %>%
  mutate(time_end =paste(str_replace(time_end, "[.]", ":"))) %>%
  mutate(time_start =paste(str_remove_all(time_start, " "))) %>%
  mutate(time_end =paste(str_remove_all(time_end, " ")))%>%
  mutate(time_start = str_pad(time_start, width="5", side = "left", pad = 0)) %>%
  mutate(time_start = times(paste0(time_start, ":00"))) %>%
  mutate(time_end = times(paste0(time_end, ":00"))) %>%
  mutate(date_start = as.Date(paste0(Tag, Monat, Jahr, sep=""), "%d.%m.%Y")) %>%
  mutate(date_end = as.Date(paste0 (Tag1, Monat1, Jahr1, sep=""), "%d.%m.%Y")) %>%
  .[-(12:17)]->veranstaltungen_schroederHaus_2


# Finale Bereinigung 
veranstaltungen_schroederHaus_2 %>%
  mutate(description= paste(Eventname_zusatz, Datum, description, if_else(str_detect(Ort, "Treffpunkt|Abfahrt") ==TRUE, paste(Ort), paste("")), sep="\n\n")) %>%
  mutate(description= trimws(description)) %>%
  .[-2] %>%
  .[-3] %>%
  mutate(Ort = trimws(Ort)) %>% 
  mutate(organizer = paste("R.-A.Schroeder-Haus"))-> veranstaltungen_schroederHaus_3

# Locations
default_lng <- 9.93501
default_lat <- 49.78972

register_google (key="AIzaSyBhVIcrHbDcFz-TssiCGSRBUXUXbxApY60")
locations <- distinct(veranstaltungen_schroederHaus_3, Ort) 
locations_try <- mutate_geocode(locations, Ort) %>%
  rename(lng =lon) %>% 
  mutate(lng = if_else(is.na(lng), default_lng, lng)) %>%
  mutate(lat = if_else(is.na(lat), default_lat, lat)) %>%
  mutate(lat = if_else(lng <=0, default_lat, lat)) %>%
  mutate(lng=if_else(lng <= 0, default_lng, lng)) 


# Join
veranstaltungen_schroederHaus_3 %>%
  left_join(locations_try) %>%
  mutate(city=as.character(NA)) %>%
  mutate(street=as.character(NA)) %>%
  mutate(zip= as.numeric (NA)) %>%
  .[-3]-> veranstaltungen_schroederHaus_4


# Ordnen
veranstaltungen_schroederHaus <- veranstaltungen_schroederHaus_4[c(1,4,2,10,11,12,13,14,5,6,7,8,3,9)]
  


# date_start ist der erste Termin der Eventreihe, date_end der letzte 
# (fast alle Events sind bezahlte Kurse, daher ist eine Teilnahme nach dem ersten Eventtag 
# unwahrscheinlich) 
