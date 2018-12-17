#Theater Ensemble

theater_crawler <- function(){

library(tidyverse)
library(rvest)
library(lubridate)
library(stringi)
library(chron)

url_theater <- "http://theater-ensemble.net/spielplan/"
nodes_theater <- ".meta , .title a , .time , span:nth-child(1)"

raw_crawler <- function(url, nodes) {
  read_html(url) %>%
    html_nodes(nodes) %>%
    html_text(trim = TRUE) 
} 

#Raw Data
raw_theater <- raw_crawler(url_theater, nodes_theater)

#Dataframe erstellen 
tidy_theater <- raw_theater %>%
  as.data.frame()
tidy_theater <- tidy_theater[-c(1),]
tidy_theater <- as.data.frame(split(tidy_theater, 1:4)) %>%
  select(date_start = X1, time_start = X2, title = X3, price = X4) %>%
  mutate(url = url_theater) %>%
  mutate(description = NA) %>%
  mutate(lng = "9.89444") %>%
  mutate(lat = "49.79372") %>% 
  mutate(city = "Wuerzburg") %>%
  mutate(street = "Frankfurter Str. 87") %>%  
  mutate(zip = "97082") %>% 
  mutate(date_end = date_start) %>%
  mutate(time_end = NA) %>%
  mutate(organizer = "Theater Ensemble") 

#Anpassung des Inhalts
tidy_theater$date_start <- gsub(",", "", as.character(tidy_theater$date_start), n) 
tidy_theater$date_start <- dmy(tidy_theater$date_start) 
tidy_theater$date_end <- gsub(",", "", as.character(tidy_theater$date_end), n) 
tidy_theater$date_end <- dmy(tidy_theater$date_end) 

tidy_theater$price <- gsub("[^0-9(]", "", tidy_theater$price)
tidy_theater$price <- stri_replace_last_coll(tidy_theater$price, "(", ", ermaessigt ")
tidy_theater$price <- stri_replace_first_coll(tidy_theater$price, "(", "")

tidy_theater$time_start <- substr(tidy_theater$time_start, 0, 5)

#Anpassung der Datenformate
tidy_theater$title <- as.character(tidy_theater$title)
tidy_theater$description <- as.character(tidy_theater$description)
tidy_theater$lng <- as.numeric(tidy_theater$lng)
tidy_theater$lat <- as.numeric(tidy_theater$lat)
tidy_theater$zip <- as.numeric(tidy_theater$zip)
tidy_theater$time_start <- times(paste0(tidy_theater$time_start, ":00"))
tidy_theater$time_end <- times(tidy_theater$time_end)

#Reihenfolge der Spalten
tidy_theater <- tidy_theater[,c(3,5,6,7,8,9,10,11,1,12,2,13,4,14)] 

return(tidy_theater)

}