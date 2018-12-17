library(RMariaDB)
library(dbConnect)
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(tidyr)
library(knitr)
library(chron)
library(lubridate)
library(plyr)
library(qdapRegex)
library(RSelenium)
library(rowr)
library(purrr)
library(gsubfn)
library(RJSONIO)

# problem: unifying, simplfying library

getCairo=function() {
  print("Cairo")
  jugendkulturhaus <- "https://cairo.wue.de/"
  
  jugendkulturhaus %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("time") %>%
    html_text() -> datum4
  
  raw_data %>%
    html_nodes("#block-views-block-veranstaltungen-frontpage-block-1 .titlefr") %>%
    html_text() -> title4
  
  lat <- c(49.78961)
  lng <- c(9.92466)
  
  link <- "https://cairo.wue.de/"
  link <- as.character(link)
  
  city <- ("Wuerzburg")
  city <- as.character(city)
  
  street <- ("Fred-Joseph-Platz 3")
  street <- as.character(street)
  
  zip <- c(97082)
  
  organizer <- ("Jugendkulturhaus Cairo")
  organizer <- as.character(organizer)
  
  veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, date_start = datum4, lat = lat, lng = lng, city = city, street = street, zip = zip)
  
  veranstaltungenjugendkulturhaus$date_start <- paste0(datum4, ".2018")
  gsub("[A-z]{1,10}","", veranstaltungenjugendkulturhaus$date_start) -> veranstaltungenjugendkulturhaus$date_start
  veranstaltungenjugendkulturhaus$date_start <- str_extract(veranstaltungenjugendkulturhaus$date_start,"[0-9]{1,2}/[0-9]{1,2}.[0-9]{1,4}")
  
  gsub("/", ".", veranstaltungenjugendkulturhaus$date_start, fixed = TRUE) -> date_start
  
  date_start1 <- as.Date(date_start, format = "%d.%m.%Y")
  
  veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, description = NA, lng = lng, lat = lat, city = city, street = street, zip = zip, date_start = date_start1, date_end = NA, time_start = NA, time_end = NA, price = NA, organizer = organizer)
  return(veranstaltungenjugendkulturhaus)
}

Cairo=getCairo()