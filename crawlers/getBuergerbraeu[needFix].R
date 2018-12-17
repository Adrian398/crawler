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

# problem: unifying, simplfying library, browser crawler (without browser possible)

getBuergerbraeu = function(){
  print("BürgerbrÃ¤u")
  buergerbraeu <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"
  
  buergerbraeu %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("time") %>%
    html_text() -> datum5
  dfdatum5 <- data.frame(datum5)
  
  raw_data %>%
    html_nodes(".first-headline span") %>%
    html_text() -> title5
  dftitle5 <- data.frame(title5)
  
  raw_data %>%
    html_nodes(".teaser-text p") %>%
    html_text() -> description
  
  lat <- 49.7937172
  lon <- 9.894353
  
  link <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"
  link <- as.character(link)
  
  city <- ("Wuerzburg")
  city <- as.character(city)
  
  street <- ("Frankfurter Strasse 87")
  street <- as.character(street)
  
  zip <- c(97082)
  
  organizer <- ("BürgerbrÃ¤u")
  organizer <- as.character(organizer)
  
  veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, date_start = datum5, date_end = NA, time_end = NA, description = description, lat = lat, lng = lon, city = city, street = street, zip = zip, organizer = organizer, price = NA)
  
  
  veranstaltungenbuergerbraeu$time_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}:[0-9]{1,2}")
  veranstaltungenbuergerbraeu$date_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}.[0-9]{1,2}.[0-9]{1,4}")
  
  date_start <- veranstaltungenbuergerbraeu$date_start
  time_start <- veranstaltungenbuergerbraeu$time_start
  time_start <- as.character(time_start)
  
  
  date_start <- as.Date(date_start, format = "%d.%m.%Y")
  
  time_start <- times(paste0(time_start, ":00"))
  
  
  veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, description = description, lng = lon, lat = lat, city = city, street = street, zip = zip, date_start = date_start, date_end = NA, time_start = time_start, time_end = NA, price = NA, organizer = organizer)
  
}

Buergerbraeu=getBuergerbraeu()