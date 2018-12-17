library(plyr)
library(rvest)
library(tidyverse)
library(purrr)
library(gsubfn)
library(stringr)
library(chron)

url <- "https://www.museum-am-dom.de/veranstaltungen.htm"

# ziehe Tag und Uhrzeit
url %>%
  read_html() %>%
  html_nodes(".calDate") %>%
  html_text() -> date_time

# suche Datum
date <- unlist(strapply(date_time, "\\d{1,2}\\.\\d{1,2}\\."))

# Startdatum immer gleich Enddatum, also identisch
date_start <- sapply(date, function(x) paste0(x, "2018"))
date_end <- date_start

# ziehe Zeit und füge, falls es fehlt ":00" hinzu
time_start <- sapply(date_time, function(x) str_extract(x, '\\d{1,2}:\\d{1,2}(?=\\sUhr)|\\d{1,2}(?=\\sUhr)'))
time_start <- sapply(time_start, function(x) ifelse(grepl(":", x), x, paste0(x, ":00"))) 
time_end <- rep(NA, length(date_start))

date_start <- as.Date(date_start, format="%d.%m.%Y")
date_end <- as.Date(date_end, format="%d.%m.%Y")
time_start <- times(paste0(time_start,":00"))
 

# ziehe Namen 
url %>%
  read_html() %>%
  html_nodes(".calDescr a") %>%
  html_text() -> title

# ziehe Infos 
url %>%
  read_html() %>%
  html_nodes("p+ p") %>%
  html_text() -> info

# Preis kommt immer nach "Kosten:". Manchmal endet er erst mit dem nächsten Satz ".Bitte anmelden"
preis_info <- function(string){
  if(grepl(".Bitte anmelden", string)){
    price = str_extract(string, "(?<=Kosten:\\s).+(?=.Bitte)")
  }
  else {
    price = str_extract(string, "(?<=Kosten:\\s).+")
  }
  return(price)
}
price <- sapply(info, preis_info)

# Location ist immer identisch
organizer <- rep("Museum am Dom", length(title))

city <- rep("Wuerzburg", length(title))
street <- rep("Domerschulstraße 2", length(title))
zip <- rep(97070, length(title))
lng <- rep(9.9316742, length(title))
lat <- rep(49.793606, length(title))

# link so oft wie es Titel gibt
link <- rep("https://www.museum-am-dom.de/veranstaltungen.htm", length(title))

# mache dataframe daraus
mus_df <- data.frame(title, link, info, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)

colnames(mus_df) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip","date_start", "date_end", "time_start","time_end","price", "organizer")

rownames(mus_df) <- NULL