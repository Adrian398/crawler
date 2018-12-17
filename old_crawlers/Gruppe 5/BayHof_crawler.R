BayHof_crawler <- function(){

library(tidyverse)
library(rvest)
library(stringr)
library(chron)
library(lubridate)

url <- "https://www.bayerischerhof.de/de/erleben-geniessen/eventkalender.html"

url %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes(".listViewPackageRow .cell , img") -> node_data

title_selector <- ".w430px h3"
datum_selector <- ".hook h2"
uhrzeit_selector <- ".hook h3"
ort_selector <- ".w430px h2"
link_selector <- ".listViewPackageRow .cell"

node_data %>%
  html_nodes(title_selector) %>%
  html_text(trim = T) -> title

node_data %>%
  html_nodes(datum_selector) %>%
  html_text(trim = T) -> raw_datum

node_data %>%
  html_nodes(uhrzeit_selector) %>%
  html_text(trim = T) -> raw_uhrzeit

#node_data %>%
 # html_nodes(ort_selector) %>%
 # html_text(trim = T) -> raw_ort

node_data %>%
  html_nodes(link_selector) %>%
  html_attr("href") -> raw_link


#url <- paste0("", raw_link)


#Date
regex_datum = "[0-9]{1,2}[\\./]{1}[0-9]{1,2}[\\./]?(20[1-2]{1}[0-9]{1})?"

raw_datum
raw_datum %>%
  str_extract_all(regex_datum, simplify = T) -> date_start

#kein Jahr auf der Seite mit angegeben
#cast in das Date format

paste0(date_start, "2018") -> date_start

dmy(date_start) -> date_start

date_end <- date_start


#timeOfDay
regex_uhrzeit = "[0-9]{1,2}[:]{1}[0-9]{1,2}"

raw_uhrzeit %>%
  str_extract_all(regex_uhrzeit, simplify = T) -> time_start

#time zu times format mit chron package
time_start <- times(paste0(time_start, ":00"))


time_end <- NA



#Ort
#raw_ort %>%
 # str_replace("Veranstaltungsort:", "") %>%
 #  str_replace_all("[:space:]{2,}", " ") -> ort



#price nicht vorhanden

price <- NA

#verlinkung nicht crawlbar

description <- NA

url <- NA

organizer <- "Bayerischer Hof"

#Seite für Anfahrt

url_location <- "https://www.bayerischerhof.de/de/service/lage-anfahrt.html"

url_location %>%
  read_html() %>%
  html_node(".address") %>%
  html_text(trim = T) -> url_location

url_location


#long and lat

lat <- 49.7479214
lng <- 9.8594372

#Adresse im Moment noch hardcoded aber wir erstellen bereits eine Lookup-Table

city <- "Muenchen"

street <- "Promenadenplatz 2-6"

zip <- 80333

#df erstellen

df <- data.frame(title, url, description, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)

#Factors zu chr casten
df$title <- as.character(df$title)
df$city <- as.character(df$city)
df$street <- as.character(df$street)
df$organizer <- as.character(df$organizer)
df$price <- as.character(df$price)
df$url <- as.character(df$url)




return(df)



}

