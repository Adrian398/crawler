ClubKatze_crawler <- function(){

library(tidyverse)
library(rvest)
require(stringr)
library(chron)
library(lubridate)

url_samstag <- "http://katze-club.de/portfolio_page/haltdieklappe/"
url_montag <- "http://katze-club.de/portfolio_page/montaggetraenkepyramide/"

get_data <- function(url,date_start){
  
  
  # Get HTML
  url %>% 
    read_html() -> raw_data
  
  raw_data%>%
    html_node(".title_subtitle_holder span")%>%
    html_text(trim = TRUE) -> title
  
  raw_data%>%
    html_node("strong")%>%
    html_text(trim = TRUE) -> zeit_price
  
  raw_data%>%
    html_node("h5+ p")%>%
    html_text(trim = TRUE) -> description
  
  #Adresse erstmal hardcoded, lookup wird noch erstellt und dann nachgetragen
  
  lng <- 9.92626
  
  lat <- 49.79761
  
  city <- "Wuerzburg"
  
  street <- "Gerberstraße 14"
  
  zip <- 97070
  
  organizer <- "Club Katze"
  
  

  str_split(zeit_price, "\n", simplify = FALSE) -> daten_split
  
  as.character(daten_split)
  
  daten_split[[1]] -> uhrzeit
  
  uhrzeit[1] -> uhrzeit
  
  
  regex_uhrzeit  = "[0-9]{2}"
  
  uhrzeit %>%
    str_extract_all(regex_uhrzeit, simplify = TRUE) -> uhrzeit
  
  uhrzeit[1] -> time_start
  uhrzeit[2] -> time_end
  
  paste0(time_start, ":00") -> time_start
  paste0(time_end, ":00") -> time_end
  
  daten_split[[1]] -> price_raw
  
  price_raw[2] -> price_raw
  
  price_raw %>%
    str_replace("Eintritt: ", "") -> price
  
  date_end <- date_start
  
  df <- data.frame(title, url, description, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)
  
  return(df)
  
}

#Auf der Webseite nur Montag und Freitag gegeben keine Daten - deshalb getMondays und getSaturdays

getAllMondays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==1]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

getAllSaturdays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==6]
  Ms[!is.na(Ms)]
}



getAllMondays(2018) -> all_mondays
getAllSaturdays(2018) -> all_saturdays

get_data(url_montag,all_mondays) -> data_monday
get_data(url_samstag,all_saturdays) -> data_saturdays


df_katze_events <- bind_rows(data_monday, data_saturdays)

#time zu times format mit chron package
df_katze_events$time_start <- times(paste0(df_katze_events$time_start, ":00"))
df_katze_events$time_end <- times(paste0(df_katze_events$time_end, ":00"))

#Factors zu chr casten
df_katze_events$title <- as.character(df_katze_events$title)
df_katze_events$city <- as.character(df_katze_events$city)
df_katze_events$street <- as.character(df_katze_events$street)
df_katze_events$organizer <- as.character(df_katze_events$organizer)
df_katze_events$price <- as.character(df_katze_events$price)
as.Date(df_katze_events$date_start)->df_katze_events$date_start
as.Date(df_katze_events$date_end)->df_katze_events$date_end


# str(df_katze_events$organizer)
# str(df_katze_events$title)
# str(df_katze_events$city)
# str(df_katze_events$street)
# str(df_katze_events$zip)
# str(df_katze_events$date_start)
# str(df_katze_events$date_end)
# str(df_katze_events$description)
# str(df_katze_events$url)
# str(df_katze_events$time_start)
# str(df_katze_events$time_end)
# str(df_katze_events$lng)
# str(df_katze_events$lat)


return(df_katze_events)

}