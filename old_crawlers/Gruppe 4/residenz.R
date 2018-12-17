library(plyr)
library(rvest)
library(tidyverse)
library(purrr)
library(gsubfn)
library(stringr)
library(chron)

url <- "http://www.residenz-wuerzburg.de/deutsch/aktuell/veranst.asp"

# ziehe Tag und Uhrzeit
url %>%
  read_html() %>%
  html_nodes(".spalte-70prozent strong") %>%
  html_text(trim = T) -> title

# schmeiße unnötige Symbole raus
title <- sapply(title, function(x) gsub("\\\u0096\\s","- ",x))

# ziehe Zeit und Datum
url %>%
  read_html() %>%
  html_nodes(".spalte-27prozent .medium") %>%
  html_text(trim = T) -> date_time

# Funktion die doppel-null hinzufügt
add_double_zeros <- function (string){
  if(length(string) == 1){first_num <- str_extract(string, '\\d{1,2}(?=-)')
  first <- paste0(first_num, ":00")
  
  sec_num <- str_extract(string, '(?<=-)\\d{1,2}')
  sec <- paste0(sec_num, ":00")
  
  string <- paste(first, sec, sep=" - ")}
  
  else string <- string
}

# fehlt bei Datum eine führende Null füg eine hinzu
add_lead_zero <- function (string){
  
  if (nchar(str_extract(string, '\\d{1,2}(?=.)')) == 1){
    paste0("0", string)}
  
  
  else string = string
}


# ändere Char-Monate in numeric
months <- function(string){
  if (grepl("Januar",string)){
    gsub("Januar", "01.", string)
  }
  else if (grepl("Februar",string)){
    gsub("Februar", "02.", string)
  }
  else if (grepl("März",string)){
    gsub("März", "03.", string)
  }
  else if (grepl("April",string)){
    gsub("April", "04.", string)
  }
  else if (grepl("Mai",string)){
    gsub("Mai", "05.", string)
  }
  else if (grepl("Juni",string)){
    gsub("Juni", "06.", string)
  }
  else if (grepl("Juli",string)){
    gsub("Juli", "07.", string)
  }
  else if (grepl("August",string)){
    gsub("August", "08.", string)
  }
  else if (grepl("September",string)){
    gsub("September", "09.", string)
  }
  else if (grepl("Oktober",string)){
    gsub("Oktober", "10.", string)
  }
  else if (grepl("November",string)){
    gsub("November", "11.", string)
  }
  else if (grepl("Dezember",string)){
    gsub("Dezember", "12.", string)
  }
  else string = string
}


#date_time <- sapply(date_time, function(x) gsub("bis","-",x))

# such nach Datum in dem Format...
date <- strapply(date_time, "\\d{1,2}\\.\\s\\w+\\s\\d{4}")

# such nach Zeit in dem Format... und füg nuller dazu
time <-strapply(date_time, "\\d{1,2}-\\d{1,2}")

time <- sapply(time, add_double_zeros)
time <- as.matrix(time)
#time <- gsub("NULL", "", time)

# zieh die erste Zeit, weil aktuell zweite unrelevant sind
time_start <- substr(time, start = 1, stop = 5)
time_end <- substr(time, start = 9, stop = 13)
time_start <- gsub("NULL", NA, time_start)
time_end <- sapply(time_end, function(x) ifelse(x == "", NA, x))
  
# wandle jedes element, auch in jeder subliste in numerische Monate um und füge ggf. eine führende Null hinzu
date <- sapply(date, function(x) sapply(x, function(y) months(y)))
date <- sapply(date, function(x) sapply(x, function(y) gsub(" ", "", y)))
date <- sapply(date, function(x) sapply(x, function(y) add_lead_zero(y)))

# transformiere
date <- t(as.data.frame(date))

# ziehe generelle Infos
url %>%
  read_html() %>%
  html_nodes("#container p:nth-child(2)") %>%
  html_text(trim = T) -> infos

# ziehe Location
url %>%
  read_html() %>%
  html_nodes(".spalte-70prozent p:nth-child(1)") %>%
  html_text() -> location
 

# ziehe wort vor Würzburg und Würzburg
loca <- sapply(location, function(x) str_extract(x, '\\w+\\sWürzburg'))


# es gibt nur 3 mögliche Locations, die immer dieselbe Adresse haben
adresse_f <- function(string){
  if((string == "Hofgarten Würzburg")|(string =="Residenz Würzburg")){
    city = "Wuerzburg"
    street = "Residenzplatz 2"
    zip = 97070
    lat = 49.79331	
    lng = 9.93844
  }
  else if(string == "Festung Marienberg"){
    city = "Wuerzburg"
    street = "Marienberg"
    zip = 97012
    lat = 49.789722
    lng = 9.921389
  }
  else {
    city = NA
    street = NA
    zip = NA
    lat = NA
    lng = NA
  }
  return(c(lng = lng, lat = lat, city = city, street = street, zip = zip))
}

adress <- sapply(loca, adresse_f)

adress <- t(as.data.frame(adress))

# preis ist im Beispiel immer frei, ergo untersuche 30 Character um das wort "Eintritt" herum auf "frei"

beschreib_vor <- sapply(infos, function (x) str_extract(x, "(?<=Eintritt).{1,15}"))
beschreib_nach <- sapply(infos, function(x) str_extract(x, ".{1,15}(?=Eintritt)"))

beschreib_total <- paste0(beschreib_vor, beschreib_nach)

price <- sapply(beschreib_total, function(x) ifelse(grepl("frei", x), "frei", NA))
# so viele Links wie veranstaltungen

link <- rep("http://www.residenz-wuerzburg.de/deutsch/aktuell/veranst.asp", length(title))

organizer <- rep("Bayerische Schlösserverwaltung", length(title))


resi_df <- data.frame(title, link, infos, adress, date, time_start, time_end, price, organizer)

colnames(resi_df) <- c("title",  "url", "description", "lng", "lat", "city", "street", "zip","date_start", "date_end", "time_start","time_end","price", "organizer")

# wandle in Datumsformat um und sortiere sie später
resi_df$date_start <- as.Date(resi_df$date_start, format="%d.%m.%Y")
resi_df$date_end <- as.Date(resi_df$date_end, format="%d.%m.%Y")
resi_df$time_start <- times(paste0(resi_df$time_start,":00"))
resi_df$time_end <- times(paste0(resi_df$time_end,":00"))                

rownames(resi_df) <- NULL
