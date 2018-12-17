library(tidyverse)
library(rvest)
library(tidyr)
library(lubridate)
library(RcppBDT)

Fab <- function() {

#----------------FabLab---------------------------------------------------------------------

url_fab <- "https://fablab-wuerzburg.de/events"

url_fab %>%
  read_html() %>%
  html_nodes("#regelmaessige_veranstaltungen+ .level2 .li") %>%
  html_text() -> titel_fab

url_fab %>%
  read_html() %>%
  html_nodes("b") %>%
  html_text() -> org_fab

url_fab %>%
  read_html() %>%
  html_nodes("#regelmaessige_veranstaltungen+ .level2 .wikilink1") %>%
  html_attr("href")-> link_fab

link_fab<- c(link_fab,NA)

for(i in 1:length(link_fab)){
if(is.na(link_fab[i])){
  }
else{
  link_fab[i]<- paste0("https://fablab-wuerzburg.de", link_fab[i])
}
}
df_fab <- data.frame(title = titel_fab, url= link_fab, organizer = org_fab )

#Titel und Datum trennen
df_fab<-separate(data=df_fab, col=title, into=c("title", "date_start"), sep = "- Jeden")

#Datum und Uhrzeit trennen
df_fab<-separate(data=df_fab, col=date_start, into=c("date_start", "time_start"), sep = "ab")

#Uhr an Startuhrzeit h?ngen
df_fab$time_start <- paste0(df_fab$time_start, ":00")
df_fab$time_start <- times(df_fab$time_start)

#Beschreibung, Enddatum, Enduhrzeit, Ort, Adresse, lat, lon hinzuf?gen
df_fab$description <- NA
df_fab$date_end <- NA
df_fab$time_end <- NA
df_fab$city <- "Wuerzburg"
df_fab$street <- "Veitsh?chheimer-Str. 14"
df_fab$zip <- 97080
df_fab$lat <- 49.8158268
df_fab$lng <- 9.9102608
df_fab$price <- NA

df_fab_to <- df_fab[c("title","url", "description","lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price","organizer")]

#Endatum = Startdatum und Enduhrzeit = Startuhrzeit
for (i in 1:nrow(df_fab_to)){
  if (is.na(df_fab_to[i,]$date_end)){
    df_fab_to[i,]$date_end <- df_fab_to[i,]$date_start
  }
}

# Enduhrzeit = Startuhrzeit falls na
for (i in 1:nrow(df_fab_to)){
  if (is.na(df_fab_to[i,]$time_end)){
    df_fab_to[i,]$time_end <- as.character(df_fab_to[i,]$time_start)
  }
}

#Time_end im times Format
df_fab_to$time_end <- times(df_fab_to$time_end)

# Wochentage in Daten umwandeln
# Jede Woche
#letzter Eintrag
for(i in 1:(nrow(df_fab_to)-1)){
  
  if(str_detect(df_fab_to$date_start[i], "Sonntag")){
    searchday <- 7
  }
  if(str_detect(df_fab_to$date_start[i], "Samstag")){
    searchday <- 6
  }
  if(str_detect(df_fab_to$date_start[i], "Freitag")){
    searchday <- 5
  }
  if(str_detect(df_fab_to$date_start[i], "Donnerstag")){
    searchday <- 4
  }
  if(str_detect(df_fab_to$date_start[i], "Mittwoch")){
    searchday <- 3
  }
  if(str_detect(df_fab_to$date_start[i], "Dienstag")){
    searchday <- 2
  }
  if(str_detect(df_fab_to$date_start[i], "Montag")){
    searchday<- 1
  }
  heute <- Sys.Date()
  zaehler <- heute
  numdayh <- wday(zaehler)
  numnaechst <- searchday - numdayh + 1 
  datenaechst <- zaehler + numnaechst
  zaehler <- datenaechst
  day <- substring(datenaechst, 9,10)
  monat <- substring(datenaechst, 6,7)
  naechstevent <- paste0(day,'.',monat,collapse = NULL)
  date <- c(naechstevent)
  
  df_fab_to$date_start[i]<- date
}

#Eintrag f?r diesen Monat, d.h. bsp. 3. Freitag Juni --> 15.06.2018
for(i in nrow(df_fab_to)){
  
  if(str_detect(df_fab_to$date_start[i], "Sonntag")){
    searchday <- Sun
  }
  if(str_detect(df_fab_to$date_start[i], "Samstag")){
    searchday <- Sat
  }
  if(str_detect(df_fab_to$date_start[i], "Freitag")){
    searchday <- Fri
  }
  if(str_detect(df_fab_to$date_start[i], "Donnerstag")){
    searchday <- Thu
  }
  if(str_detect(df_fab_to$date_start[i], "Mittwoch")){
    searchday <- Wed
  }
  if(str_detect(df_fab_to$date_start[i], "Dienstag")){
    searchday <- Tue
  }
  if(str_detect(df_fab_to$date_start[i], "Montag")){
    searchday<- Mon
  }
  heute <- Sys.Date()
  monat <- month(heute)
  year <- year(heute)
  treffen_monat <- getNthDayOfWeek(third, searchday, monat, year)
  df_fab_to$date_start[i] <- format(as.Date(treffen_monat, origin="1970-01-01"))
}

#Datum au?er letzte Zeile
df_fab_to[1:2,]$date_start <- paste0(df_fab_to[1:2,]$date_start, ".2018")
df_fab_to[1:2,]$date_start <- as.character(as.Date(df_fab_to[1:2,]$date_start,format='%d.%m.%Y'))
df_fab_to[1:2,]$date_start <- format(as.Date(df_fab_to[1:2,]$date_start, origin="1970-01-01"))

df_fab_to$date_start <- as.Date(df_fab_to$date_start)

df_fab_to$date_end <-df_fab_to$date_start
#------------------------------------------------------------

return(df_fab_to)

}