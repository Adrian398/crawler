library(RCurl)
library(RJSONIO)
library(tidyverse)
library(rvest)
library(chron)

######---1: johanna Stahl Zentrum--------------------------------------------------------------####

##getting DATA
url <- "https://www.johanna-stahl-zentrum.de/ausstellungen-veranstaltungen/veranstaltungen-vortraege/index.html"
url %>%
  read_html() %>%
  html_nodes("#main p") %>%
  html_text() -> raw
#Entfernung der Einleitung sowie Erstellung eines Eintrags
str_replace_all(raw, "Programmvorschau für das Jahr 2018", "") -> raw
content <- paste(raw, collapse=" ")

##META DATA (location, time, date --> "all")
str_extract_all(content, "[0-9][0-9]\\.\\s[A-Z]{1}([a-z]|[ä])+\\s2018,\\s[0-9]+\\sUhr(,\\s.+?(?=\\t)|(?=\\t))") ->meta_jsz

##URL
url_jsz <- "https://www.johanna-stahl-zentrum.de/ausstellungen-veranstaltungen/veranstaltungen-vortraege/index.html"

##DESCRIPTION
#Aufteilung der Abschnitte:\t
str_extract_all(content, "(?<=\\t).+?(?=\\t)") ->abschnitte
#Auswahl der Einträge, welche nur den Kern-Text enthalten
abschnitte[[1]][!grepl("Kursnr|([0-9][0-9]\\.\\s[A-Z]{1}([a-z]|[ä])+\\s2018)|(Eintritt\\s(frei|[0-9]))",abschnitte[[1]])] -> description_jsz

##LOCATION 
#lng
lng_jsz <- as.numeric("9.945947831")

#lat
lat_jsz <-as.numeric("49.78981809")

##address
address_organizer_jsz <- "Johanna-Stahl-Zentrum"
address_street_jsz <- "Valentin-Becker-Straße 11"
address_zip_jsz <- "97072"
address_city_jsz <- "Wuerzburg"

##TITLE
title_jsz <- address_organizer_jsz

##DATE
#date_start
str_extract_all(meta_jsz,"[0-9][0-9]\\.\\s[A-Z]{1}([a-z]|[ä])+\\s2018") -> date_start_jsz
unlist(date_start_jsz) -> date_start_jsz

#Umwandlung der Monate in numerische Werte
months <- function(string){
  if (grepl("Januar",string)){
    gsub("\\sJanuar(\\s)?", "01.", string)
  }
  else if (grepl("Februar",string)){
    gsub("\\sFebruar(\\s)?", "02.", string)
  }
  else if (grepl("März",string)){
    gsub("\\sMärz(\\s)?", "03.", string)
  }
  else if (grepl("April",string)){
    gsub("\\sApril(\\s)?", "04.", string)
  }
  else if (grepl("Mai",string)){
    gsub("\\sMai(\\s)?", "05.", string)
  }
  else if (grepl("Juni",string)){
    gsub("\\sJuni(\\s)?", "06.", string)
  }
  else if (grepl("Juli",string)){
    gsub("\\sJuli(\\s)?", "07.", string)
  }
  else if (grepl("August",string)){
    gsub("\\sAugust(\\s)?", "08.", string)
  }
  else if (grepl("September",string)){
    gsub("\\sSeptember(\\s)?", "09.", string)
  }
  else if (grepl("Oktober",string)){
    gsub("\\sOktober(\\s)?", "10.", string)
  }
  else if (grepl("November",string)){
    gsub("\\sNovember(\\s)?", "11.", string)
  }
  else if (grepl("Dezember",string)){
    gsub("\\sDezember(\\s)?", "12.", string)
  }
  else string = string
}
lapply(date_start_jsz, months) -> date_start_jsz
unlist(date_start_jsz) -> date_start_jsz
as.Date(date_start_jsz, format="%d.%m.%Y") -> date_start_jsz

#date_end
date_start_jsz -> date_end_jsz

##TIME
str_extract_all(meta_jsz, "[0-9]+(?=\\sUhr)") ->time_start_jsz
unlist(time_start_jsz) -> time_start_jsz
times(paste0(time_start_jsz,":00:00")) -> time_start_jsz
NA -> time_end_jsz

##PRICE

price_jsz <- NA

##INFO (about location)
lapply(meta_jsz, function (x) str_extract_all(x, "(?<=\\sUhr,\\s).+")) -> info_jsz
lapply(info_jsz, function (x) gsub("character\\(0\\)",NA, x)) -> info_jsz
lapply(info_jsz, function(x) paste0("Veranstaltungsort: ",x)) -> info_jsz
data.frame(info_jsz) -> info_jsz
colnames(info_jsz) <- ("description")
data.frame(description_jsz) -> description_jsz
colnames(description_jsz) <- ("description")
description_jsz$rows <- row.names(description_jsz)
info_jsz$rows <- row.names(info_jsz)
inner_join(info_jsz,description_jsz, by="rows") ->description_df
paste0(description_df$description.x,sep=" | ",description_df$description.y) -> description_final


df_jsz <- data.frame(title_jsz,url_jsz,description_final,lng_jsz,lat_jsz,address_city_jsz,address_street_jsz,
                     address_zip_jsz,date_start_jsz,date_end_jsz,time_start_jsz,time_end_jsz, price_jsz,
                     address_organizer_jsz)
names(df_jsz) <- c("title",
                   "url",
                   "description",
                   "lng",
                   "lat",
                   "city",
                   "street",
                   "zip",
                   "date_start",
                   "date_end",
                   "time_start",
                   "time_end",
                   "price",
                   "organizer")

#----------------------------
#### PROBLEME
#--Auswahl der Texte (sehr hartes coding: Die Abschnitte, in welchen kein "Eintritt" und Datum vorkommen
#werden gezogen. Wie kann der Text allgemein herausgezogen werden?) -> daher viel regular expressions
#--Eintritt lässt sich nicht zuordnen(es besteht keine feste Struktur der Abschnitte. Teilweise mit, teiweise
#ohne Eintritt-Infos.)

