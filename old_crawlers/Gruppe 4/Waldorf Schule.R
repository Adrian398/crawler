######---2: Waldorf Schule-------------------------------------------------------------------------------------####

library(RCurl)
library(RJSONIO)
library(tidyverse)
library(rvest)
library(chron)

##META DATA
url <- "http://www.waldorf-wuerzburg.de/veranstaltungen/"
url %>%
  read_html() %>%
  html_nodes(".csc-textpic-intext-left b+ b , p:nth-child(1) b") %>%
  html_text() ->meta_wd
meta_wd[grepl(" ",meta_wd)]-> meta_wd
 
##DATE
date_wd <- str_extract_all(meta_wd, "([0-9]?[0-9]\\.[0-9]?[0-9]\\.\\s(bis))|([0-9][0-9]\\.\\s(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)\\s?(bis)?)|([0-9]?[0-9]\\.[0-9]?[0-9]\\.)")

#Veranstaltung mit 2 Terminen
doppelterTermin_func <- function (x) {
  if(length(x)>1) {
    if(!any(grepl("bis", x))) {
      x[[2]] 
    }
  }
  }
lapply(date_wd, doppelterTermin_func) -> doppelterTermin
which(!grepl("NULL", doppelterTermin)) -> doppelterTermin_index

#doppelte Termine werden entfernt
einmaligeTermine_func <- function (x) {
  if(length(x)>1) {
    if(!any(grepl("bis", x))) {
      x[-2] 
    } else {
      x <- gsub(" bis","",x)
    }
  } else {
    x
  }
  
}
lapply(date_wd, einmaligeTermine_func) -> einmaligeTermine
as.matrix(as.data.frame(einmaligeTermine)) -> einmaligeTermine
einmaligeTermine[1,] -> date_start_wd
einmaligeTermine[2,] -> date_end_wd

#Umwandlung der Monate in numerische Darstellung
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

sapply(date_start_wd, months) -> date_start_wd
sapply(date_end_wd, months) -> date_end_wd

#Ergänzung der Jahreszahl
add_year <- function(x) { paste0(x,"2018") }
lapply(date_start_wd, add_year) -> date_start_wd
lapply(date_end_wd, add_year) -> date_end_wd
lapply(doppelterTermin, add_year) -> doppelterTermin

#Ergänzung der "0" zu Beginn
add_zero <- function (x) {
  if(grepl("^[0-9]{1}\\.[0-9]?[0-9]\\.",x )) {
    paste0("0",x)
  } else {
    x
  }
}
lapply(date_start_wd, add_zero) -> date_start_wd
lapply(date_end_wd, add_zero) -> date_end_wd

##TIME
time_wd <- str_extract_all(meta_wd, "[0-9][0-9]:[0-9][0-9](?=\\sUhr)")
#speichern der doppelten Zeit (da wieder 2 Termine)
doppelteZeit_func <- function (x) {
  if(length(x)>1) {
    x[2]
  } 
}
sapply(time_wd, doppelteZeit_func) ->doppelteZeit
#doppelte Termine entfernen 
einfacheZeit_func <- function (x) {
  if(length(x)>1) {
    x[-2]
  } else {
    x
  }
}
sapply(time_wd, einfacheZeit_func) ->einfacheZeit
unlist(einfacheZeit) -> time_start_wd
NA -> time_end_wd

#enfernt character(0) zu NA
gsub("character\\(0\\)",NA, einfacheZeit) -> einfacheZeit

##DESCRIPTION
url <- "http://www.waldorf-wuerzburg.de/veranstaltungen/"
url %>%
  read_html() %>%
  html_nodes(".csc-textpic-intext-left p:nth-child(3) , #c4631 p:nth-child(2) , #c4630 p:nth-child(2) , p+ .indent p:nth-child(1) , .csc-textpic-intext-right p:nth-child(2)") %>%
  html_text(trim=T) ->description_wd

##TITLE
url <- "http://www.waldorf-wuerzburg.de/veranstaltungen/"
url %>%
  read_html() %>%
  html_nodes("h1") %>%
  html_text() -> h1_wd
h1_wd[seq(1,length(h1_wd),2)] ->title_wd
sapply(title_wd, tolower) -> title_wd

#die erste Überschrift ist immer "Veranstaltungen:" und fliegt daher raus
title_wd[-1] -> title_wd

#die Überschriften der Länge entsprechend des Textes aus dem df werden rausgezogen 
title_wd[seq(1,length(description_wd),1)] ->title_wd

##LOCATION
address_city_wd <- "Wuerzburg"
address_street_wd <- "Oberer Neubergweg 14"
address_zip_wd <- 97074 
organizer_wd <- "Waldorf Schule Wuerzburg"
lng_jsz <- as.numeric("9.95101")
lat_jsz <- as.numeric("49.77499")

##URL
url_wd <- "http://www.waldorf-wuerzburg.de/veranstaltungen/"

unlist(date_start_wd) -> date_start_wd
unlist(date_end_wd) -> date_end_wd

##PRICE
price_wd <-NA

##DATA FRAME
df_wd <- data.frame(title=title_wd,url=url_wd,description=description_wd,lng=lng_jsz,lat=lat_jsz,
                    city=address_city_wd,street=address_street_wd,zip=address_zip_wd,
                    date_start=date_start_wd,date_end=date_end_wd,time_start=time_start_wd,
                    time_end = time_end_wd,price=price_wd,organizer=organizer_wd)
rownames(df_wd) <- NULL

##doppelte Termine als neue Spalte ergänzen (als df2 erstellen)
df2_wd<-data.frame(title=title_wd[c(doppelterTermin_index)],url=url_wd,
                   description=description_wd[c(doppelterTermin_index)],
                   lng=lng_jsz,lat=lat_jsz,city=address_city_wd,street=address_street_wd,zip=address_zip_wd,
                   date_start=doppelterTermin[doppelterTermin_index], 
                   date_end=doppelterTermin[doppelterTermin_index],time_start=doppelteZeit[doppelterTermin_index],
                   time_end=NA, price=price_wd[doppelterTermin_index],
                   organizer=organizer_wd
                  )
colnames(df2_wd)<- c("title",
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

# kombiniere beide dfs
rbind(df_wd,df2_wd ) -> df_wd

# Umwandlungs des Formats: Datum und Uhrzeit
df_wd$date_start <- as.Date(df_wd$date_start, format="%d.%m.%Y")
df_wd$date_end <- as.Date(df_wd$date_end, format="%d.%m.%Y")
df_wd$time_start <- times(paste0(df_wd$time_start,":00"))

#nach Datum sortieren
df_wd <- df_wd[order(df_wd$date_start),]
row.names(df_wd) <- NULL

#----------------------------------------
####PROBLEME:
##--keine Struktur: viel Regex