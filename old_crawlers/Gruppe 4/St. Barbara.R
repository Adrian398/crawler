#####---3: St.Barbara -----------------------------------------------------------------------------------------------####
library(RCurl)
library(RJSONIO)
library(tidyverse)
library(rvest)
library(chron)

##TITLE
  url <- "http://www.kirche-frauenland.de/"
  url %>%
    read_html() %>%
    html_nodes(".itemtitle a") %>%
    html_text() ->title_st

##URL
  url %>%
    read_html() %>%
    html_nodes(".itemtitle a") %>%
    html_attr("href") -> links_st
  paste0("http:",links_st,"") -> links_st
  
##DATE
  get_date <- function (link) {
    url <- link
    url %>%
      read_html() %>%
      html_nodes(".date") %>%
      html_text()
  }
  lapply(links_st,get_date) -> date_st
  lapply(date_st,function(x){x[1]}) -> date_st
  lapply(date_st,function(x){if(grep("-",date_st)){str_split(x," - ")}}) -> date_st
  as.matrix(as.data.frame(date_st)) ->date_st
 
  date_start_st <- as.Date(date_st[1,], format="%d.%m.%Y")
  date_end_st <- as.Date(date_st[2,], format="%d.%m.%Y")

##TIME
  url <- "http://www.kirche-frauenland.de/"
  url %>%
    read_html() %>%
    html_nodes(".datetime") %>%
    html_text() ->meta_st
  
  #die entsprechende Zeit wird gezogen
  time_start_st <- str_extract_all(meta_st, "[0-9][0-9]:[0-9][0-9](?=\\sUhr)")
  gsub("character\\(0\\)",NA, time_start_st) -> time_start_st
  times(paste0(time_start_st,":00")) -> time_start_st
  time_end_st <- NA

##LOCATION
    url <- "http://kirche-frauenland.de/index.html/kinderkirche/9e6278b8-b615-40c1-a3f3-6f048d2f466d?mode=detail"
    url %>%
      read_html() %>%
      html_nodes("td") %>%
      html_text(trim=T) -> raw
      raw[raw!=""] -> raw
      raw[seq(grep("Ort",raw)+1,length(raw))] -> location_st
      location_st[1] -> organizer_st
      location_st[3] -> street_st
     str_split(location_st[4]," ")[[1]][1] ->zip_st
     str_split(location_st[4]," ")[[1]][2] ->city_st
     lng_st <- 9.95795
     lat_st <- 49.79574

##DESCRIPTION
  get_info <- function (link) {
    url <- link
    url %>%
      read_html() %>%
      html_nodes("#col3_content1 p") %>%
      html_text(trim=T)
  }
  lapply(links_st,get_info) -> info_st
  gsub("\\n"," ",info_st) -> info_st
  gsub("character\\(0\\)",NA, info_st) -> description_st
  
##PRICE
  str_extract(description_st,"([0-9]+,[0-9]+.(?=€))|(kostenfrei)|((?<=Eintritt:\\s)[0-9]+)s") -> price_st

##DATA FRAME
  df_st <- data.frame(title=title_st, url=links_st, description=description_st,lng=lng_st,lat=lat_st,
                      city=city_st,street=street_st,zip=zip_st,date_start=date_start_st,date_end=date_end_st,
                      time_start=time_start_st,time_end=time_end_st,price=price_st,organizer=organizer_st)

#--------------------------------
####PROBLEME
##--es wird nur die erste Seite erkannt. Wenn versucht wird, die anderen Seiten zu ziehen, erhält man
#die gleichen Daten wir bei der ersten Seite...weird!?