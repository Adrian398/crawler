library(tidyverse)
require(rvest)

url="http://tanzspeicher.de/spielplan"

url %>%
  read_html() -> raw_data

#def selectors
title_selector <- ".vsel-meta-title"
datum_selector <- ".vsel-meta-single-date span"
description_selector <- ".vsel-info p"

# Titel, Datum und Links crawlen
raw_data %>%
  html_nodes(title_selector) %>%
  html_text(trim = T) -> title

raw_data %>%
  html_nodes(datum_selector) %>%
  html_text(trim = T) -> dates

month_convertor <- function(given_date){
  given_date = gsub("Januar ","01.",given_date)
  given_date = gsub("Februar ","02.",given_date)
  given_date = gsub("März ","03.",given_date)
  given_date = gsub("April ","04.",given_date)
  given_date = gsub("Mai ","05.",given_date)
  given_date = gsub("Juni ","06.",given_date)
  given_date = gsub("Juli ","07.",given_date)
  given_date = gsub("August ","08.",given_date)
  given_date = gsub("September ","09.",given_date)
  given_date = gsub("Oktober ","10.",given_date)
  given_date = gsub("November ","11.",given_date)
  given_date = gsub("Dezember ","12.",given_date)
  return(given_date)
}
dates = map(dates,month_convertor)
dates = unlist(dates)
dates = gsub(", ",".",dates)
date_start = as.Date(dates,"%m.%d.%Y")



raw_data %>%
  html_nodes(description_selector) %>%
  html_text(trim = T) -> description

## clean description
relevant = c()

for ( i in seq(1:length(description))){
  if (strsplit(description[i], " ")[[1]][1] == "Leitung:" || strsplit(description[i], " ")[[1]][1] == "Eintrittspreis:"){
    
  }else
    relevant = c(relevant,i)
  
}
description = description[relevant]


# Restliche Daten eingeben
city <- "Wuerzburg"
street <- "Oskar-Laredo-Platz 1"
zip <- "97080"
organizer <- "Tanzspeicher"
lng <- "9.9102"
lat <- "49.8158"
link = url
date_end = NA
date_end = as.Date(date_end)
time_start = NA
time_end = NA



#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)

#add metadf idlocation
idlocation = 4775
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)


