##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
library(RCurl)

### Possible Improvements ###
# 1. description can not be crawled properly due to the structure of the website
# responsible: wei


### Franz-Oberthür-Schule ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub(" Januar ","01.",given_date)
  given_date = gsub(" Februar ","02.",given_date)
  given_date = gsub(" März ","03.",given_date)
  given_date = gsub(" April ","04.",given_date)
  given_date = gsub(" Mai ","05.",given_date)
  given_date = gsub(" Juni ","06.",given_date)
  given_date = gsub(" Juli ","07.",given_date)
  given_date = gsub(" August ","08.",given_date)
  given_date = gsub(" September ","09.",given_date)
  given_date = gsub(" Oktober ","10.",given_date)
  given_date = gsub(" November ","11.",given_date)
  given_date = gsub(" Dezember ","12.",given_date)
  return(given_date)
}

# crawl data
url = "https://www.franz-oberthuer-schule.de/events/event/page/"

loop_active = T
loop_number = 1
urls = c()

while (loop_active) {
  temp_link = paste(url, as.character(loop_number), sep = "")
  if(url.exists(temp_link)){
    urls = c(urls, temp_link)
    loop_number = loop_number + 1
  } else {
    loop_active = F
  }
}

title = c()
date_start = c()
date_end = c()
time_start = c()
time_end = c()


for (link in urls) {
  link %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".entry-title") %>%
    html_text(trim = T) -> temp_title
  
  raw_read %>%
    html_nodes(".eo-event-date") %>%
    html_text(trim = T) -> temp_date
  
  for (date in temp_date){
    temp_date_end = str_extract(date,"[0-9]+\\.\\s[[:alpha:]]+\\s[0-9]+")
    
    # check if end date available
    if (!is.na(str_extract(date,"^[0-9]+\\s–"))){
      temp_date_start = paste(gsub("\\s–", "", str_extract(date,"^[0-9]+\\s–"))
                              , str_extract(date,"\\.\\s[[:alpha:]]+\\s[0-9]+"), sep = "")
    } else {
      temp_date_start = temp_date_end
      temp_date_end = NA
    }
    
    temp_date_start = month_convertor(temp_date_start)
    temp_date_end = month_convertor(temp_date_end)
    
    # check if time available
    if (!is.na(str_extract(date,"[0-9]+:[0-9]+"))){
      date %>%
        str_extract("[0-9]+:[0-9]+") %>%
        paste(.,":00", sep = "") -> temp_time_start
      
      if (!is.na(str_extract(date,"–\\s[0-9]+:[0-9]+"))){
        date %>%
          str_extract("–\\s[0-9]+:[0-9]+") %>%
          gsub("–\\s", "", .) %>%
          paste(.,":00", sep = "") -> temp_time_end
      } else {
        temp_time_end = NA
      }
        
    } else {
      temp_time_start = NA
      temp_time_end = NA
    }
    
    date_start = c(date_start, temp_date_start)
    date_end = c(date_end, temp_date_end)
    time_start = c(time_start, temp_time_start)
    time_end = c(time_end, temp_time_end)
  }
  
  title = c(title, temp_title)
}

# fixed data setup
category = rep("Kinder / Jugend", length(title))
description = rep(NA, length(title))
organizer = "Franz-Oberthür-Schule"
lat = rep(49.7865, length(title))
lng = rep(9.95439, length(title))
street = rep("Zwerchgraben 2", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))
link = rep("https://www.franz-oberthuer-schule.de/events/event/page/", length(title))
image_url = rep(NA, length(title))
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
                 category = category,
                 title = title,
                 date_start = date_start,
                 date_end = date_end, 
                 time_start = time_start,
                 time_end = time_end,
                 price = price,
                 advanced_price = advanced_price,
                 description = description,
                 lat = lat,
                 lng = lng,
                 street = street,
                 zip = zip,
                 city = city,
                 link = link,
                 image_url = image_url)

meta_df = data.frame(url = url
                     , organizer = organizer)
