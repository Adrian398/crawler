##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Possible Improvement ###
# 1. description can not be crawled, only with java
# 2. crawl only for next 3 months
# responsible: Wei


### Jugendbildungsstätte Unterfranken ####
# crawl setup
url_year = c()
url_month = c()
title = c()
date_start = c()
date_end = c()
time_start = c()
time_end = c()
organizer = c()
description = c()
lat = c()
lng = c()
street = c()
zip = c()
city = c()

# year and month for url setup
for (n in 0:2){
  temp_year = as.integer(format(Sys.Date(), "%Y"))
  temp_month = as.integer(format(Sys.Date(), "%m")) + n
  if (temp_month > 12){
    url_year = c(url_year, temp_year + 1)
    url_month = c(url_month, sprintf("%02d", temp_month - 12))
  } else {
    url_year = c(url_year, temp_year)
    url_month = c(url_month, sprintf("%02d", temp_month))
  }
  
}

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


for (x in 1:length(url_month)){
  url <- paste("http://www.jubi-unterfranken.de/events/", url_year[x], "-", url_month[x], sep = "")
  
  url %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".tribe-events-has-events > div > h3 > a") %>%
    html_attr("href") %>% 
    unique() -> link
  
  # skip crawling when no links available
  if (!is_empty(link)) {
    for (n in link){
      # crawl data
      single_date_event = F
      temp_url <- n
      temp_url %>%
        read_html() -> temp_raw_read
      
      
      temp_raw_read %>%
        html_nodes(".tribe-events-single-event-title") %>%
        html_text(trim = T) -> temp_title
      
      temp_raw_read %>%
        html_nodes(".tribe-event-date-start") %>%
        html_text(trim = T) -> temp_raw_date_start
      
      temp_raw_read %>%
        html_nodes(".tribe-event-date-end") %>%
        html_text(trim = T) -> temp_raw_date_end
      
      if (is_empty(temp_raw_date_end)){
        # single day event check
        single_date_event = T
        temp_raw_read %>%
          html_nodes(".tribe-event-time") %>%
          html_text(trim = T) %>%
          paste(":00", sep = "")-> temp_time_end
      }
      
      temp_raw_read %>%
        html_nodes(".tribe-organizer") %>%
        html_text(trim = T) -> temp_organizer
      
      temp_raw_read %>%
        html_nodes(".tribe-street-address") %>%
        html_text(trim = T) -> temp_street
      
      temp_raw_read %>%
        html_nodes(".tribe-locality") %>%
        html_text(trim = T) -> temp_city
      
      temp_raw_read %>%
        html_nodes(".tribe-postal-code") %>%
        html_text(trim = T) -> temp_zip
      
      # data mutation
      temp_date_start = paste(str_extract(temp_raw_date_start, "[^|]+"), url_year[x], sep = "")
      temp_time_start = paste(str_extract(temp_raw_date_start, "[0-9]{2}:[0-9]{2}"),":00", sep = "")
      
      if (single_date_event){
        temp_date_end = NA
      } else {
        temp_date_end = paste(str_extract(temp_raw_date_end, "[^|]+"), url_year[x], sep = "")
        temp_time_end = paste(str_extract(temp_raw_date_end, "[0-9]{2}:[0-9]{2}"),":00", sep = "")
      }
      
      # data merge
      title = c(title, temp_title)
      date_start = c(date_start, temp_date_start)
      date_end = c(date_end, temp_date_end)
      time_start = c(time_start, temp_time_start)
      time_end = c(time_end, temp_time_end)
      organizer = c(organizer, temp_organizer)
      description = c(description, "")
      lat = c(lat, 49.73939)
      lng = c(lng, 9.9547)
      street = c(street, temp_street)
      zip = c(zip, temp_zip)
      city = c(city, temp_city)
    }
  }
}

# data type conversion
date_start = as.Date(month_convertor(date_start), "%d.%m.%Y")
date_end = as.Date(month_convertor(date_end), "%d.%m.%Y")

time_start = chron(times = time_start)
time_end = chron(times = time_end)


# Build table
df <- data.frame(title = title,
                  date_start = date_start,
                  date_end = date_end, 
                  time_start = time_start,
                  time_end = time_end,
                  description = description,
                  organizer = organizer,
                  lat = lat,
                  lng = lng,
                  street = street,
                  zip = zip,
                  city = city,
                  link = link)

#set up to write to database
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
meta_df = df[c("organizer", "link")][1,]
names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

