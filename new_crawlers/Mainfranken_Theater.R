##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")


### Mainfranken Theater  ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub("Januar","01.",given_date)
  given_date = gsub("Februar","02.",given_date)
  given_date = gsub("März","03.",given_date)
  given_date = gsub("April","04.",given_date)
  given_date = gsub("Mai","05.",given_date)
  given_date = gsub("Juni","06.",given_date)
  given_date = gsub("Juli","07.",given_date)
  given_date = gsub("August","08.",given_date)
  given_date = gsub("September","09.",given_date)
  given_date = gsub("Oktober","10.",given_date)
  given_date = gsub("November","11.",given_date)
  given_date = gsub("Dezember","12.",given_date)
  return(given_date)
}

# crawl data
url <- "https://www.mainfrankentheater.de/spielplan/spielzeit-18-19/"

url %>%
  read_html() %>%
  html_nodes(".season__production") %>%
  html_attr("href") -> link

link = paste0("https://www.mainfrankentheater.de", link)

title = c()
date_start = c()
date_end = c()
description = c()

for (temp_url in link){
  temp_url %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_node(".headline__headline") %>%
    html_text(trim = T)-> temp_title
  
  raw_read %>%
    html_node(".productionschedule__infoheadline") %>%
    html_text(trim = T) %>%
    strsplit(" ")  %>%
    unlist() -> temp_date_start
  
  day = temp_date_start[1]
  month = month_convertor(temp_date_start[2])
  year = temp_date_start[3]
  
  temp_date_start = paste0(paste0(day,month),year)
  
  raw_read %>%
    html_node(".--margintop-none") %>%
    html_text(trim = T)-> temp_description
  
  title = c(title, temp_title)
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
  description = c(description, temp_description)
  
}

# fixed data setup
time_start = NA
time_end = NA
organizer = "Mainfranken Theater Würzburg"
lat = rep(49.79304, length(title))
lng = rep(9.95808, length(title))
street = rep("Richard-Wagner-Straße 60", length(title))
zip = rep(97074, length(title))
city = "Würzburg"

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(NA, "%d.%m.%Y")


# build table
df <- data.frame(title = title,
                 date_start = date_start,
                 date_end = date_end, 
                 time_start = time_start,
                 time_end = time_end,
                 description = description,
                 lat = lat,
                 lng = lng,
                 street = street,
                 zip = zip,
                 city = city,
                 link = link)

#set up to write to database
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
idlocation = 4153
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
