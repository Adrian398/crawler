##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: your name


### Website name ####
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
url <- "https://www.yourHtml.com"

url %>%
  read_html() %>%
  html_nodes(".yourNode")-> raw_read

raw_read %>%
  html_nodes(".yourTitle") %>%
  html_text(trim = T) -> title

str_extract(start_date, "[0-9]+\\.\\s[[:alpha:]]+\\s[0-9]{4}") # substract date
str_extract(start_time, "[0-9]{2}:[0-9]{2}") # substract time

# fixed data setup
date_end = rep(NA, length(title))
time_end = rep(NA, length(title))
description = rep(NA, length(title))
organizer = rep("who?", length(title))
lat = rep(NA, length(title))
lng = rep(NA, length(title))
street = rep("Your street name", length(title))
zip = rep(NA, length(title))
city = rep("Würzburg", length(title))
link = rep("https://www.yourHtml.com", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
ukw <- data.frame(title = title,
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