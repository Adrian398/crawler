##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Possible Improvements ###
# 1. lat long are not 100% correct, becase ggmap has visit limit and therefore unusable
# responsible: Wei


### Schroeder Haus  ####
# functions initialization #
time_convertor = function(time){
  if(!is.na(time)){
    time = gsub("\\s", "", time)
    time = gsub("\\.", ":", time)
    time = paste(time, ":00", sep = "")
  }
  return(time)
}

# crawl data
url <- "https://www.schroeder-haus.de/programm/"

url %>%
  read_html() %>%
  html_nodes(".col-md-12")-> raw_read

raw_read %>%
  html_nodes(".veranstaltung span") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".bg-dynamic") %>%
  html_attr("href") -> link

link = paste("https://www.schroeder-haus.de/", link, sep = "")

# vector setup for loop 
date_start = c()
date_end = c()
time_start = c()
time_end = c()
description = c()
price = c()
street = c()
city = c()
zip = c()


for (url in link) {
  url %>%
    read_html() -> raw_read
  
  # extract date and time
  raw_read %>%
    html_nodes(".subheader b") %>%
    html_text(trim = T) -> temp_date
  
  temp_date_start = str_extract(temp_date, "[0-9]+\\.[0-9]+\\.")
  temp_date_end = str_extract(temp_date, "[0-9]+\\.[0-9]+\\.[0-9]+")
  temp_date_end = paste(str_extract(temp_date_end, "[0-9]+\\.[0-9]+"), ".20", substr(temp_date_end, nchar(temp_date_end)-1, nchar(temp_date_end)), sep = "")
  temp_date_start = paste(temp_date_start, "20", substr(temp_date_end, nchar(temp_date_end)-1, nchar(temp_date_end)), sep = "")
  temp_time_start = tryCatch(str_extract_all(temp_date, "\\s[0-9]{2}\\.[0-9]{2}\\s"
                                             , simplify = T)[,1], error = function(err) NA)
  temp_time_start = time_convertor(temp_time_start)
  temp_time_end = tryCatch(str_extract_all(temp_date, "\\s[0-9]{2}\\.[0-9]{2}\\s"
                                           , simplify = T)[,2], error = function(err) NA)
  temp_time_end = time_convertor(temp_time_end)
  
  # extract location info
  raw_read %>%
    html_nodes(".ort") %>%
    html_text(trim = T) -> location
  
  location %>%
    str_extract("\\,\\s.*\\,") %>%
    gsub("\\,\\s", "",.) %>%
    gsub(",", "",.)-> temp_street
  
  temp_city = str_extract(location, "[0-9]+\\s[[:alpha:]]+")
  temp_zip = str_extract(temp_city, "[0-9]+")
  temp_city = str_extract(temp_city, "[[:alpha:]]+")
  
  raw_read %>%
    html_nodes("#text") %>%
    html_text(trim = T) -> temp_description
  
  raw_read %>%
    html_nodes("#infos") %>%
    html_text(trim = T) %>%
    str_extract("\200\\s[0-9]+") -> temp_price
  
  # aggregate data
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
  time_start = c(time_start, temp_time_start)
  time_end = c(time_end, temp_time_end)
  description = c(description, temp_description)
  price = c(price, temp_price)
  city = c(city, temp_city)
  street = c(street, temp_street)
  zip = c(zip, temp_zip)
}

# fixed data setup (not accurate)
category = rep("Vortrag / Seminar", length(title))
organizer = "Rudolf-Alexander-Schröder-Haus"
lat = rep(49.78972, length(title))
lng = rep(9.93501, length(title))
image_url = rep(NA, length(title))
advanced_price = rep(NA, length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

price = as.numeric(price)
# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    date_end = date_end, 
                    time_start = time_start,
                    time_end = time_end,
                    price = price,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

#add metadf idlocation
idlocation = 4149
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
