##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
library(RCurl)

### Possible Improvements ###
# 1. time information is not unified, it seems to be manual typed in
# responsible: Wei


### Klinikum Würzburg Mitte ####
# crawl data
url = "http://www.kwm-klinikum.de/termine.html?page="

loop_active = T
loop_number = 1
urls = c()

while (loop_active) {
  temp_link = paste(url, as.character(loop_number), sep = "")
  
  temp_link %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".title a") %>%
    html_text(trim = T) -> temp_title
  
  if(!is_empty(temp_title)){
    urls = c(urls, temp_link)
    loop_number = loop_number + 1
  } else {
    loop_active = F
  }
}

# crawler setup
title = c()
description = c()
date_start = c()
link = c()
image_url = c()

for (url in urls) {
  url %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".title a") %>%
    html_text(trim = T) -> temp_title
  
  temp_date_start = gsub(":", "", str_extract(temp_title, ".*:"))
  temp_title = gsub(":\\s", "", str_extract(temp_title, ":.*"))
  
  raw_read %>%
    html_nodes(".readmore") %>%
    html_attr("href") %>%
    paste("http://www.kwm-klinikum.de/", ., sep = "") -> temp_link
  
  raw_read %>%
    html_nodes(".post-left img") %>%
    html_attr("src") %>%
    paste("http://www.kwm-klinikum.de", ., sep = "") -> temp_image_url
  
  
  for (event_link in temp_link) {
    event_link %>%
      read_html() -> raw_read
    
    raw_read %>%
      html_nodes(".rowall") %>%
      html_text(trim = T) -> temp_description
    
    description = c(description, temp_description)
    
  }
  
  title = c(title, temp_title)
  date_start = c(date_start, temp_date_start)
  link = c(link, temp_link)
  image_url = c(image_url, temp_image_url)
}

# fixed data setup
organizer = "Klinikum Würzburg Mitte"
url = "http://www.kwm-klinikum.de/"
category= rep("Kinder / Jugend", length(title))
date_end = rep(NA, length(title))
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))
lat = rep(49.79464, length(title))
lng = rep(9.95295, length(title))
street = rep("Salvatorstraße 7", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))

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
