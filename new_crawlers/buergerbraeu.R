##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: wei


### Bürgerbräu ####
# crawl data
url <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".header > h2 > a") %>%
  html_attr("href") -> link

title = c()
description = c()
date_start = c()
time_start = c()
location = c()
image_url = c()

for (cache_link in link) {
  
  cache_link %>%
    read_html() -> cache_read
  
  cache_read %>%
    html_nodes("h3") %>%
    html_text(trim = T) -> cache_title
  
  cache_read %>%
    html_nodes(".news-text-wrap") %>%
    html_text(trim = T) -> cache_description
  
  cache_read %>%
    html_nodes("time") %>%
    html_text(trim = T) -> cache_date
  
  cache_date_start = str_extract(cache_date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
  cache_time_start = paste(str_extract(cache_date, "[0-9]+:[0-9]+"), ":00", sep = "")
  
  cache_read %>%
    html_nodes(".news-list-category") %>%
    html_text(trim = T) -> cache_location
  
  cache_read %>%
    html_nodes(".lightbox img") %>%
    html_attr("src") -> cache_image_url
  
  if (is_empty(cache_image_url)){
    image_url = c(image_url, NA)
  } else {
    image_url = c(image_url, cache_image_url)
  }
  
  title = c(title, cache_title)
  description = c(description, cache_description)
  date_start = c(date_start, cache_date_start)
  time_start = c(time_start, cache_time_start)
  location = c(location, cache_location)
}

street = paste(location, "Frankfurter Straße 87", sep = ", ")

# fixed data setup
organizer = "Bürgerbräu"
url = "https://www.buergerbraeu-wuerzburg.de/"
category= rep("Ausstellung / Messe", length(title))
lat = rep(49.7937172, length(title))
lng = rep(9.894353, length(title))
zip = rep(97082, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
time_start <- chron(times = time_start)

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    time_start = time_start,
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