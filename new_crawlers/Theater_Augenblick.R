##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. only crawl the first page of events
# responsible: Wei


### Theater Augenblick ####
# crawl data
url = "https://www.theater-augenblick.de/programm-karten.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".spielplan-info h2") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".date") %>%
  html_text(trim = T) -> date_start

raw_read %>%
  html_nodes(".time") %>%
  html_text(trim = T) -> time_start

time_start = str_extract(time_start, "[0-9]{2}:[0-9]{2}")

raw_read %>%
  html_nodes(".preise") %>%
  html_text(trim = T) -> price

raw_read %>%
  html_nodes(".zum-stueck") %>%
  html_attr("href") -> link

link = paste("https://www.theater-augenblick.de/", link, sep = "")

description = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes("#article-9 p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  description = c(description, cache_description)
  
}

# fixed data setup
organizer = "Theater Augenblick"
url = "https://www.theater-augenblick.de/"
lat = rep(49.8018561, length(title))
lng = rep(9.9984487, length(title))
street = rep("Im Kreuz 1", length(title))
zip = rep(97076, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = paste(time_start, ":00", sep = "")
time_start <- chron(times = time_start)

# build table
crawled_df <- data.frame(
                    title = title,
                    date_start = date_start,
                    time_start = time_start,
                    price = price,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

meta_df = data.frame(url = url
                     , organizer = organizer)