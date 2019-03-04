##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Innovations- und Gründerzentrum Würzburg ####
# crawl data
url = "http://www.igz.wuerzburg.de/angebote-und-leistungen/veranstaltungen/index.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes("#embhl a , tr:nth-child(18) strong") %>%
  html_attr("href") -> link

link = link[!is.na(link)]
link = paste("http://www.igz.wuerzburg.de", link, sep = "")

description = c()
title = c()
date_start = c()
time_start = c()
time_end = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes("#embhl div:nth-child(1) strong") %>%
    html_text(trim = T) -> cache_title
  
  cache_raw_read %>%
    html_nodes("td") %>%
    html_text(trim = T) -> cache_description
  
  cache_date_start = str_extract(cache_description ,"[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")
  
  extract_time = str_extract_all(cache_description, "[0-9]{2}:[0-9]{2}", simplify = T)
  if (length(extract_time) > 0) {
    cache_time_start = extract_time[,1]
  } else {
    cache_time_start = ""
  }
  
  if (length(extract_time) > 1) {
    if (extract_time[,1] != extract_time[,2]){
      cache_time_end = extract_time[,2]
    } else {
      cache_time_end = ""
    }
  } else {
    cache_time_end = ""
  }
  
  description = c(description, cache_description)
  title = c(title, cache_title)
  time_start = c(time_start, cache_time_start)
  time_end = c(time_end, cache_time_end)
  date_start = c(date_start, cache_date_start)
  
}

# fixed data setup
organizer = "Innovations- und Gründerzentrum Würzburg"
url = "http://www.igz.wuerzburg.de/"
category= rep("Kurs", length(title))
lat = rep(49.8034612, length(title))
lng = rep(9.9981454, length(title))
street = rep("Friedrich-Bergius-Ring 15", length(title))
zip = rep(97076, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))
time_end = gsub("^:00", "", paste(time_end, ":00", sep = ""))
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    time_start = time_start,
                    time_end = time_end,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

meta_df = data.frame(url = url
                     , organizer = organizer)