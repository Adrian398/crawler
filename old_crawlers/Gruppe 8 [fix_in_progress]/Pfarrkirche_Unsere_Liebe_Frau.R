##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Pfarrkirche Unsere Liebe Frau ####
# crawl data
url <- "http://kirche-frauenland.de/index.html"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".itemcontent") %>%
  html_text(trim = T) -> description

description = gsub("\r\n\t\t\t\t\t\n\t\t\t\t\t\tMEHR","", description)

raw_read %>%
  html_nodes(".datetime") %>%
  html_text(trim = T) -> date

date_start = str_extract(date, "[0-9]+\\.[0-9]+\\.[0-9]+")
date_end = gsub("bis\\s", "", str_extract(date, "bis\\s[0-9]+\\.[0-9]+\\.[0-9]+"))

time_start = str_extract(date, "[0-9]+:[0-9]+")
time_end = str_extract(str_extract(date, "bis.*"), "[0-9]+:[0-9]+")

# time transformation
for (n in 1:length(title)) {
  if(!is.na(time_start[n])){
    time_start[n] = paste(time_start[n],":00", sep = "")
  }
  if(!is.na(time_end[n])){
    time_end[n] = paste(time_end[n],":00", sep = "")
  }
}

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_attr("href") %>%
  paste("https:", ., sep = "")-> link

# fixed data setup
organizer = "Pfarrkirche Unsere Liebe Frau"
url = "http://kirche-frauenland.de/index.html"
category= rep("Religion", length(title))
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))
lat = rep(49.7866, length(title))
lng = rep(9.94851, length(title))
street = rep("Zu-Rhein-Straße 3", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))
image_url = rep(NA, length(title))

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
