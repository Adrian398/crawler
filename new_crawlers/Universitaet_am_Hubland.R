##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
library(BBmisc)
library(RCurl)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Universität am Hubland ####
# crawl data
url <- "https://www.uni-wuerzburg.de/aktuelles/veranstaltungen/"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".news-list__item-header a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".news-list__item-header a") %>%
  html_attr('href') -> temp_link

raw_read %>%
  html_nodes(".news-single__item-value") %>%
  html_text(trim = T) -> date

date_start = str_extract(date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
date_end = gsub("\\s", "", str_extract(date, "\\s[0-9]+\\.[0-9]+\\.[0-9]{4}"))

time_start = paste(str_extract_all(date, "[0-9]+:[0-9]+", simplify = T)[,1], ":00", sep = "")
time_end = paste(str_extract_all(date, "[0-9]+:[0-9]+", simplify = T)[,2], ":00", sep = "")

raw_read %>%
  html_nodes(".news-list__item-event-category .news-list__item-value") %>%
  html_text(trim = T) -> category

raw_read %>%
  html_nodes(".news-list__item") %>%
  html_text(trim = T) -> all

# filter out street information
street = gsub("Veranstalter.*", "", str_extract(all, "Ort:(.*)"))
street = gsub("Vortragender.*", "", street)
street = gsub("Ort:", "", street)

description = c()
image_url = c()
link = c()

# fix webiste address
for (web_link in temp_link) {
  if (!grepl("https://", web_link)){
    temp_url = paste("https://www.uni-wuerzburg.de", web_link, sep = "")
  } else {
    temp_url = web_link
  }
  link = c(link, temp_url)
}

# collect descriptions and img
for (url in link) {
  url %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".intro") %>%
    html_text(trim = T) -> temp_intro
  
  raw_read %>%
    html_nodes(".news-single__item-content") %>%
    html_text(trim = T) -> temp_description
  
  temp_description = paste(temp_intro, temp_description, sep = " ")
  
  if (is_empty(temp_description)){
    description = c(description, NA)
  } else {
    description = c(description, temp_description)
  }
  
  raw_read %>%
    html_nodes(".news-single__item-big-image a") %>%
    html_attr('href') -> temp_img_url
  
  if (is_empty(temp_img_url)){
    image_url = c(image_url, NA)
  } else {
    temp_img_url = paste("https://www.uni-wuerzburg.de", temp_img_url, sep = "")
    image_url = c(image_url, temp_img_url)
  }
}

# fixed data setup
organizer = "Universität am Hubland"
url = "https://www.uni-wuerzburg.de/aktuelles/veranstaltungen/"
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))
lat = rep(49.7815829, length(title))
lng = rep(9.9707861, length(title))
zip = rep(97070, length(title))
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
