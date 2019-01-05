##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Sparkasse Mainfranken ####
# crawl data
url <- "https://www.sparkasse-mainfranken.de/de/home/ihre-sparkasse/termine-und-events.html"

url %>%
  read_html() %>%
  html_nodes(".if6_section")-> raw_read

raw_read %>%
  html_nodes(".left:nth-child(2)") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".table .left:nth-child(1)") %>%
  html_text(trim = T) -> temp_date

raw_read %>%
  html_nodes(".left~ .left+ .left") %>%
  html_text(trim = T) -> description

date_start = str_extract(temp_date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
date_end = gsub("\n", "", str_extract(temp_date, "\n[0-9]+\\.[0-9]+\\.[0-9]{4}"))

# fixed data setup
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
organizer = rep("Sparkasse Mainfranken", length(title))
lat = rep(49.79417, length(title))
lng = rep(9.93496, length(title))
street = rep("Hofstraße 9", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))
link = rep("https://www.sparkasse-mainfranken.de/de/home/ihre-sparkasse/termine-und-events.html", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
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