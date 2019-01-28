##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Possible Improvements ###
# 1. scrape only the first page, following pages are not crawlable
# 2. price uncrwalable due to unstructured content
# responsible: Wei


### Kolping-Center Mainfranken ####
# crawl data
url <- "https://www.kolping-akademie-wuerzburg.de/alle-veranstaltungen"

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
organizer = "Kolping-Center Mainfranken"
url = "https://navi.kolping-akademie-wuerzburg.de/index.html"
category= rep("Vortrag / Seminar", length(title))
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))
lat = rep(49.79683, length(title))
lng = rep(9.93274, length(title))
street = rep("Kolpingplatz 1", length(title))
zip = rep(97070, length(title))
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
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

meta_df = data.frame(url = url
                     , organizer = organizer)

names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
