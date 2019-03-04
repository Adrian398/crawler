##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. only 8 events crawlable, because the website addresses are not identifiable
# responsible: Wei


### Exerzitienhaus Himmelspforten ####
# crawl data
url = "https://www.himmelspforten.net/veranstaltungen/"

raw_read = read_html(url)

raw_read %>%
  html_nodes(".event-item-text h2") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".event-item-text p") %>%
  html_text(trim = T) -> description

raw_read %>%
  html_nodes(".event-item-date") %>%
  html_text(trim = T) -> raw_date

raw_read %>%
  html_nodes(".time span") %>%
  html_text(trim = T) -> raw_time

raw_read %>%
  html_nodes(".price") %>%
  html_text(trim = T) -> price

raw_read %>%
  html_nodes(".event-item a") %>%
  html_attr("href") -> link

date = str_extract(raw_date, "[0-9]+\\.[0-9]+")
year = str_extract(raw_date, "[0-9]{4}")
date_start = paste(date, year, sep = ".")

date_end = str_extract_all(raw_time, "[0-9]+\\.[0-9]+\\.", simplify = T)[, 2]
date_end = gsub("^2019", "", paste(date_end, year[1], sep = ""))

time_start = str_extract_all(raw_time, "\\s[0-9]{2}\\.[0-9]{2}", simplify = T)[, 1]
time_start = gsub("\\.", ":", gsub("\\s", "", time_start))
time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))

time_end = str_extract_all(raw_time, "\\s[0-9]{2}\\.[0-9]{2}", simplify = T)[, 2]
time_end = gsub("\\.", ":", gsub("\\s", "", time_end))
time_end = gsub("^:00", "", paste(time_end, ":00", sep = ""))

# fixed data setup
organizer = "Exerzitienhaus Himmelspforten"
url = "https://www.himmelspforten.net/"
category= rep("Führung", length(title))
lat = rep(49.79973, length(title))
lng = rep(9.90437, length(title))
street = rep("Mainaustr. 42", length(title))
zip = rep(97082, length(title))
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
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

meta_df = data.frame(url = url
                     , organizer = organizer)
