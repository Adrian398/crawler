##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Possible Improvements ###
# 1. none
# responsible: Wei


### Plastisches Theater Hobbit ####
# crawl data
url <- "http://www.theater-hobbit.de/"

url %>%
  read_html() %>%
  html_nodes("#stacks_out_23_page3")-> raw_read

raw_read %>%
  html_nodes(".stacks_right a") %>%
  html_text(trim = T) -> temp_title

raw_read %>%
  html_nodes("strong") %>%
  html_text(trim = T) -> temp_date_start

temp_date_start = gsub("UHR", "", temp_date_start)

raw_read %>%
  html_nodes(".stacks_right span") %>%
  html_text(trim = T) -> temp_description

title = c()
date_start = c()
time_start = c()
description = c()

for (n in 1:length(temp_date_start)){
  temp_date = str_extract_all(temp_date_start[n], "[0-9]+\\.[0-9]{2}", simplify = T)
  temp_time = str_extract_all(temp_date_start[n], "[0-9]+:[0-9]+", simplify = T)
  # add date, time and corresponding title
  for (i in 1:length(temp_date)){
    date_start = c(date_start, temp_date[i])
    time_start = c(time_start, temp_time[i])
    title = c(title, temp_title[n])
    description = c(description, temp_description[n])
  }
}

date_start = paste(date_start, format(Sys.Date(), "%Y"), sep = ".")
time_start = paste(time_start, ":00", sep = "")

# fixed data setup
category = rep("Bühne", length(title))
date_end = rep(NA, length(title))
time_end = rep(NA, length(title))
organizer = "Plastisches Theater Hobbit"
lat = rep(49.78884, length(title))
lng = rep(9.93252, length(title))
street = rep("Münzstraße 1", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))
link = rep("http://www.theater-hobbit.de/", length(title))
image_url = rep(NA, length(title))
price = rep(NA, length(title))
advanced_price = rep(NA, length(title))

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
