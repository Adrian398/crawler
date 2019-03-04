##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Staatlicher Hofkeller Würzburg ####
# crawl data
url <- "https://shop.hofkeller.de/veranstaltungen"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".product-name a") %>%
  html_text(trim = T) -> raw_title

raw_read %>%
  html_nodes(".std") %>%
  html_text(trim = T) -> description

title = str_extract(raw_title, "[[:alpha:]].*")

date_start = str_extract(raw_title, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")

time_start = str_extract_all(description, "[0-9]{2}\\.[0-9]{2}", simplify = T)[,1]
time_end = str_extract_all(description, "[0-9]{2}\\.[0-9]{2}", simplify = T)[,2]

time_start = paste(gsub("\\.", ":", time_start) , ":00", sep = "")
time_start = gsub("^:00", "", time_start)

time_end = paste(gsub("\\.", ":", time_end) , ":00", sep = "")
time_end = gsub("^:00", "", time_end)

raw_read %>%
  html_nodes(".price") %>%
  html_text(trim = T) -> price

raw_read %>%
  html_nodes(".link-learn") %>%
  html_attr("href") -> link

# fixed data setup
organizer = "Staatlicher Hofkeller Würzburg"
url = "https://www.hofkeller.de/"
category= rep("Rund um den Wein", length(title))
lat = rep(49.793936, length(title))
lng = rep(9.9361123, length(title))
street = rep("Residenzplatz 3", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
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

meta_df = data.frame(url_crawler = url
                     , organizer = organizer)


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
