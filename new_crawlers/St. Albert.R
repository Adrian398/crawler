##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. descriptions not crawlable because of unorganized website structure
# responsible: Wei


### websiteName ####
# crawl data
url = "http://lindleinsmuehle.pg-albert-jakobus.de/index.html#page1"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".datetime") %>%
  html_text(trim = T) -> raw_date

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_attr("href") -> link

link = paste("http:", link, sep = "")

date_start = str_extract(raw_date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
date_end = str_extract_all(raw_date, "[0-9]+\\.[0-9]+\\.[0-9]{4}", simplify = T)[,2]

time_start = str_extract_all(raw_date, "[0-9]+:[0-9]+", simplify = T)[,1]
time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))

# fixed data setup
organizer = "St. Albert"
url = "http://lindleinsmuehle.pg-albert-jakobus.de/index.html"
category= rep("Religion", length(title))
lat = rep(49.8084818, length(title))
lng = rep(9.9634205, length(title))
street = rep("Frankenstraße 21", length(title))
zip = rep(97078, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end = NA
description = NA

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

#add metadf idlocation
idlocation = 12710
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 22
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)