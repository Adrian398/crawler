
##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Bürgerspital Weingut ####
# crawl data
url <- "https://www.buergerspital.de/weingut/termine/index.html"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".events_list_middle .title a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".eventrowday b") %>%
  html_text(trim = T) -> date

date_start = str_extract(date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")

raw_read %>%
  html_nodes(".ev_datetime") %>%
  html_text(trim = T) -> time

time_start = str_extract(time, "[0-9]{2}:[0-9]{2}")
time_end = gsub("\\s", "", str_extract(time, "\\s[0-9]{2}:[0-9]{2}"))

raw_read %>%
  html_nodes(".events_list_middle .title a") %>%
  html_attr("href") %>%
  paste("https://www.buergerspital.de",., sep = "")-> link

description = c()
price = c()

for (cache_link in link) {
  
  cache_link %>%
    read_html() -> cache_read
  
  cache_read %>%
    html_nodes("td p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  cache_price = gsub("Preis:\\s", "", str_extract(cache_description, "Preis:\\s[0-9]+"))
  
  description = c(description, cache_description)
  price = c(price, cache_price)
}

# fixed data setup
organizer = "Bürgerspital Weingut"
url = "https://www.buergerspital.de/"
category= rep("Weinevent", length(title))
lat = rep(49.7957823, length(title))
lng = rep(9.9350524, length(title))
street = rep("Theaterstraße 19", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end = NA
date_end = as.Date(date_end, "%d.%m.%Y")

time_start = paste(time_start, ":00", sep = "")
time_start = chron(times = time_start)

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

#add metadf idlocation
idlocation = 403100
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'
meta_df["idcrawler"] = 4
meta_df["id_category"] = 10586


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
