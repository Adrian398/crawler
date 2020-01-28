##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. search for events in upcoming 2 months
# responsible: Wei


### Bürgerspital Betriebshof am Pfaffenberg ####
# crawl data
base_url = "https://www.buergerspital.de/aktuelles/termine/index.html?ev%5Bsearch%5D=&ev%5Bcat%5D=&ev%5Bstart%5D="

current_month = paste(format(Sys.Date(),"%m"), ".", sep = "")
current_day =  paste(format(Sys.Date(),"%d"), ".", sep = "")
current_year = format(Sys.Date(),"%Y")

until_month = paste(format(Sys.Date()+60,"%m"), ".", sep = "")
until_day = paste(format(Sys.Date()+60,"%d"), ".", sep = "")
until_year = format(Sys.Date()+60,"%Y")

url = paste(base_url, current_day, current_month, current_year, "&ev%5Bend%5D="
            , until_day, until_month, until_year, "&ev%5Bsubcat%5D=", sep = "")

raw_read = read_html(url)

raw_read %>%
  html_nodes(".title a") %>%
  html_attr("href") -> link

link = link[-((length(link)-1):length(link))]
link = paste("https://www.buergerspital.de", link, sep = "")

title = c()
description = c()
date_start = c()
time_start = c()
time_end = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes(".artikelansicht_classic div b") %>%
    html_text(trim = T) -> cache_title
  
  cache_raw_read %>%
    html_nodes("td") %>%
    html_text(trim = T) -> cache_content
  
  cache_date_start = str_extract(cache_content, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")
  cache_time_start = str_extract_all(cache_content, "[0-9]{2}:[0-9]{2}", simplify = T)[, 1]
  cache_time_end = str_extract_all(cache_content, "[0-9]{2}:[0-9]{2}", simplify = T)[, 2]
  
  cache_raw_read %>%
    html_nodes(".artikelansicht_classic p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  title = c(title, cache_title)
  description = c(description, cache_description)
  date_start = c(date_start, cache_date_start)
  time_start = c(time_start, cache_time_start)
  time_end = c(time_end, cache_time_end)
  
}


# fixed data setup
organizer = "Bürgerspital Betriebshof am Pfaffenberg"
url = "https://www.buergerspital.de/"
lat = rep(49.7953775, length(title))
lng = rep(9.9354671, length(title))
street = rep("Theaterstraße 19", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = paste(time_start, ":00", sep = "")
time_end = paste(time_end, ":00", sep = "")
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
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

#add metadf idlocation
idlocation = 11586
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

meta_df["idcrawler"] = 3
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
