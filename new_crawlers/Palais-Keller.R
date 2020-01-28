##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Palais-Keller (im Würtzburg-Palais) ####
# crawl data
url = "https://www.bayerischerhof.de/de/erleben-geniessen/eventkalender.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".listViewListContainer a") %>%
  html_attr("href") -> link

# remove ticket shop link
link = link[link!="http://www.astor-cinemalounge.de/tickets-kaufen.html"]
link = link[link!="https://www.astor-cinemalounge.de/tickets-kaufen.html"]
link = link[link!="https://www.astor-cinemalounge.de/filmprogramm.html"]


link = paste("https://www.bayerischerhof.de/", link, sep = "")

title = c()
description = c()
date_start = c()
date_end = c()
time_start = c()
time_end = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes(".fs50px") %>%
    html_text(trim = T) -> cache_title
  
  cache_raw_read %>%
    html_nodes(".content p") %>%
    html_text(trim = T) -> raw_date
  
  cache_raw_read %>%
    html_nodes(".contentMain") %>%
    html_text(trim = T) -> cache_description
  
  date_extract = str_extract_all(raw_date, "[0-9]{2}\\.[0-9]{2}\\.", simplify = T)
  cache_date_start = date_extract[,1]
  if (length(date_extract) > 1){
    cache_date_end = date_extract[,2]
  } else {
    cache_date_end = ""
  }
  
  time_extract = str_extract_all(raw_date, "[0-9]{2}:[0-9]{2}", simplify = T)
  cache_time_start = time_extract[,1]
  if (length(time_extract) > 1){
    cache_time_end = time_extract[,2]
  } else {
    cache_time_end = ""
  }
  cache_time_start = cache_time_start[1]
  cache_time_end = cache_time_end[1]
  cache_date_start = cache_date_start[1]
  cache_date_end = cache_date_end[1]
  
  title = c(title, cache_title)
  description = c(description, cache_description)
  date_start = c(date_start, cache_date_start)
  date_end = c(date_end, cache_date_end)
  time_start = c(time_start, cache_time_start)
  time_end = c(time_end, cache_time_end)
  
}

# fixed data setup
organizer = "Palais-Keller (im Würtzburg-Palais)"
url = "https://www.bayerischerhof.de/de.html"
lat = 49.7479214
lng = 9.8594372
street = "Am Congress Centrum"
zip = 97070
city = "Würzburg"

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start = paste(time_start, ":00", sep = "")
time_end = paste(time_end, ":00", sep = "")
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
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

#add metadf idlocation
idlocation = 11445
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 15
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
