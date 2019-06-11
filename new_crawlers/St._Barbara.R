##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. only first page of events can be crawled
# responsible: Wei


### St. Barbara ####
# crawl data
url = "https://www.kirche-frauenland.de/seiten/index.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_attr("href") -> link

link = paste("https:", link, sep = "")

description = c()
date_start = c()
date_end = c()
time_start = c()
time_end = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes(".datetime") %>%
    html_text(trim = T) -> raw_date
  
  extract_date = str_extract_all(raw_date[2], "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", simplify = T)
  cache_date_start = extract_date[,1]
  if (length(extract_date) > 1) {
    cache_date_end = extract_date[,2]
  } else {
    cache_date_end = ""
  }
  
  extract_time = str_extract_all(raw_date[2], "[0-9]+:[0-9]+", simplify = T)
  if (length(extract_time) > 0) {
    cache_time_start = extract_time[,1]
    # adjust format to hh:mm
    if (nchar(cache_time_start) < 4) {
      cache_time_start = paste("0", cache_time_start, sep = "")
    }
  } else {
    cache_time_start = ""
  }
  if (length(extract_time) > 1) {
    cache_time_end = extract_time[,2]
    # adjust format to hh:mm
    if (nchar(cache_time_end) < 4) {
      cache_time_end = paste("0", cache_time_end, sep = "")
    }
  } else {
    cache_time_end = ""
  }
  
  cache_raw_read %>%
    html_nodes("#col3_content1 p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  description = c(description, cache_description)
  date_start = c(date_start, cache_date_start)
  date_end = c(date_end, cache_date_end)
  time_start = c(time_start, cache_time_start)
  time_end = c(time_end, cache_time_end)
  
}

# fixed data setup
organizer = "St. Barbara"
url = "https://www.kirche-frauenland.de/seiten/index.html"
category= rep("Religion", length(title))
lat = rep(49.79574, length(title))
lng = rep(9.95795, length(title))
street = rep("Barbarastraße 44", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))
time_end = NA
time_start <- chron(times = time_start)

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
idlocation = 11046
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

