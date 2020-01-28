##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Improvement ###
# 1. price avaliable but not included
# responsible: Wei


### Frankenwarte ###
url = "https://www.frankenwarte.de/unser-bildungsangebot/aktuell.html"

url %>% 
  read_html() %>% 
  html_nodes(".intern") %>% 
  html_attr('href') %>% 
  unique() -> link

link = paste("https://www.frankenwarte.de/", link, sep = "")

title = c()
description = c()
date_start = c()
date_end = c()

for (n in link) {
  n %>%
    read_html() -> raw_read
   
  raw_read %>%
    html_nodes("#main h1") %>%
    html_text(trim = T) -> temp_title
  
  raw_read %>%
    html_nodes("h1+ p") %>%
    html_text(trim = T) -> temp_description
  
  raw_read %>%
    html_nodes(".va-termin h1") %>%
    html_text(trim = T) -> temp_raw_date
  
  # data mutation
  temp_date_start = str_extract(temp_raw_date, "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}|^[0-9]{2}\\.[0-9]{2}")
  
  if (length(temp_date_start) > 5) {
    # single day event check
    temp_date_end = NA
  } else {
    temp_date_end = str_extract(temp_raw_date, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")
    temp_date_start = paste(temp_date_start, ".",str_extract(temp_raw_date, "[0-9]{4}"), sep = "")
  }
  
  # date merge
  title = c(title, temp_title)
  description = c(description, temp_description)
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
}

# data type conversion
date_start = as.Date(date_start, "%d.%m.%Y")
date_end = as.Date(date_end, "%d.%m.%Y")

# Fixed data setup
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
lat = rep(49.78187, length(title))
lng = rep(9.9063928, length(title))
organizer = rep("Akademie Frankenwarte", length(title))
street = rep("Leutfresserweg 81 - 83", length(title))
zip = rep(97082, length(title))
city = rep("Würzburg", length(title))

# Build table
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

#set up to write to database
idlocation = 299
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
meta_df = df[c("organizer", "link")][1,]
names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'
meta_df["idlocation"] = idlocation

meta_df["idcrawler"] = 0
meta_df["id_category"] = 10586
#crawled_df[1:15,]
#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
