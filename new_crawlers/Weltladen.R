##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Weltladen ####
# crawl data
url <- "https://www.weltladen-wuerzburg.de/termine"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".datetime") %>%
  html_text(trim = T) -> date

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_attr("href") -> link

date_start = str_extract_all(date, "[0-9]+\\.[0-9]+\\.[0-9]{4}", simplify = T)[, 1] 
date_end = NA

time_start = str_extract_all(date, "[0-9]{2}:[0-9]{2}", simplify = T)[, 1]
time_end = NA

time_start = paste(time_start, ":00", sep = "")
#time_end = paste(time_end, ":00", sep = "")
#time_end = gsub("^:00", "", time_end)

link = paste("https:", link, sep = "")

raw_read %>%
  html_nodes(".itemcontent") %>%
  html_text(trim = T)-> description
description = gsub("\r\n\t\t\t\t\t\n\t\t\t\t\t\tMEHR", "", description)

#image_url = c()
#for (cache_link in link) {

  #cache_read = read_html(cache_link)
  
  #cache_read %>%
    #html_nodes(".subtitle") %>%
    #html_text(trim = T) -> cache_description
  
  #cache_read %>%
    #html_nodes(".imagethumb") %>%
    #html_attr("src") -> cache_image_url
  
  #image_url = c(image_url, cache_image_url[1])
  #description = c(description, cache_description)
#}

# fixed data setup
organizer = "Weltladen"
url = "https://www.weltladen-wuerzburg.de/"
category= rep("Ausstellung / Messe", length(title))
lat = rep(49.7924792, length(title))
lng = rep(9.9312072, length(title))
street = rep("Plattnerstraße 14", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron::chron(times = time_start)
time_end <- NA

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
idlocation = 403026
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 35
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
