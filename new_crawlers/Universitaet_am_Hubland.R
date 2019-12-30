##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
library(BBmisc)
library(RCurl)

### Possible Improvements ###
# 1. none
# responsible: Wei

### Universität am Hubland ####
# crawl data

current_year = str_extract(Sys.Date(), "[0-9]{4}")
current_month = gsub("-", "", str_extract(Sys.Date(), "-[0-9]{2}"))

url = c()

for (n in current_month:12) {
  cache_url = paste("https://www.uni-wuerzburg.de/aktuelles/veranstaltungen/zeitraum", current_year, n, sep = "/")
  url = c(url, cache_url)  
}

title = c()
date_start = c()
date_end = c()
time_start = c()
time_end = c()
street = c()
category = c()
description = c()
image_url = c()
link = c()

for (cache_url in url) {
  
  cache_url %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes(".news-list__item-header a") %>%
    html_text(trim = T) -> cache_title
  
  raw_read %>%
    html_nodes(".news-list__item-header a") %>%
    html_attr('href') -> single_page_links
  
  raw_read %>%
    html_nodes(".news-list__item-date") %>%
    html_text(trim = T) -> date
  
  cache_date_start = str_extract(date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
  cache_date_end = gsub("\\s", "", str_extract(date, "\\s[0-9]+\\.[0-9]+\\.[0-9]{4}"))
  
  cache_time_start = paste(str_extract_all(date, "[0-9]+:[0-9]+", simplify = T)[,1], ":00", sep = "")
  cache_time_end = paste(str_extract_all(date, "[0-9]+:[0-9]+", simplify = T)[,2], ":00", sep = "")
  
  raw_read %>%
    html_nodes("tr:nth-child(1) .news-list__item-value") %>%
    html_text(trim = T) -> cache_category
  
  raw_read %>%
    html_nodes(".news-list__item") %>%
    html_text(trim = T) -> all
  
  # filter out street information
  cache_street = gsub("Veranstalter.*", "", str_extract(all, "Ort:(.*)"))
  cache_street = gsub("Vortragender.*", "", cache_street)
  cache_street = gsub("Ort:", "", cache_street)
  
  title = c(title, cache_title)
  date_start = c(date_start, cache_date_start)
  date_end = c(date_end, cache_date_end)
  time_start = c(time_start, cache_time_start)
  time_end = c(time_end, cache_time_end)
  street = c(street, cache_street)
  category = c(category, cache_category)
  
  # fix webiste address
  loop_links = c()
  for (cache_link in single_page_links) {
    if (!grepl("https://", cache_link)){
      fixed_url = paste("https://www.uni-wuerzburg.de", cache_link, sep = "")
      loop_links = c(loop_links, fixed_url)
    } else {
      loop_links = c(loop_links, cache_link)
    }
  }
  
  # add webiste address to link
  link = c(link, loop_links)
  
  # collect descriptions and image_url
  # for (cache_link in loop_links) {
  #   cache_link %>%
  #     read_html() -> cache_read
  #   
  #   cache_read %>%
  #     html_nodes(".intro") %>%
  #     html_text(trim = T) -> cache_intro
  #   
  #   cache_read %>%
  #     html_nodes(".news-single__item-content") %>%
  #     html_text(trim = T) -> cache_description
  #   
  #   if (!is_empty(cache_intro)){
  #     cache_description = paste(cache_intro, cache_description, sep = " ")
  #   }
  #   
  #   if (!is_empty(cache_description)){
  #     if (nchar(cache_description) > 0){
  #       description = c(description, cache_description)
  #     } else {
  #       description = c(description, NA)
  #     }
  #   } else {
  #     description = c(description, NA)
  #   }
  #   
  #   cache_read %>%
  #     html_nodes(".news-single__item-big-image a") %>%
  #     html_attr('href') -> cache_image_url
  #   
  #   if (is_empty(cache_image_url)){
  #     image_url = c(image_url, NA)
  #   } else {
  #     cache_image_url = paste("https://www.uni-wuerzburg.de", cache_image_url, sep = "")
  #     image_url = c(image_url, cache_image_url)
  #   }
  # }
}

# fixed data setup
organizer = "Universität am Hubland"
url_crawler = "https://www.uni-wuerzburg.de/aktuelles/veranstaltungen/"
lat = rep(49.7815829, length(title))
lng = rep(9.9707861, length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)
description = as.character(NA) #fix later with the commented code above
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
url = "https://www.uni-wuerzburg.de/aktuelles/veranstaltungen/"
idlocation = 4770
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

