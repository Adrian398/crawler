##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. no time start for indivitual event
# responsible: Wei

### Ökohaus ####
# crawl data
url = "https://wuerzburg.bund-naturschutz.de/veranstaltungen/teil/1.html"

raw_read = read_html(url)

raw_read %>%
  html_nodes(".f3-widget-paginator a") %>%
  html_text(trim = T) -> page_number

page_number = page_number[length(page_number)-1]

urls = c()
for (n in 1:page_number) {
  cache_url = paste("https://wuerzburg.bund-naturschutz.de/veranstaltungen/teil/", n, ".html", sep = "")
  urls = c(urls, cache_url)
}

title = c()
date_start = c()
category = c()
street = c()
link = c()
description = c()
time_start = c()
time_end = c()

for (cache_url in urls) {
  cache_raw_read = read_html(cache_url)
  
  cache_raw_read %>%
    html_nodes("td a") %>%
    html_text(trim = T) -> cache_title
  
  cache_raw_read %>%
    html_nodes("td:nth-child(1)") %>%
    html_text(trim = T) -> cache_category
  
  cache_raw_read %>%
    html_nodes("td:nth-child(3)") %>%
    html_text(trim = T) -> cache_street
  
  cache_raw_read %>%
    html_nodes("td:nth-child(4)") %>%
    html_text(trim = T) -> cache_date
  
  cache_date = str_extract(cache_date, "[0-9]+\\.[0-9]+\\.[0-9]{4}")
  
  cache_raw_read %>%
    html_nodes("td a") %>%
    html_attr("href") -> cache_link
  
  cache_link = paste("https://wuerzburg.bund-naturschutz.de", cache_link, sep = "")
  
  for (temp_link in cache_link) {
    temp_raw_read = read_html(temp_link)
    
    temp_raw_read %>%
      html_nodes(".border") %>%
      html_text(trim = T) -> temp_description
    
    if(length(str_extract_all(temp_description, "[0-9]{2}:[0-9]{2}", simplify = T)) > 0){
      cache_time_start = str_extract_all(temp_description, "[0-9]{2}:[0-9]{2}", simplify = T)[,1]  
    } else {
      cache_time_start = ""
    }
    if(length(str_extract_all(temp_description, "[0-9]{2}:[0-9]{2}", simplify = T)) > 1){
      cache_time_end = str_extract_all(temp_description, "[0-9]{2}:[0-9]{2}", simplify = T)[,2]  
    } else {
      cache_time_end = ""
    }
    
    time_start = c(time_start, cache_time_start)
    time_end = c(time_end, cache_time_end)
    description = c(description, temp_description)
  }
  
  temp_link = "https://wuerzburg.bund-naturschutz.de/veranstaltungen/termin/schimmelpilze-in-wohnraeumen-was-tun-4.html"
  
  title = c(title, cache_title)
  date_start = c(date_start, cache_date)
  category = c(category, cache_category)
  street = c(street, cache_street)
  link = c(link, cache_link)

}

# fixed data setup
organizer = "Ökohaus"
url = "https://wuerzburg.bund-naturschutz.de/"
lat = rep(49.79746, length(title))
lng = rep(9.9199, length(title))
zip = rep(97082, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))
time_end = gsub("^:00", "", paste(time_end, ":00", sep = ""))

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
                    category = category,
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
idlocation = 4589
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)