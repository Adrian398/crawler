##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. only crawl first 9 pages of events
# responsible: Wei


### Katholische Akademie Domschule ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub("Januar","01",given_date)
  given_date = gsub("Februar","02",given_date)
  given_date = gsub("März","03",given_date)
  given_date = gsub("April","04",given_date)
  given_date = gsub("Mai","05",given_date)
  given_date = gsub("Juni","06",given_date)
  given_date = gsub("Juli","07",given_date)
  given_date = gsub("August","08",given_date)
  given_date = gsub("September","09",given_date)
  given_date = gsub("Oktober","10",given_date)
  given_date = gsub("November","11",given_date)
  given_date = gsub("Dezember","12",given_date)
  return(given_date)
}

# crawl data
url = "https://www.domschule-wuerzburg.de/aktuell/index.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".selectpage") %>%
  html_attr("href") -> urls

urls = paste("https://www.domschule-wuerzburg.de", urls[2:length(urls)], sep = "")
urls = c(url, urls)

title = c()
link = c()
date_start = c()
time_start = c()
time_end = c()
description = c()
image_url = c()
price = c()
street = c()

for (cache_url in urls) {
  cache_raw_read = read_html(cache_url)
  
  cache_raw_read %>%
    html_nodes(".eventcontent .title a") %>%
    html_text(trim = T) -> cache_title
  
  cache_raw_read %>%
    html_nodes(".eventcontent .title a") %>%
    html_attr("href") -> cache_link
  
  cache_link = paste("https:",cache_link, sep = "")
  
  title = c(title, cache_title)
  link = c(link, cache_link)
  
  for (temp_link in cache_link) {
    cache_raw_read = read_html(temp_link)
    
    cache_raw_read %>%
      html_nodes(".blog-info li:nth-child(1)") %>%
      html_text(trim = T) -> raw_date
    
    cache_date_start = str_extract(raw_date, "[0-9]+\\.\\s[[:alpha:]]+\\s[0-9]{4}")
    cache_date_start = gsub("\\s", ".", gsub("\\.", "", cache_date_start))
    cache_date_start = month_convertor(cache_date_start)
    
    extract_time = str_extract_all(raw_date, "[0-9]{2}:[0-9]{2}", simplify = T)
    if (length(extract_time) > 0) {
      cache_time_start = extract_time[,1]
    } else {
      cache_time_start = ""
    }
    if (length(extract_time) > 1) {
      cache_time_end = extract_time[,2]
    } else {
      cache_time_end = ""
    }
    
    date_start = c(date_start, cache_date_start)
    time_start = c(time_start, cache_time_end)
    time_end = c(time_end, cache_time_end)
    
    cache_raw_read %>%
      html_nodes(".detailmetadata") %>%
      html_text(trim = T) -> cache_description
    
    description = c(description, cache_description)
    
    cache_raw_read %>%
      html_nodes(".carousel-inner a") %>%
      html_attr("href") -> cache_image_url
    
    if (length(cache_image_url) > 2){
      cache_image_url = paste("https://www.domschule-wuerzburg.de", cache_image_url[3], sep = "")
    } else {
      cache_image_url = ""
    }
    image_url = c(image_url, cache_image_url)
    
    cache_raw_read %>%
      html_nodes(".f_cost .f_value") %>%
      html_text(trim = T) -> cache_price
    
    price = c(price, cache_price)
    
    cache_raw_read %>%
      html_nodes(".f_location .f_value") %>%
      html_text(trim = T) -> cache_street
    
    if (length(cache_street)>0){
      street = c(street, cache_street)
    } else {
      street = c(street, "")
    }
    
  }
}

temp_link = "https://www.domschule-wuerzburg.de/aktuell/index.html/was-heisst-heimat-und-identitaet/81c244da-4c95-4cae-980d-312acaff35ee?mode=detail"


# fixed data setup
organizer = "Katholische Akademie Domschule"
url = "https://www.domschule-wuerzburg.de/index.html"

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = gsub("^:00", "", paste(time_start, ":00", sep = ""))
time_end = gsub("^:00", "", paste(time_end, ":00", sep = ""))
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
crawled_df <- data.frame(
                    title = title,
                    date_start = date_start,
                    time_start = time_start,
                    time_end = time_end,
                    price = price,
                    description = description,
                    street = street,
                    link = link,
                    image_url = image_url)

#add metadf idlocation
idlocation = 11035
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
