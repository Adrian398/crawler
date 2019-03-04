##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: wei


### Theater Ensemble Würzburg ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub("Jan","01",given_date)
  given_date = gsub("Feb","02",given_date)
  given_date = gsub("Mär","03",given_date)
  given_date = gsub("Apr","04",given_date)
  given_date = gsub("Mai","05",given_date)
  given_date = gsub("Jun","06",given_date)
  given_date = gsub("Jul","07",given_date)
  given_date = gsub("August ","08",given_date)
  given_date = gsub("Sep","09",given_date)
  given_date = gsub("Okt","10",given_date)
  given_date = gsub("Nov","11",given_date)
  given_date = gsub("Dez","12",given_date)
  return(given_date)
}

# crawl data
url <- "http://theater-ensemble.net/spielplan/"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".title a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".date") %>%
  html_text(trim = T) -> date

date_start = str_extract(date, "[0-9]+\\.\\s[[:alpha:]]+")
date_start = gsub("\\s", "", date_start)
year = gsub("\\,\\s", "20", str_extract(date, "\\,\\s[0-9]+"))
date_start = paste(date_start, year, sep = ".")
date_start = month_convertor(date_start)

time_start = str_extract(date, "[0-9]{2}:[0-9]{2}")
time_start = paste(time_start, ":00", sep = "")
  
raw_read %>%
  html_nodes(".title a") %>%
  html_attr("href") -> link

raw_read %>%
  html_nodes(".meta") %>%
  html_text(trim = T) -> price

price = gsub(".*\n\t\t\t", "", price)

description = c()
image_url = c()

for (cache_link in link) {
  
  cache_link %>%
    read_html() -> cache_read
  
  cache_read %>%
    html_nodes(".content__inner.content__p p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  cache_read %>%
    html_nodes(".content__poster.content__inner img") %>%
    html_attr("src") -> cache_image_url
  
  if (is_empty(cache_description)){
    description = c(description, NA)
  } else {
    description = c(description, cache_description)
  }
  
  if (is_empty(cache_image_url)){
    image_url = c(image_url, NA)
  } else {
    image_url = c(image_url, cache_image_url)
  }
  
}

# fixed data setup
organizer = "Theater Ensemble Würzburg"
url = "http://theater-ensemble.net/"
category= rep("Bühne", length(title))
lat = rep(49.7937276, length(title))
lng = rep(9.9684822, length(title))
street = rep("Frankfurter Straße 87", length(title))
zip = rep(97082, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
time_start <- chron(times = time_start)

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    time_start = time_start,
                    price = price,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link,
                    image_url = image_url)

meta_df = data.frame(url_crawler = url
                     , organizer = organizer)


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
