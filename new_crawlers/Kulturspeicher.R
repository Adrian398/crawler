##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. only crawl events on the event list
# responsible: Wei


### Kulturspeicher ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub(" Januar","01.",given_date)
  given_date = gsub(" Februar","02.",given_date)
  given_date = gsub(" März","03.",given_date)
  given_date = gsub(" April","04.",given_date)
  given_date = gsub(" Mai","05.",given_date)
  given_date = gsub(" Juni","06.",given_date)
  given_date = gsub(" Juli","07.",given_date)
  given_date = gsub(" August","08.",given_date)
  given_date = gsub(" September","09.",given_date)
  given_date = gsub(" Oktober","10.",given_date)
  given_date = gsub(" November","11.",given_date)
  given_date = gsub(" Dezember","12.",given_date)
  return(given_date)
}

# crawl data
url = "https://www.kulturspeicher.de/veranstaltungen/index.html"
raw_read = read_html(url)

raw_read %>%
  html_nodes("h1 a") %>%
  html_text(trim = T) -> raw_title

date_start = str_extract(raw_title, "[0-9]{2}\\.\\s[[:alpha:]]+")
date_start = month_convertor(date_start)
date_start = paste(date_start, format(Sys.Date(), "%Y"), sep = "")

time_start = str_extract(raw_title, "[0-9]{2}\\.[0-9]{2}")
time_start = gsub("\\.", ":", time_start)
time_start = paste(time_start, ":00", sep = "")

title = c()
# name filter
for (sub_title in raw_title) {
  cache_title = str_extract(sub_title, "r:.*")
  if (!is.na(cache_title)) {
    cache_title = gsub("r:\\s", "", cache_title)
  } else {
    cache_title = sub_title
  }
  title = c(title, cache_title)
}

raw_read %>%
  html_nodes("h1 a") %>%
  html_attr("href") -> link

link = paste("https://www.kulturspeicher.de", link, sep = "")

description = c()
price = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes("p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  cache_price = str_extract(cache_description, "[0-9]+\\.[0-9]{2}\\sEuro")
  
  description = c(description, cache_description)
  price = c(price, cache_price)
}

# fixed data setup
organizer = "Kulturspeicher"
url = "https://www.kulturspeicher.de/"
lat = rep(49.80243, length(title))
lng = rep(9.92119, length(title))
street = rep("Oskar-Laredo-Platz 1", length(title))
zip = rep(97080, length(title))
city = rep("Würzburg", length(title))

# data type conversion    
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start <- chron(times = time_start)

time_end = rep(NA, length(title))
# build table
crawled_df <- data.frame(
                    title = title,
                    date_start = date_start,
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
idlocation = 6393
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
