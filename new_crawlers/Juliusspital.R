##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. end date and time uncrawlable because of unorganized website struture
# responsible: Wei


### Zehntscheune, Juliusspital ####
# crawl data
url = "https://www.juliusspital-palliativakademie.de/programmkalender/index.html"

raw_read = read_html(url)

raw_read %>%
  html_nodes("#eventlist a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".evstart") %>%
  html_text(trim = T) -> date_start

raw_read %>%
  html_nodes("#eventlist a") %>%
  html_attr("href") -> link

link = paste("https://www.juliusspital-palliativakademie.de", link, sep = "")

description = c()
for (cache_link in link) {
  
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes(".eventfurther") %>%
    html_text(trim = T) -> cache_description
  
  description = c(description, cache_description)
}

# fixed data setup
organizer = "Zehntscheune, Juliusspital"
url = "https://www.juliusspital.de/"
category= rep("Vortrag / Seminar", length(title))
lat = rep(49.79853, length(title))
lng = rep(9.93207, length(title))
street = rep("Klinikstraße 1", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

meta_df = data.frame(url = url
                     , organizer = organizer)
