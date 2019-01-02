##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. description chaotic
# responsible: Wei


### Salon 77  ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub(" Januar ","01.",given_date)
  given_date = gsub(" Februar ","02.",given_date)
  given_date = gsub(" März ","03.",given_date)
  given_date = gsub(" April ","04.",given_date)
  given_date = gsub(" Mai ","05.",given_date)
  given_date = gsub(" Juni ","06.",given_date)
  given_date = gsub(" Juli ","07.",given_date)
  given_date = gsub(" August ","08.",given_date)
  given_date = gsub(" September ","09.",given_date)
  given_date = gsub(" Oktober ","10.",given_date)
  given_date = gsub(" November ","11.",given_date)
  given_date = gsub(" Dezember ","12.",given_date)
  return(given_date)
}

# crawl data
url <- "https://www.salon77.de/index.php?nav=ve&mod=va&bc1=Veranstaltungen"

url %>%
  read_html() %>%
  html_nodes("#innercontent a") %>%
  html_attr("href") -> link

link = paste("https://www.salon77.de", gsub("\\s", "%20", link), sep = "")

title = c()
date_start = c()
date_end = c()
description = c()

for (temp_url in link){
  temp_url %>%
    read_html() %>%
    html_nodes("#content") -> raw_read
  
  raw_read %>%
    html_node("h1") %>%
    html_text(trim = T)-> temp_title
  
  raw_read %>%
    html_node("h2+ strong") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") -> temp_date_start
  
  raw_read %>%
    html_node("h2+ strong") %>%
    html_text(trim = T) %>%
    str_extract("-\\s[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") -> temp_date_end
  
  temp_date_end = gsub("-\\s", "", temp_date_end)
  
  raw_read %>%
    html_node("#ktml") %>%
    html_text(trim = T)-> temp_description
  
  title = c(title, temp_title)
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
  description = c(description, temp_description)
  
}

# fixed data setup
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
organizer = rep("Salon 77", length(title))
lat = rep(49.79304, length(title))
lng = rep(9.95808, length(title))
street = rep("Richard-Wagner-Straße 60", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
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
