##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
library(gsubfn)
source("write_to_database.R")

### Possible Improvements ###
# none
# responsible: Wei

#### Mariannhill ####
# function initializaiton #
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
url <-"http://www.kirchenmusik-mariannhill.de/programm.html"

url %>%
  read_html() %>%
  html_nodes(".texteng")-> raw_read

raw_read %>%
  html_nodes("span:nth-child(1)") %>%
  html_text(trim = T) -> raw_date

raw_date = unique(raw_date[raw_date != ""])

date_start = month_convertor(str_extract(raw_date, "[0-9]+\\.\\s[[:alpha:]]+\\s[0-9]{4}"))

time_start = paste(gsub("\\.", ":", str_extract(raw_date, "[0-9]{2}\\.[0-9]{2}")), ":00", sep = "") 

raw_read %>%
  html_nodes("span:nth-child(3)") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes("span:nth-child(7)") %>%
  html_text(trim = T) -> description

# Fixed data setup
date_end = rep(NA, length(title))
time_end = rep(NA, length(title))
organizer = rep("Kirchenmusik Mariannhill Würzburg", length(title))
lat = rep(49.79348, length(title))
lng = rep(9.9545, length(title))
street = rep("Mariannhillstraße 1", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))
link = rep("http://www.kirchenmusik-mariannhill.de", length(title))

# data type conversion
date_start = as.Date(date_start, "%d.%m.%Y")
date_end = as.Date(date_end, "%d.%m.%Y")

time_start = chron(times = time_start)
time_end = chron(times = time_end)

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
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
meta_df = df[c("organizer", "link")][1,]
names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)