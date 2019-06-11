##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: your name


### Website name ####
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
url <- "https://www.ukw.de/chirurgie-i/veranstaltungen/"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".title") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".date1") %>%
  html_text(trim = T) -> date_start

raw_read %>%
  html_nodes(".time") %>%
  html_text(trim = T) -> time

raw_read %>%
  html_nodes(".list-content-right > .text") %>%
  html_text(trim = T) -> description

raw_read %>%
  html_nodes(".print a") %>%
  html_attr("href") -> link

link = paste("https://www.ukw.de", link, sep = "")

time_start = str_extract(time, "[0-9]{2}:[0-9]{2}")

# fixed data setup
organizer = "Universitätsklinikum Würzburg - Zentrum für Operative Medizin (ZOM)"
url = "https://www.ukw.de/chirurgie-i/startseite/"
category= rep("Vortrag / Seminar", length(title))
lat = rep(49.8058689, length(title))
lng = rep(9.9570689, length(title))
street = rep("Oberdürrbacher Straße 6", length(title))
zip = rep(97080, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")

time_start = paste(time_start, ":00", sep = "")
time_start <- chron(times = time_start)
time_end = NA
date_end = as.Date(NA, "%d.%m.%Y")

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
                    city = city)

#add metadf idlocation
idlocation = 403030
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
