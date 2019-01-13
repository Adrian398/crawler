##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Improvement ###
# 1. price avaliable but not included
# responsible: Wei


### Frankenwarte ###
url = "https://www.frankenwarte.de/unser-bildungsangebot/aktuell.html"

url %>% 
  read_html() %>% 
  html_nodes(".intern") %>% 
  html_attr('href') %>% 
  unique() -> link

link = paste("https://www.frankenwarte.de/", link, sep = "")

title = c()
description = c()
date_start = c()
date_end = c()

for (n in link) {
  n %>%
    read_html() -> raw_read
  
  raw_read %>%
    html_nodes("#main h1") %>%
    html_text(trim = T) -> temp_title
  
  raw_read %>%
    html_nodes("h1+ p") %>%
    html_text(trim = T) -> temp_description
  
  raw_read %>%
    html_nodes(".va-termin h1") %>%
    html_text(trim = T) -> temp_raw_date
  
  # data mutation
  temp_date_start = str_extract(temp_raw_date, "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}|^[0-9]{2}\\.[0-9]{2}")
  
  if (length(temp_date_start) > 5) {
    # single day event check
    temp_date_end = NA
  } else {
    temp_date_end = str_extract(temp_raw_date, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")
    temp_date_start = paste(temp_date_start, ".",str_extract(temp_raw_date, "[0-9]{4}"), sep = "")
  }
  
  # date merge
  title = c(title, temp_title)
  description = c(description, temp_description)
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
}

# data type conversion
date_start = as.Date(date_start, "%d.%m.%Y")
date_end = as.Date(date_end, "%d.%m.%Y")

# Fixed data setup
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
lat = rep(49.78187, length(title))
lng = rep(9.9063928, length(title))
organizer = rep("Akademie Frankenwarte", length(title))
street = rep("Leutfresserweg 81 - 83", length(title))
zip = rep(97082, length(title))
city = rep("Würzburg", length(title))
house_number = rep(81, length(title))

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
                 link = link,
                 house_number)



#title req
#description req
#url req 
#date_start  req 
#date_end req 
#time_start req 00:00:00 if Na
#time_end req as 00:00:00 if Na

#price not req


##meta dataframe
#organizer req

#url_crawler req

#house_number req
#street req
#city req
#zip req 
#lng req
#lat req 
#booking_office not req

getSqlConnection <- function(){
  con <-dbConnect(
    RMySQL::MySQL(),
    username = 'crawler',
    password = 'crawler2018',
    host = 'a1.cyawe3clu0j3.eu-west-1.rds.amazonaws.com',
    port = 3306,
    dbname = 'eventscalender'
  )
  return(con)
}
conn <- getSqlConnection()

crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start")]
meta_df = df[c("organizer","house_number", "street", "city", "zip", "lng", "lat")][1,]
meta_df$url_crawler = url

names(crawled_df)[names(crawled_df) == 'link'] <- 'url'


meta_df
crawled_df
write_dataframes_to_database(crawled_df, meta_df, conn)
meta_df["url_crawler"]

dbReadTable(conn, name ="event")



###test


db_events_current_crawler = as.data.frame(tbl(conn, "event") %>%
                                            filter(idcrawler == 1))

db_events_current_crawler
#compare all the events of current crawler in the database with the new events of the current crawler
#parse date and time
db_events_current_crawler["date_start"] = as.POSIXct(c(sapply(db_events_current_crawler["date_start"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
db_events_current_crawler["date_end"] = as.POSIXct(c(sapply(db_events_current_crawler["date_end"], as.character)), tz = "UTC", format = "%Y-%m-%d")




db_events_current_crawler = as.data.frame(tbl(conn, "event") %>%
                                            filter(idcrawler == 3))
db_events_current_crawler

db_events_current_crawler["date_start"] = as.POSIXct(c(sapply(db_events_current_crawler["date_start"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
db_events_current_crawler["date_end"] = as.POSIXct(c(sapply(db_events_current_crawler["date_end"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
crawled_df

as.Date(db_events_current_crawler["date_start"][1,1]
crawled_df["date_start"][1,1]

new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "url","date_start", "date_end"))


