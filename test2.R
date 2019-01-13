##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Improvement ###
# 1. only crawl 7 events without loading more
# responsible: Wei


### Universitätsklinikum Würzburg ####
# crawl data
url <- "https://www.ukw.de/patienten-besucher/veranstaltungskalender/"

url %>%
  read_html() %>%
  html_nodes(".list-content-right")-> raw_read

raw_read %>%
  html_nodes(".title") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".tags") %>%
  html_text(trim = T) %>%
  str_remove_all("\r|\n|\t") -> basic_info


str_extract(basic_info, "^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}|^[0-9]{2}\\.[0-9]{2}") -> date_start

gsub("-\\s", "", 
     str_extract(basic_info, "-\\s[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")) -> date_end

str_extract(basic_info, "[0-9]{2}:[0-9]{2}") -> time_start

gsub("-\\s", "",
     str_extract(basic_info, "-\\s[0-9]{2}:[0-9]{2}")) -> time_end

raw_read %>%
  html_nodes(".text") %>%
  html_text(trim = T) -> description

# Fixed data setup
link = rep("https://www.ukw.de/patienten-besucher/veranstaltungskalender/", length(title))
lat = rep(49.8007, length(title))
lng = rep(9.95373, length(title))
organizer = rep("Universitätsklinikum Würzburg", length(title))
street = rep("Josef-Schneider-Straße", length(title))
zip = rep(97080, length(title))
city = rep("Würzburg", length(title))
house_number = rep(2, length(title))

# Data mutation
for (n in 1:length(date_start)){
  if (is.na(str_extract(date_start[n], "[0-9]{4}"))){
    date_start[n] = paste(date_start[n], str_extract(date_end[n], "[0-9]{4}"), sep = ".")
  }
  if (!is.na(time_start[n])){
    time_start[n] = paste(time_start[n], ":00", sep = "")
  }
  if (!is.na(time_end[n])){
    time_end[n] = paste(time_end[n], ":00", sep = "")
  }
}

date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")


time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

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
meta_df = df[c("organizer", "link","house_number", "street", "city", "zip", "lng", "lat")][1,]


names(crawled_df)[names(crawled_df) == 'link'] <- 'url'
names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'

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


