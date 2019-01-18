library(tidyverse)
library(rvest)
library(lubridate)
library(chron)
library(tibble)
library("DBI")
library(mongolite)
library(dbplyr)
source("write_to_database.R")

url_immerhin <- "http://www.immerhin-wuerzburg.de/"
nodes_immerhin1 <- "section:nth-child(1) li a"
nodes_immerhin2 <- "section:nth-child(1) b"

raw_crawler <- function(url, nodes1, nodes2) {
  read_html(url) -> site
  site %>%
    html_nodes(nodes1) %>%
    html_text(trim = TRUE) -> date
  
  date = date[date!=""]
  
  site %>%
    html_nodes(nodes2) %>%
    html_text(trim = TRUE) -> description
  
  return(data.frame(date=date, description=description))
} 


#Raw Data
raw_immerhin <- raw_crawler(url_immerhin, nodes_immerhin1, nodes_immerhin2)

#Dataframe erstellen

raw_immerhin %>%
  filter(description != "Geschlossen") -> raw_immerhin

raw_immerhin %>%
  separate(date,into=c("day", "date_start", "type"), sep=" ") %>%
  mutate(description = paste (type, description)) %>%
  select(-type, -day) %>%
  mutate(url = url_immerhin) %>%
  mutate(lng = "9.93173") %>%
  mutate(lat = "49.80186") %>%
  mutate(city = "Wuerzburg") %>%
  mutate(street = "Bahnhofplatz 2 (Posthalle)") %>% 
  mutate(zip = "97080") %>%
  mutate("date_end" = date_start) %>%
  mutate("time_start" = NA) %>%
  mutate("time_end" = NA) %>%
  mutate(price = NA) %>%
  mutate(organizer = "Immerhin") %>%
  mutate(date_start = parse_date_time(date_start, "d!.m!.y!")) %>%
  mutate(date_end = parse_date_time(date_end, "d!.m!.y!")) %>%
  mutate(lng <- as.numeric(lng),
         lat <- as.numeric(lat),
         zip <- as.numeric(zip),
         time_start <- times(time_start),
         time_end <- times(time_end),
         price <- as.character(price),
         date_start <- as.Date(date_start),
         date_end <- as.Date(date_end)) -> tidy_immerhin

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
dbListTables(conn)

#title
#description
#url
#date_start
#date_end
#time_start
#time_end
#idgroup
#idevent_location
#advance_sale_price
#sale_price
#event_site
#image
#category


###meta dataframe
#organizer

#house_number
#street
#city
#zip
#lng
#lat
#booking_office


####################
tidy_immerhin$title = tidy_immerhin$description
###one meta dataframe####
url_crawler = url_immerhin
organizer = "Immerhin"
# city = "Würzburg"
# street = "Bahnhofplatz"
# house_number = "2"
# lng = "9.93173"
# lat = "49.80186"
# zip = "97080"
meta_df = data.frame(url_crawler, organizer)
meta_df

crawled_df = tidy_immerhin
colnames(crawled_df)[3] = "link"
#extract organizer
crawled_df = crawled_df[-13]

#Posix to Date
as.Date(crawled_df["date_start"])
crawled_df["date_start"] = as.Date(c(sapply(crawled_df["date_start"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
crawled_df["date_end"] = as.Date(c(sapply(crawled_df["date_end"], as.character)), tz =  "UTC", format = "%Y-%m-%d")


###test use function
crawled_df[c(1,3),]
crawled_df[1:2,]

meta_df
write_dataframes_to_database(crawled_df[1:2,], meta_df, conn)

as.data.frame(tbl(conn, "event") %>%
    filter(idcrawler==1))

dbReadTable(conn, name ="event")

###test use function


### write to crawler table

db_url_crawlers <- as.data.frame(tbl(conn, "crawler") %>%
  select(url_crawler))
## if url_crawler doesnt exist write url_crawler in database
if(!any(db_url_crawlers==as.character(meta_df["url_crawler"][1,1]))){
  dbWriteTable(conn, value = meta_df["url_crawler"], name = "crawler", append = TRUE, row.names=F )
}

### write to organizer table
db_organizer_names <- as.data.frame(tbl(conn, "organizer") %>%
  select(organizer))
## if organizer doesnt exist write him in database
if(!any(db_organizer_names==as.character(meta_df["organizer"][1,1]))){
  dbWriteTable(conn, value = meta_df["organizer"], name = "organizer", append = TRUE, row.names=F )
}
  

### write to event_location table  
meta_df
df_event_location = meta_df[c("house_number",
  "street",
  "city",
  "zip",
  "lng",
  "lat")]
db_event_location_lng_lat <- as.data.frame(tbl(conn, "event_location") %>%
  select(lng, lat))

## if event_location doesnt exist write it in database
if(!(any(db_event_location_lng_lat["lng"]==as.character(meta_df["lng"][1,1])) & any(db_event_location_lng_lat["lat"]==as.character(meta_df["lat"][1,1])) )){
  dbWriteTable(conn, value = df_event_location, name = "event_location", append = TRUE, row.names=F )
}

### write to event table
## add ids for crawler, organizer, event_location
# add crawler foreignkey
idcrawler = tbl(conn, "crawler") %>%
  filter(url_crawler == as.character(meta_df["url_crawler"][1,1]))%>%
  select(idcrawler)
idcrawler = rep.int(as.integer(as.data.frame(idcrawler)), nrow(crawled_df))
crawled_df = cbind(crawled_df,idcrawler)

# add organizer foreignkey
idorganizer = tbl(conn, "organizer") %>%
  filter(organizer == as.character(meta_df["organizer"][1,1]))%>%
  select(idorganizer)
idorganizer = rep.int(as.integer(as.data.frame(idorganizer)), nrow(crawled_df))
crawled_df = cbind(crawled_df,idorganizer)

# add event_location foreignkey
idevent_location = tbl(conn, "event_location") %>%
  filter(lng == as.character(meta_df["lng"][1,1]) & lat == as.character(meta_df["lat"][1,1]))%>%
  select(idevent_location)

idevent_location = rep.int(as.integer(as.data.frame(idevent_location)), nrow(crawled_df))
crawled_df = cbind(crawled_df,idevent_location)
crawled_df

## if event doesnt exist write it in database
#get all events of the current crawler
db_events_current_crawler = as.data.frame(tbl(conn, "event") %>%
  filter(idcrawler == idcrawler[1]))
#compare all the events of current crawler in the database with the new events of the current crawler
#parse date and time
db_events_current_crawler["date_start"] = as.POSIXct(c(sapply(db_events_current_crawler["date_start"], as.character)), "UTC")
db_events_current_crawler["date_end"] = as.POSIXct(c(sapply(db_events_current_crawler["date_end"], as.character)), "UTC")

#teststart
crawled_df_test = crawled_df[1:5,]
#testend

new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "url","date_start", "date_end"))
if (nrow(new_events) != 0) {
  dbWriteTable(conn, value = new_events, name = "event", append = TRUE, row.names=F )
}
events_to_delete = anti_join(db_events_current_crawler, crawled_df,by=c("title", "url","date_start", "date_end"))
if (nrow(events_to_delete) != 0) {
  #create sql delete string
  sql_charaacter_ids_comma_separated_for_query = paste(as.character(events_to_delete$idevent), collapse=",")
  sql_delete_query = paste0('DELETE FROM event WHERE idevent IN (',sql_charaacter_ids_comma_separated_for_query,')')
  sql_delete_query
  #delete ids from database
  dbSendQuery(conn, sql_delete_query)
}
#idorganizer = c(1,2)

dbReadTable(conn, name ="event_location")

# Closes all open connections
dbDisconnect(conn)



lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)




### saved old connection: a1.cj8zdbsk8kip.eu-central-1.rds.amazonaws.com
#organizer und veranstaltungsorte notieren.
## check how to do the missing date values

## change url to link




