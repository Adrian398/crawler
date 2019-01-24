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
                  link = link)



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
    dbname = 'eventscalender',
    encoding = "latin1"
  )
  return(con)
}
conn <- getSqlConnection()

crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
meta_df = df[c("organizer", "link")][1,]


names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'

meta_df
crawled_df$booking_office = rep("hi",7)
crawled_df$image_url = rep("hi",7)
crawled_df$category = rep("hi",7)
crawled_df$idgroup = seq(1,7)
  crawled_df$price[1] = rep(90.23,1)
crawled_df$advance_price = rep(34.23,7)

crawled_df
write_dataframes_to_database(crawled_df[1,], meta_df, conn)
write_dataframes_to_database(crawled_df[5:7,], meta_df, conn)
meta_df["url_crawler"]

dbReadTable(conn, name ="event")


as.data.frame(tbl(conn, "event") %>%
                          filter(idcrawler == 2))

Encoding(as.character(a["street"][1,1]))

a = crawled_df
b = crawled_df[1:2,]
anti_join(a,b, by = "time_end")
b
a

###test






#### tests uniklinikum convert utf8



crawled_df$street

dbReadTable(conn, name ="event")

crawled_df$city = as.character(crawled_df$city)
Encoding(crawled_df$city) <- "UTF-8"
crawled_df$city <- iconv(crawled_df$city, "latin1", "latin1")
Encoding(crawled_df$city)
crawled_df$city
c =data.frame(crawled_df$city)
Encoding(as.character(crawled_df$city))

crawled_df$description = as.character(crawled_df$description)

db_events_current_crawler = as.data.frame(tbl(conn, "event", stringsasfactor = FALSE) %>%
                                            filter(idcrawler == 1))



db_events_current_crawler$city <- iconv(db_events_current_crawler$city, "latin1", "UTF-8")
db_events_current_crawler$city
Encoding(db_events_current_crawler$city) 


"UTF-8"
db_events_current_crawler$city

db_events_current_crawler
meta_df

x = data.frame(c("Änderung","Würzburg"))
write.xml(x,"x.xml")


db_organizer_names <- as.data.frame(tbl(conn, "organizer"))
db_organizer_names                                                                
db_organizer_names$organizer = iconv(db_organizer_names$organizer, "latin1", "UTF-8")                                    

select(db_organizer_names,"Universitätsklinikum Würzburg")
db_organizer_names
db_organizer_names <- as.data.frame(tbl(conn, "organizer") %>%
                                      select(organizer))

db_organizer_names$organizer = iconv(db_organizer_names$organizer, "latin1", "UTF-8")

!any(db_organizer_names==as.character(meta_df["organizer"][1,1]))


db_events_current_crawler = data.frame(db_events_current_crawler)

db_events_current_crawler %>% map_if(is.character, iconv("latin1", "UTF-8")) %>% as_data_frame -> db_events_current_crawler


typeof(db_events_current_crawler$idevent)

db_events_current_crawler %>% map_if(is.character, iconv(.,"latin1", "UTF-8"))

db_events_current_crawler


db_events_current_crawler$category = iconv(db_events_current_crawler$category, "latin1", "UTF-8")

db_events_current_crawler

db_events_current_crawler %>% mutate_if(is.character, iconv("latin1", "UTF-8"))

for (i in colnames(db_events_current_crawler)){
  if(is.character(db_events_current_crawler[[i]])){
    db_events_current_crawler[[i]] = iconv(db_events_current_crawler[[i]], "latin1", "UTF-8")
  }
}
class(db_events_current_crawler)
colnames(db_events_current_crawler)
db_events_current_crawler


a = data.frame(c("Änderung", "ünder"))
write.xml(a, "a.xml")


xml <- xmlTree()
# names(xml)
xml$addTag("report", close=FALSE, attrs=c(type="enhanced"))
xml$addTag("pages", close=FALSE)
for (i in 1:nrow(data)) {
  xml$addTag("page", close=FALSE)
  for (j in names(data)) {
    xml$addTag(j, data[i, j])
  }
  xml$closeTag()
}
xml$closeTag()
xml$closeTag()
cat(saveXML(xml))








