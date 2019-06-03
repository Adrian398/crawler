library(feedeR)
library(tidyRSS)
url = "https://www.pg-sanderau.de/"
df = tidyfeed("https://www.pg-sanderau.de/aktuelles---termine/index.html.rss")
  
date_start = as.Date(df$items$date)
time_start = chron(times = strftime(df$items$date, format="%H:%M:%S"))
# setting up the rest data
city = "Wuerzburg"
street = "Matthias-Ehrenfried-Straße 2"
zip = 97074
organizer = "St. Alfons"
lat = 49.77511
lng = 9.95303
description = NA
date_end = NA
time_start = NA
time_end = NA

#set up to write to database
meta_df = data.frame(organizer, url)
crawled_df = data.frame(df$items$title, description, df$items$link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)



df$item_description[1] %>% html_nodes("span")

test = xmlParse(df$item_description[1])
