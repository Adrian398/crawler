library(feedeR)
library(tidyRSS)
library(XML)
library(selectr)

url = "https://www.weltladen-wuerzburg.de/termine"
df = tidyfeed("https://www.weltladen-wuerzburg.de/termine.rss")

doc <- htmlParse(df$item_description)

link = df$item_link
date_start = as.Date(xmlValue(querySelector(doc, 'div > span')), "%d.%m.%Y" )
time_start = NA
# setting up the rest data
city = "Wuerzburg"
street = "Plattnerstr. 14"
zip = 97070
organizer = "St. Alfons"
lat = 49.7924792
lng = 9.9312072
description = NA
date_end = NA
time_start = NA
time_end = NA
title = df$item_title


#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)

#add metadf idlocation
idlocation = 403026
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'
crawled_df

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)






