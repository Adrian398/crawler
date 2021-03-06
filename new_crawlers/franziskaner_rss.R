library(feedeR)
library(tidyRSS)

url = "https://www.franziskanerkloster-wuerzburg.de/startseite-links/veranstaltungen"
df = tidyfeed("https://www.franziskanerkloster-wuerzburg.de/startseite-links/veranstaltungen.rss")

date_start = NA
time_start = NA

date_start = as.Date(xmlValue(querySelector(df, 'div > span')), "%d.%m.%Y" )
# setting up the rest data
city = "Wuerzburg"
street = "Matthias-Ehrenfried-Straße 2"
zip = 97074
organizer = "St. Alfons"
lat = 49.77511
lng = 9.95303
description = NA
date_end = as.Date(NA)
time_start = NA
time_end = NA

#set up to write to database
crawled_df = data.frame(title = df$item_title, description, link = df$item_link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
idlocation = 5180
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)


