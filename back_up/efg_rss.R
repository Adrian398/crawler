url = "http://www.efg-wuerzburg.de/news"
df = tidyfeed("http://www.efg-wuerzburg.de/news/feed.xml")

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
crawled_df = data.frame(df$items$title, description, df$items$link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
meta_df = data.frame(organizer, url)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

df$item_description[2]
