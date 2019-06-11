
url <- "https://wuerzburg.bund-naturschutz.de/veranstaltungen.html"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes("td a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes("td a") %>%
  html_attr("href") -> link

raw_read %>%
  html_nodes(".deadlinelist p") %>%
  html_text(trim = T) -> dates

for(i in 1:length(dates)){
  dates[i]= strsplit(dates[i], " ")[[1]][2]
}

date_start = as.Date(dates,"%d.%m.%Y")

date_end = NA
time_start = NA
time_end = NA

# setting up the rest data
city = "Wuerzburg"
street = "Luitpoldstraße 7a"
zip = 97082
organizer = "Oekohaus Wuerzburg"
lat = 49.79746
lng = 9.9199
description = NA

#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)

#add metadf idlocation
idlocation = 4589
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
