##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. none
# responsible: Wei


### Posthalle Würzburg ####
# crawl data
url = "https://posthalle.de/programm"

raw_read = read_html(url)

raw_read %>%
  html_nodes(".tease-text > h2 > a > span") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".field--type-datetime") %>%
  html_text(trim = T) -> raw_date

raw_read %>%
  html_nodes(".tease-text > h2 > a") %>%
  html_attr("href") -> link

raw_read %>%
  html_nodes(".tease-img a > img") %>%
  html_attr("src") -> image_url

link = paste("https://posthalle.de", link, sep = "")
image_url = paste("https://posthalle.de", image_url, sep = "")

date_start = str_extract(raw_date, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}")
time_start = str_extract(raw_date, "\\s[0-9]{2}\\.[0-9]{2}")
time_start = paste(gsub("\\.", ":", gsub("\\s", "", time_start)), ":00", sep = "")

description = c()
for (cache_link in link) {
  
  cache_raw_read = read_html(cache_link)
  
  cache_raw_read %>%
    html_nodes(".region-content > article p") %>%
    html_text(trim = T) -> cache_description
  
  cache_description = paste(cache_description, collapse = " ")
  
  description = c(description, cache_description)
}

# fixed data setup
organizer = "Posthalle Würzburg"
url = "http://www.posthalle.de"
category= rep("Nachtleben", length(title))
lat = rep(49.801638407695, length(title))
lng = rep(9.9321162380983, length(title))
street = rep("Bahnhofplatz 2", length(title))
zip = rep(97070, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end = NA
date_end = as.Date(date_start, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end = NA
#time_end = chron(times = time_end)

# build table
crawled_df <- data.frame(
                    category = category,
                    title = title,
                    date_start = date_start,
                    date_end = date_start,
                    time_start = time_start,
                    time_end = time_end,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link,
                    image_url = image_url)

#add metadf idlocation
idlocation = 12843
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

meta_df["idcrawler"] = 17
meta_df["id_category"] = 10586


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
