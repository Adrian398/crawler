##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvements ###
# 1. crawl only first page of events (js)
# responsible: Wei


### Schönstatt-Zentrum Marienhöhe ####
# crawl data
url = "https://www.schoenstatt-wuerzburg.de/startseite-links/veranstaltungen"
raw_read = read_html(url)

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".itemtitle a") %>%
  html_attr("href") -> link

link = paste("https:", link, sep = "")

raw_read %>%
  html_nodes(".datetime") %>%
  html_text(trim = T) -> raw_date

date_start = str_extract_all(raw_date, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", simplify = T)[,1]
date_end = str_extract_all(raw_date, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", simplify = T)[,2]

description = c()

for (cache_link in link) {
  cache_raw_read = read_html(cache_link)
  cache_raw_read %>%
    html_nodes(".datarow-container") %>%
    html_text(trim = T) -> cache_description
  
  description = c(description, cache_description)
}

# fixed data setup
organizer = "Schönstatt-Zentrum Marienhöhe"
url = "https://www.schoenstatt-wuerzburg.de/index.html"
lat = rep(49.77239, length(title))
lng = rep(9.97337, length(title))
street = rep("Josef-Kentenich-Weg 1", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end = NA
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start = NA
time_end = NA

# build table
crawled_df <- data.frame(
                    title = title,
                    date_start = date_start,
                    date_end = date_end,
                    time_start = time_start,
                    time_end = time_end,
                    description = description,
                    lat = lat,
                    lng = lng,
                    street = street,
                    zip = zip,
                    city = city,
                    link = link)

#add metadf idlocation
idlocation = 6489
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 18
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

