##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

### Possible Improvement ###
# no
# responsible: Wei

### LaViva Danceclub ###
# crawl data
url = "https://www.la-viva-danceclub.de/index.php/events-laviva-danceclub"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".date") %>%
  html_text(trim = T) -> date_start

raw_read %>%
  html_nodes("h2") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".details > p") %>%
  html_text(trim = T) -> description

raw_read %>%
  html_nodes(".details") %>%
  html_text(trim = T) %>%
  str_extract("[0-9]{2}:[0-9]{2}") -> time_start

# data mutation
date_start = gsub("\\s", "", str_extract(raw_data, "[0-9]{2}\\s\\.[0-9]{2}\\.\\s[0-9]{4}"))
date_start = as.Date(date_start, "%d.%m.%Y")

# Fixed data setup
date_end = rep(NA, length(title))
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
link = rep("https://www.la-viva-danceclub.de/index.php/events-laviva-danceclub", length(title))
lat = rep(49.7956313, length(title))
lng = rep(9.970704, length(title))
organizer = rep("Laviva Danceclub", length(title))
street = rep("Nürnberger Straße 72", length(title))
zip = rep(97076, length(title))
city = rep("Würzburg", length(title))

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
                  city = city)
