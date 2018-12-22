##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)

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
street = rep("Josef-Schneider-Straße 2", length(title))
zip = rep(97080, length(title))
city = rep("Würzburg", length(title))

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
ukw <- data.frame(title = title,
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

