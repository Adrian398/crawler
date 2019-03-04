library(chron) 
library(tidyverse)

url <- "https://www.skz.de/de/index.html"
url <- as.character(url)

lng <- c(9.9154094)
lng <- as.numeric(lng)

lat <- c(49.7943474)
lat <- as.numeric(lat)

city <- "Wuerzburg"
city <- as.character(city)

street <- "Friedrich-Bergius-Ring 22"
street <- as.character(street)

zip <- c(97076)
zip <- as.numeric(zip)

organizer <- "Fördergemeinschaft für das Kunststoff-Zentrum SKZ"
organizer <- as.character(organizer)

time_start <- NA
time_end <- NA

price <- NA

v3 <- data.frame(title = title47,url = url, lng =lng, lat =lat, city =city, street = street, zip =zip, Datum = zeit47, time_start = time_start, time_end = time_end)

v3$description <- str_extract_all(v3$Datum, "(?<= 2018)[[:alpha:]].*")
v3$date_start <- str_extract(v3$Datum, "[0-9]{1,2}[\\.]")
v3$date_end <- str_extract(v3$Datum, "[0-9]{1,2}[\\.] - [0-9]{0,2}[\\.]*")
v3$date_end <- gsub(".*-","",v3$date_end) 

v3$date_start <- paste0(v3$date_start,"06.2018")

v3$date_end <- paste0(v3$date_end,"06.2018")
v3$date_end[v3$date_end == "NA06.2018"] <- NA
v3$Datum <- NULL
v3$date_start <- as.Date(v3$date_start, format = "%d.%m.%Y")
v3$date_end <- as.Date(v3$date_end, format = "%d.%m.%Y")

v3$description <- gsub("mit Live-Vorführungen im SKZ Verarbeitungstechnikum",NA,v3$description)
v3$description <- gsub("mit Get-together am 20. Juni 2018",NA,v3$description)

meta_df = data.frame(url = url
                     , organizer = organizer)

names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'
names(v3)[names(v3) == 'url'] <- 'link'

v3[[3]] = as.character(v3[[3]])
#write to database
write_dataframes_to_database(v3, meta_df, conn)

v3
