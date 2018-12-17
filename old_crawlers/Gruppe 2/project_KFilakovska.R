library(tidyverse)
library(xml2)
library(rvest)
library(tidyr)
library(dplyr)
library(naniar)
library(plyr)
library(chron)
library(lubridate)

# Seite ohne richtige Struktur - kann nur manuell weitergeführt werden


url44 <- "https://www.martinvonwagner-museum.com/veranstaltungen"

url44 %>% 
  read_html() -> raw_data44

raw_data44 %>%
  html_nodes(".font_0:nth-child(74) , .font_0:nth-child(67) , .font_0:nth-child(61) , .font_0:nth-child(53) , .font_0:nth-child(47) , .font_0:nth-child(41) , .font_0:nth-child(32) , .font_0:nth-child(25) , .font_0:nth-child(19) , .font_8~ .font_0+ .font_0 , .font_0:nth-child(11) , .font_0:nth-child(4)") %>%
  html_text() -> datum44
datum44 <- gsub("und", "-", datum44)
date_start <- gsub("\\-.*","",datum44)
date_end <- str_extract(datum44,"(?<=-).*")

raw_data44  %>%
  html_nodes(".font_8:nth-child(76) , .font_8:nth-child(69) , .font_8:nth-child(63) , .font_8:nth-child(55) , .font_8:nth-child(49) , .font_8:nth-child(43) , .font_8:nth-child(34) , .font_8:nth-child(27) , .font_8:nth-child(21) , .font_8:nth-child(16) , .font_8:nth-child(12) , .font_8:nth-child(6)") %>%
  html_text() -> title44

raw_data44 %>%
  html_nodes(".font_8:nth-child(68) , .font_8:nth-child(62) , .font_8:nth-child(54) , .font_8:nth-child(48) , .font_8:nth-child(42) , .font_8:nth-child(33) , .font_8:nth-child(26) , .font_8:nth-child(20) , .font_8:nth-child(5) , .font_8:nth-child(75)") %>%
  html_text() -> title44_2
title44_2 <- data.frame(title44_2)

raw_data44 %>%
  html_nodes(".font_8:nth-child(78) , .font_8:nth-child(71) , .font_8:nth-child(65) , .font_8:nth-child(58) , .font_8:nth-child(51) , .font_8:nth-child(45) , .font_8:nth-child(37) , .font_8:nth-child(29) , .font_8:nth-child(23) , .font_8:nth-child(17) , .font_8:nth-child(13) , .font_8:nth-child(9)") %>%
  html_text() -> details44

description <- str_extract(details44,"(?<=,).*")
description <- data.frame(description)
description %>%
  filter(!is.na("\\d")) -> description
description <- data.frame(description)



Zeit <- gsub("\\,.*","",details44)
Zeit <- gsub("[[:alpha:]]", "",Zeit)

time_start <- str_extract(Zeit, "[0-9]{1,2}")
time_end <- str_extract_all(Zeit, "(?<=- )[0-9]{1,2}")
time_end <- as.character(time_end)
time_end[time_end == "character(0)"] <- NA


time_start <- times(paste0(time_start, ":00:00"))

time_end <- times(paste0(time_end, ":00:00"))



url <- "https://www.martinvonwagner-museum.com/veranstaltungen"
url <- as.character(url)

lng <- c(9.93844)
lng <- as.numeric(lng)

lat <- c(49.79331)
lat <- as.numeric(lat)

city <- "Wuerzburg"
city <- as.character(city)

street <- "Residenzplatz 2A"
street <- as.character(street)

zip <- c(97070)
zip <- as.numeric(zip)

date_start <- gsub("Januar","01",date_start)
date_start <- gsub("Febraur","02",date_start)
date_start <- gsub("März","03",date_start)
date_start <- gsub("April","04",date_start)
date_start <- gsub("Mai","05", date_start)
date_start <- gsub("Juni","06",date_start)
date_start <- gsub("Juli","07",date_start)
date_start <- gsub("August","08",date_start)
date_start <- gsub("September","09",date_start)
date_start <- gsub("Oktober","10",date_start)
date_start <- gsub("November","11",date_start)
date_start <- gsub("Dezember","12",date_start)
date_start <- gsub(" ", "", date_start)

date_start <- paste0(date_start,".2018")

date_start <- as.Date(date_start, format = "%d.%m.%Y")

date_end <- gsub("Januar","01",date_end)
date_end <- gsub("Febraur","02",date_end)
date_end <- gsub("März","03",date_end)
date_end <- gsub("April","04",date_end)
date_end <- gsub("Mai","05", date_end)
date_end <- gsub("Juni","06",date_end)
date_end <- gsub("Juli","07",date_end)
date_end <- gsub("August","08",date_end)
date_end <- gsub("September","09",date_end)
date_end <- gsub("Oktober","10",date_end)
date_end <- gsub("November","11",date_end)
date_end <- gsub("Dezember","12",date_end)
date_end <- gsub(" ", "", date_end)

date_end <- paste0(date_end,".2018")
date_end[date_end == "NA.2018"] <- NA
date_end <- as.Date(date_end, format = "%d.%m.%Y")

price <- str_extract(details44,"(?<=,).*") 
price <- str_extract(details44, "(?<=, )[[:punct:]\\d]")
price <- as.character(price)

organizer <- "Martin von Wagner Museum"
organizer <- as.character(organizer)



v1 <- data.frame(title = title44, url = url, description = description, lng = lng, lat = lat, city = city, street = street, zip = zip, date_start = date_start, date_ende = date_end, time_start = time_start, time_end = time_end, price = price, organizer = organizer)


v1$description <- str_remove_all(v1$description, "[[:punct:]€\\d]")
v1$description[v1$description == "  "] <- NA
v1$description[v1$description == "character(0)"] <- NA
v1$description[v1$description == ""] <- NA

colnames(v1) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")




url46 <- "http://www.familienzentrum-wuerzburg.de/termine/"

url46 %>%
  read_html() %>%
  html_nodes(".csc-textpic-text p") %>%
  html_text(trim = T) -> data1
v2 <- as.data.frame(data1)


#edit data

v2$data <- str_remove_all(v2$data1, "[A-z]{2}[\\.]{1}\\s?[0-9]{1,2}[\\.]{1}[0-9]{1,2}[\\.]{1} ")
v2$title <- str_extract(v2$data, "(?<=- )[A-zÄäÖöÜüßéÉ[:punct:]\\s]{1,25} ")
v2$title <- gsub(",","",v2$title)

v2$url <- "http://www.familienzentrum-wuerzburg.de/termine/"
v2$url <- as.character(v2$url)

v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")
v2$description <-  str_extract_all(v2$data, "(?<= Uhr)[[:punct:]\\w ]*")
v2$description[v2$description == "  "] <- NA
v2$description[v2$description == "character(0)"] <- NA
v2$description[v2$description == ""] <- NA


v2$lng <- c(9.93684)
v2$lng <- as.numeric(v2$lng)

v2$lat <- c(49.76189)
v2$lat <- as.numeric(v2$lat)

v2$city <- "Wuerzburg-Heidingsfeld"
v2$city <- as.character(v2$city)

v2$street <- "Frau-Holle-Weg 27"
v2$street <- as.character(v2$street)

v2$zip <- c(97084)
v2$zip <- as.numeric(v2$zip)


v2$date_start <- str_extract(v2$data1, "[0-9]{1,2}[\\.][0-9]{1,2}")
v2$date_start <- paste0(v2$date_start,".2018")
v2$date_start <- as.Date(v2$date_start, format = "%d.%m.%Y")

v2$data <- str_remove_all(v2$data1, "[A-z]{2}[\\.]{1}\\s?[0-9]{1,2}[\\.]{1}[0-9]{1,2}[\\.]{1} ")

v2$date_end <- NA

v2$price <- NA

v2$time_start <- str_extract(v2$data, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}") 
v2$time_end <- str_extract(v2$data, "(?<=- )[0-9]{1,2}[\\.]{1}[0-9]{1,2}")

v2$time_start <- gsub("\\.", ":", v2$time_start) 
v2$time_end <- gsub("\\.",":",v2$time_end)

v2$time_start <- times(paste0(v2$time_start, ":00"))
v2$time_end <- times(paste0(v2$time_end, ":00"))

v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")


v2$organizer <- "Familienzentrum Würzburg e.V."
v2$organizer <- as.character(v2$organizer)

v2 %>%
  filter(!is.na(title)) -> v2

v2$data1 <- NULL
v2$data <- NULL



#skz kann nur  manuell weiterbearbeitet werden


url47 <- "https://www.skz.de/de/index.html"

url47 %>% 
  read_html() -> raw_data47

raw_data47 %>%
  html_nodes(".tree_28675 .jbtitle+ p") %>%
  html_text() -> zeit47
zeit47 <- gsub("bis", "-", zeit47)


raw_data47  %>%
  html_nodes(".tree_28675 .jbtitle") %>%
  html_text() -> title47

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

v3 <- data.frame(title = title47,url = url, lng =lng, lat =lat, city =city, street = street, zip =zip, organizer = organizer, Datum = zeit47, time_start = time_start, time_end = time_end, price = price)

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



v_all <- rbind.fill(v1[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")], v2[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "organizer", "price")], v3[c("title", "url", "lng", "lat", "city", "street", "zip", "organizer", "description", "time_start", "time_end","date_start", "date_end", "price")])
v_all <- v_all[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")]

v_all <- rbind(if(exists("v1")) v1,
               if(exists("v2")) v2,
               if(exists("v3")) v3)


df_test <- v_all

df_test <- apply(df_test, 2, as.character)  
write.csv2(df_test, "df_complete_K_Fila.csv")

