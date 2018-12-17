library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(tidyr)
library(naniar)
library(chron)
library(lubridate)


#Buchhandlung: keine Veranstaltungen angegeben


#Posthalle: FERTIG

site <- 'http://www.posthalle.de/de/programm.html'

site %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes(".prog_event_timing") %>%
  html_text() -> zeit10

raw_data %>%
  html_nodes(".prog_event_headline > a") %>%
  html_text() -> title

raw_data %>%
  html_nodes(".prog_event_date") %>%
  html_text() -> datum

lat <- c(49.801638407695)
lng <- c(9.9321162380983)

link1 <- "http://www.posthalle.de/de/programm.html"
link1 <- as.character(link1)

city1 <- ("Wuerzburg")
city1 <- as.character(city1)

street1 <- ("Bahnhofplatz 2")
street1 <- as.character(street1)

zip1 <- c(97070)

organizer1 <- ("Posthalle Würzburg")
organizer1 <- as.character(organizer1)

vs <- data.frame(title = title, url = link1, time_start = zeit10, date_start = datum, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city1, street = street1, zip = zip1, organizer = organizer1, price = NA)

gsub("[A-z]{1,10}","", vs$date_start) -> vs$date_start
vs$description <- str_extract(vs$time_start,"[A-z]{1,7}: [0-9]{1,2}:[0-9]{1,2}[A-z]{1,3}")
gsub("[A-z]{1,10}","", vs$time_start) -> vs$time_start
gsub(".*,","", vs$time_start) -> vs$time_start
vs$time_start <- str_extract(vs$time_start,"[0-9]{1,2}:[0-9]{1,2}")

vs$date_start <- as.Date(vs$date_start, format = "%d.%m.%Y")

time_start0 <- vs$time_start
time_start0 <- as.character(time_start0)
time_start0 <- times(paste0(time_start0, ":00"))

vs <- data.frame(title = title, url = link1, description = vs$description, time_start = time_start0, date_start = vs$date_start, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city1, street = street1, zip = zip1, organizer = organizer1, price = NA)


s <- html_session(site)
s <- s %>% follow_link(css = '.page-navi-next a')
url2 <- s$handle$url

s %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes(".prog_event_timing") %>%
  html_text() -> zeit12

raw_data %>%
  html_nodes(".prog_event_headline > a") %>%
  html_text() -> title2

raw_data %>%
  html_nodes(".prog_event_date") %>%
  html_text() -> datum2

lat <- c(49.801638407695)
lng <- c(9.9321162380983)

link2 <- "http://www.posthalle.de/de/programm.html"
link2 <- as.character(link2)

city2 <- ("Wuerzburg")
city2 <- as.character(city2)

street2 <- ("Bahnhofplatz 2")
street2 <- as.character(street2)

zip2 <- c(97070)

organizer2 <- ("Posthalle Würzburg")
organizer2 <- as.character(organizer2)

vs2 <- data.frame(title = title2, url = link2, time_start = zeit12, date_start = datum2, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city2, street = street2, zip = zip2, organizer = organizer2, price = NA)

gsub("[A-z]{1,10}","", vs2$date_start) -> vs2$date_start
vs2$description <- str_extract(vs2$time_start,"[A-z]{1,7}: [0-9]{1,2}:[0-9]{1,2}[A-z]{1,3}")
gsub("[A-z]{1,10}","", vs2$time_start) -> vs2$time_start
gsub(".*,","", vs2$time_start) -> vs2$time_start
vs2$time_start <- str_extract(vs2$time_start,"[0-9]{1,2}:[0-9]{1,2}")

vs2$date_start <- as.Date(vs2$date_start, format = "%d.%m.%Y")

time_start1 <- vs2$time_start
time_start1 <- as.character(time_start1)
time_start1 <- times(paste0(time_start1, ":00"))

vs2 <- data.frame(title = title2, url = link2, description = vs2$description, time_start = time_start1, date_start = vs2$date_start, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city2, street = street2, zip = zip2, organizer = organizer2, price = NA)


s <- s %>% follow_link(css = '.page-navi-next a')
url3 <- s$handle$url

s %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes(".prog_event_timing") %>%
  html_text() -> zeit13


raw_data %>%
  html_nodes(".prog_event_headline > a") %>%
  html_text() -> title3

raw_data %>%
  html_nodes(".prog_event_date") %>%
  html_text() -> datum3

lat <- c(49.801638407695)
lng <- c(9.9321162380983)

link3 <- "http://www.posthalle.de/de/programm.html"
link3 <- as.character(link3)

city3 <- ("Wuerzburg")
city3 <- as.character(city3)

street3 <- ("Bahnhofplatz 2")
street3 <- as.character(street3)

zip3 <- c(97070)

organizer3 <- ("Posthalle Würzburg")
organizer3 <- as.character(organizer3)

vs3 <- data.frame(title = title3, url = link3, time_start = zeit13, date_start = datum3, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city3, street = street3, zip = zip3, organizer = organizer3, price = NA)

gsub("[A-z]{1,10}","", vs3$date_start) -> vs3$date_start
vs3$description <- str_extract(vs3$time_start,"[A-z]{1,7}: [0-9]{1,2}:[0-9]{1,2}[A-z]{1,3}")
gsub("[A-z]{1,10}","", vs3$time_start) -> vs3$time_start
gsub(".*,","", vs3$time_start) -> vs3$time_start
vs3$time_start <- str_extract(vs3$time_start,"[0-9]{1,2}:[0-9]{1,2}")

vs3$date_start <- as.Date(vs3$date_start, format = "%d.%m.%Y")

time_start2 <- vs3$time_start
time_start2 <- as.character(time_start2)
time_start2 <- times(paste0(time_start2, ":00"))

vs3 <- data.frame(title = title3, url = link3, description = vs3$description, time_start = time_start2, date_start = vs3$date_start, lat = lat, lng = lng, date_end = NA, time_end = NA, city = city3, street = street3, zip = zip3, organizer = organizer3, price = NA)


library(plyr)
veranstaltungenposthalle <- rbind.fill(vs[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")], vs2[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")], vs3[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")])



#Jugendkulturhaus: FERTIG

jugendkulturhaus <- "https://cairo.wue.de/"

jugendkulturhaus %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes("time") %>%
  html_text() -> datum4

raw_data %>%
  html_nodes("#block-views-block-veranstaltungen-frontpage-block-1 .titlefr") %>%
  html_text() -> title4

lat <- c(49.78961)
lng <- c(9.92466)

link <- "https://cairo.wue.de/"
link <- as.character(link)

city <- ("Wuerzburg")
city <- as.character(city)

street <- ("Fred-Joseph-Platz 3")
street <- as.character(street)

zip <- c(97082)

organizer <- ("Jugendkulturhaus Cairo")
organizer <- as.character(organizer)

veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, date_start = datum4, lat = lat, lng = lng, city = city, street = street, zip = zip)

veranstaltungenjugendkulturhaus$date_start <- paste0(datum4, ".2018")
gsub("[A-z]{1,10}","", veranstaltungenjugendkulturhaus$date_start) -> veranstaltungenjugendkulturhaus$date_start
veranstaltungenjugendkulturhaus$date_start <- str_extract(veranstaltungenjugendkulturhaus$date_start,"[0-9]{1,2}/[0-9]{1,2}.[0-9]{1,4}")

gsub("/", ".", veranstaltungenjugendkulturhaus$date_start, fixed = TRUE) -> date_start

date_start1 <- as.Date(date_start, format = "%d.%m.%Y")

veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, description = NA, lng = lng, lat = lat, city = city, street = street, zip = zip, date_start = date_start1, date_end = NA, time_start = NA, time_end = NA, price = NA, organizer = organizer)



#Weltladen: keine Veranstaltungen angegeben


#Bürgerbräu:FERTIG


buergerbraeu <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"

buergerbraeu %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes("time") %>%
  html_text() -> datum5
dfdatum5 <- data.frame(datum5)

raw_data %>%
  html_nodes(".first-headline span") %>%
  html_text() -> title5
dftitle5 <- data.frame(title5)

raw_data %>%
  html_nodes(".teaser-text p") %>%
  html_text() -> description

lat <- c(49.7937172)
lon <- c(9.894353)

link <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"
link <- as.character(link)

city <- ("Wuerzburg")
city <- as.character(city)

street <- ("Frankfurter Straße 87")
street <- as.character(street)

zip <- c(97082)

organizer <- ("Bürgerbräu")
organizer <- as.character(organizer)

veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, date_start = datum5, date_end = NA, time_end = NA, description = description, lat = lat, lng = lng, city = city, street = street, zip = zip, organizer = organizer, price = NA)


veranstaltungenbuergerbraeu$time_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}:[0-9]{1,2}")
veranstaltungenbuergerbraeu$date_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}.[0-9]{1,2}.[0-9]{1,4}")

date_start <- veranstaltungenbuergerbraeu$date_start
time_start <- veranstaltungenbuergerbraeu$time_start
time_start <- as.character(time_start)


date_start <- as.Date(date_start, format = "%d.%m.%Y")

time_start <- times(paste0(time_start, ":00"))


veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, description = description, lng = lng, lat = lat, city = city, street = street, zip = zip, date_start = date_start, date_end = NA, time_start = time_start, time_end = NA, price = NA, organizer = organizer)

veranstaltungengesamt <- rbind.fill(veranstaltungenposthalle[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")], veranstaltungenjugendkulturhaus[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")], veranstaltungenbuergerbraeu[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")])
veranstaltungengesamt <- veranstaltungengesamt[c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")]
            

veranstaltungengesamt[] <- lapply(veranstaltungengesamt, function(x) if (is.factor(x)) as.character(x) else {x})


veranstaltungengesamt <- rbind(if(exists("veranstaltungenposthalle")) veranstaltungenposthalle,
                               if(exists("veranstaltungenjugendkulturhaus")) veranstaltungenjugendkulturhaus,
                               if(exists("veranstaltungenbuergerbraeu")) veranstaltungenbuergerbraeu)

#df_test <- veranstaltungengesamt

#df_test <- apply(df_test, 2, as.character)              
#write.csv2(df_test, "df_complete_A_Lips.csv")