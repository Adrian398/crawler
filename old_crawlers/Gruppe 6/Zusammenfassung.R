library(xml2)
library(rvest)
library(tidyverse)
library(RSelenium)
library(chron)

##############################
# Qiuyan
##############################

##############################
# AMW
##############################
link_amv <- 'https://www.amv-wuerzburg.de/semesterprogramm'
page_amv <- read_html(link_amv)

events_amv <- html_nodes(page_amv,'#comp-irfhuo3u .font_9') %>%
  html_text() 

# get the date 
regex_date <- "[A-z]{2}\\s{1,2}[0-9]{2}[\\./][0-9]{2}[\\./]"
date_amv <- str_extract_all(events_amv,regex_date) %>%
  as.character()%>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub("\U00A0"," ",.) %>%
  gsub("[A-z]{2}\\s{1,2}","",.)%>%
  str_extract_all("\\d{2}[\\./]\\d{2}") %>%
  as.character()
date_amv <- as.Date(date_amv, format = "%d.%m")

# get the time
regex_time <- "\\b([A-z]{2}\\s)?[0-9]{2}[\\:/][0-9]{2}"
time_amv <- str_extract_all(events_amv,regex_time) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub("ab ","",.)
time_amv <- times(paste0(time_amv, ":00"))

# get the events
regex_new_events <- "-\\s\\D{5,50}"
new_events_amv <- str_extract_all(events_amv,regex_new_events) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub("\U00A0","",.) %>%
  gsub("- ","",.) %>%
  gsub("c[\\(/]","",.) 


time_amv_end <- rep(NA, length(new_events_amv)) %>%
  as.character() %>%
  times()
  
veranstalter_amw <- rep("AMW", length(new_events_amv))
preis_amw <- rep(NA,length(new_events_amv)) %>%
  as.character()
kurz_beschreibung <- rep(NA,length(new_events_amv)) %>%
  as.character()
city_amw <- rep('Wuerzburg', length(new_events_amv))
street_amw <- rep('Valentin-Becker-Straße 2', length(new_events_amv))
zip_amw <- rep(97072, length(new_events_amv))
lat_amw <- rep(49.7909, length(new_events_amv))
lng_amw <- rep(9.94384, length(new_events_amv))
url_amw <- rep("https://www.amv-wuerzburg.de/semesterprogramm", length(new_events_amv))

# complete list of events
final_event <- data.frame(
  date_start = date_amv,
  date_end= date_amv,
  time_start = time_amv,
  time_end= time_amv_end,
  title = new_events_amv,
  description=kurz_beschreibung,
  price=preis_amw,
  city= city_amw,
  street= street_amw,
  zip= zip_amw,
  organizer = veranstalter_amw,
  lat= lat_amw,
  lng= lng_amw,
  url= url_amw,
  stringsAsFactors = F
)

final_event %>%
  filter(!is.na(title)) -> event_clean

##############################
# Cinemaxx
##############################
cinemaxx_link <- 'https://www.cinemaxx.de/event'
page_cinemaxx <- read_html(cinemaxx_link)
# get the events and the date
events_cinemaxx <-html_nodes(page_cinemaxx,'.underline--alt') %>%
  html_text() %>%
  gsub("\r\n","",.) %>%
  str_trim()

regex_cinemaxx_date <- '[0-9]{1,2}[\\./]\\s[A-z]{3,9}'
cinemaxx_date <- str_extract_all(events_cinemaxx,regex_cinemaxx_date) %>%
  as.character()

for (i in cinemaxx_date){
  cinemaxx_date = gsub(" Januar","01",cinemaxx_date)
  cinemaxx_date = gsub(" Februar","02",cinemaxx_date)
  cinemaxx_date = gsub(" März", "03", cinemaxx_date)
  cinemaxx_date = gsub(" April", "04", cinemaxx_date)
  cinemaxx_date = gsub(" Mai", "05", cinemaxx_date)
  cinemaxx_date = gsub(" Juni", "06", cinemaxx_date)
  cinemaxx_date = gsub(" Juli", "07", cinemaxx_date)
  cinemaxx_date = gsub(" August", "08", cinemaxx_date)
  cinemaxx_date = gsub(" September", "09", cinemaxx_date)
  cinemaxx_date = gsub(" Oktober", "10", cinemaxx_date)
  cinemaxx_date = gsub(" November", "11", cinemaxx_date)
  cinemaxx_date = gsub(" Dezember", "12", cinemaxx_date)
}

cinemaxx_date <- as.Date(cinemaxx_date, format = "%d.%m")
cinemaxx_date_end <- cinemaxx_date

cinemaxx_event <- gsub('[0-9]{1,2}[\\./]\\s[A-z]{3,9}\\s',"",events_cinemaxx) 

# get the links of events
main_url_cinemaxx <- 'https://www.cinemaxx.de'
events_detail <- html_nodes(page_cinemaxx,'.ml-movie-boxes__layer__link') %>%
  html_attr('href') %>%
  paste(main_url_cinemaxx,.) %>%
  gsub(" ","",.)

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate('https://www.cinemaxx.de/event')
Sys.sleep(3)
remDr$maxWindowSize()
# shut down the cookie message
remDr$findElement(using = 'css selector', "#offershub > div.cookie-msg.active > div > span > svg > path")$clickElement()

# choose the city
remDr$findElement(using = 'css selector', "li:nth-child(29) a")$clickElement()

# try to get the detail of the event
zeit_list <- c()
kurz_infor_list <- c()
for (event_link in events_detail){
  remDr$navigate(event_link)
  # get the HTML-Site
  detail_pg <- read_html(remDr$getPageSource()[[1]])
  zeit <- html_nodes(detail_pg,'.default') %>%
    html_text()
  zeit_list <- append(zeit_list,zeit)
  kurz_infor_cinemaxx <- html_nodes(detail_pg,'.expand-small .offer__intro') %>%
    html_text()
  kurz_infor_list <- append(kurz_infor_list,kurz_infor_cinemaxx)
  Sys.sleep(2)
}

remDr$close()
rm(rD)
gc()

zeit_list <- times(paste0(zeit_list, ":00"))

time_end_cinemaxx <- rep(NA,length(cinemaxx_date)) %>%
  as.numeric() %>%
  times()
preis_cinemaxx <- rep(NA,length(cinemaxx_date)) %>%
  as.character()
veranstalter_cinemaxx <- rep("Cinemaxx",length(cinemaxx_date))
city_cinemaxx <- rep("Wuerzburg",length(cinemaxx_date))
street_cinemaxx <- rep("Veitshöchheimer Straße 5a ",length(cinemaxx_date))
zip_cinemaxx <- rep(97080,length(cinemaxx_date))
lat_cinemaxx <- rep(49.8028517,length(cinemaxx_date))
lng_cinemaxx <- rep(9.9209681,length(cinemaxx_date))

complete_events_list <- data.frame(
  date_start = cinemaxx_date,
  date_end= cinemaxx_date_end,
  time_start = zeit_list,
  time_end= time_end_cinemaxx,
  title = cinemaxx_event,
  description=kurz_infor_list,
  price=preis_cinemaxx,
  city= city_cinemaxx, 
  street= street_cinemaxx,
  zip= zip_cinemaxx,
  organizer = veranstalter_cinemaxx,
  lat=lat_cinemaxx,
  lng= lng_cinemaxx,
  url= events_detail,
  stringsAsFactors = F
)

##############################
# merging all calendars
##############################
end_eventcalender <- rbind(event_clean, complete_events_list, stringsAsFactors = F)

##############################
# Chen
##############################

##############################
# Bockshorn
##############################
url_bock <- 'http://www.bockshorn.de/spielplan.htm'
page_bock <- read_html(url_bock)

# INFO link
info_links <- html_nodes(page_bock,'.aspevpreis') %>%
  html_attr('href')
main_link_bock <- 'http://www.bockshorn.de/'
info_links <- paste(main_link_bock,info_links) %>%
  gsub(" ","",.)

# use RSelenium to get all title
rD <- rsDriver()
remDr <- rD[["client"]]

title_list_bock <- c()
for (infor_link in info_links){
  remDr$navigate(infor_link)
  event_page <- read_html(remDr$getPageSource()[[1]])
  event_page %>%
    html_nodes('h1') %>%
    html_text() -> event_title
  
  title_list_bock <- append(title_list_bock,event_title)
}

# shut down the browser
remDr$close()
rm(rD)
gc()

table_bock <- html_table(page_bock, fill=TRUE)

reg_date <- "[A-z]{2}[\\,/]\\s[0-9]{2}[\\./][0-9]{2}[\\./]20[1-2]{1}[0-9]{1}"
datum_bock <- str_extract_all(table_bock[[3]]$X1,reg_date) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) 
datum_bock <- datum_bock[!is.na(datum_bock)]

# format the date
for (i in datum_bock) {
  datum_bock=gsub("Mo, ","",datum_bock)
  datum_bock=gsub("Di, ","",datum_bock)
  datum_bock=gsub("Mi, ","",datum_bock)
  datum_bock=gsub("Do, ","",datum_bock)
  datum_bock=gsub("Fr, ","",datum_bock)
  datum_bock=gsub("Sa, ","",datum_bock)
  datum_bock=gsub("So, ","",datum_bock)
}
datum_bock <- as.Date(datum_bock, format = "%d.%m.%Y")
datum_bock

reg_uhrzeit <- "[0-9]{2}[\\:/][0-9]{2}\\sUhr"
uhrzeit_bock <- str_extract_all(table_bock[[3]]$X2,reg_uhrzeit) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub(" Uhr","",.)
uhrzeit_bock <- uhrzeit_bock[!is.na(uhrzeit_bock)]
uhrzeit_bock <- times(paste0(uhrzeit_bock, ":00"))

time_end_bock <- rep(NA,length(uhrzeit_bock)) %>%
  as.numeric() %>%
  times()

reg_ort <- "\\bOrt[\\:/]\\s[A-z]+"
ort_bock <- str_extract_all(table_bock[[3]]$X1,reg_ort) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
ort_bock <- ort_bock[!is.na(ort_bock)]
ort_bock <- gsub("Ort: ","",ort_bock)
ort_bock

veranstalter_bock <- rep("Bockshorn",length(uhrzeit_bock))

reg_preis <- '[0-9]{1,2}[\\,/][0-9]{1,2}.+'
preis_bock <- str_extract_all(table_bock[[3]]$X4,reg_preis) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
preis_bock <- preis_bock[!is.na(preis_bock)]
preis_bock

kurz_infor_bock <- rep(NA,length(uhrzeit_bock))
city_bock <- rep("Wuerzburg",length(uhrzeit_bock))
street_bock <- rep("Oskar-Laredo-Platz 1 (vormals Veitshöchheimer Straße 5a )",length(uhrzeit_bock))
zip_bock <- rep(97080,length(uhrzeit_bock))
lat_bock <- rep(49.8158268,length(uhrzeit_bock))
lng_bock <- rep(9.9102608,length(uhrzeit_bock))

veranstaltungen_bock <- data.frame(date_start=datum_bock,
                                   date_end=datum_bock,
                                   time_start=uhrzeit_bock,
                                   time_end=time_end_bock,
                                   title=title_list_bock,
                                   description=kurz_infor_bock,
                                   price=preis_bock,
                                   city=city_bock,
                                   street=street_bock,
                                   zip=zip_bock,
                                   organizer=veranstalter_bock,
                                   lat=lat_bock,
                                   lng=lng_bock,
                                   url=info_links)

##############################
# Jugendkulturhaus Cairo
##############################
url_cairo <- 'https://cairo.wue.de/home'
page_cairo <- read_html(url_cairo)

veran_cairo_html <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .titlefr')
veran_cairo <- html_text(veran_cairo_html)
head(veran_cairo)

time_cairo_html <- html_nodes(page_cairo,'time')
time_cairo <- html_text(time_cairo_html)
for (i in time_cairo) {
  time_cairo=gsub("Mo/","",time_cairo)
  time_cairo=gsub("Di/","",time_cairo)
  time_cairo=gsub("Mi/","",time_cairo)
  time_cairo=gsub("Do/","",time_cairo)
  time_cairo=gsub("Fr/","",time_cairo)
  time_cairo=gsub("Sa/","",time_cairo)
  time_cairo=gsub("So/","",time_cairo)
  time_cairo=gsub("/",".",time_cairo)
}

time_cairo <- paste(time_cairo,".2018") %>%
  gsub(" ","",.)
time_cairo <- as.Date(time_cairo, format = "%d.%m.%Y")

# get the event link
main_url_cairo <- 'https://cairo.wue.de'

detail_link_relative <- html_nodes(page_cairo, '#block-views-block-veranstaltungen-frontpage-block-1 .front-inner') %>%
  html_attr('href')
detail_link <- paste(main_url_cairo, detail_link_relative) %>%
  gsub(" ","",.)
detail_link

# Use RSelenium to get all detail
rD <- rsDriver()
remDr <- rD[["client"]]

uhrzeit_list <- c()
preis_list <- c()
beschreibung_list <- c()

for (event_url in detail_link){
  remDr$navigate(event_url)
  event_pg <- read_html(remDr$getPageSource()[[1]])
  
  event_pg %>%
    html_node('.col-sm-3') -> node_data
  
  event_pg %>%
    html_node('.field--name-field-abendkasse .field--item') %>%
    html_text() -> preis_1
  
  event_pg %>%
    html_node('body > div.main-container.container.js-quickedit-main-content > div > section > div.region.region-content > article > div > div:nth-child(1) > div.col-sm-3 > div.field.field--name-field-info.field--type-string.field--label-inline > div.field--item') %>%
    html_text() -> preis_2
  
  preis_puff <- paste(preis_1,preis_2) %>%
    gsub("NA","",.)
  preis_list <- append(preis_list,preis_puff)
  
  event_pg %>%
    html_node('p:nth-child(1)') %>%
    html_text() -> beschreibung_cairo 
  beschreibung_list <- append(beschreibung_list, beschreibung_cairo)
  
  node_data %>%
    html_node('.field--name-field-beginn .field--item') %>%
    html_text() %>%
    gsub(" Uhr","",.)-> uhrzeit_cairo_event
  if(!is.na(uhrzeit_cairo_event)){
    uhrzeit_cairo_event <- paste0(uhrzeit_cairo_event, ":00")
  }
  
  uhrzeit_list <- append(uhrzeit_list, uhrzeit_cairo_event)
  Sys.sleep(1)
}
uhrzeit_list <- times(uhrzeit_list)

# shut down the browser
remDr$close()
rm(rD)
gc()

preis_list %>%
  str_trim()

veranstalter_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))
time_end_cairo <- rep(NA,length(veran_cairo)) %>%
  as.numeric() %>%
  times()

ort_cairo <- rep("Jugendkulturhaus Cairo",length(veran_cairo))
city_cairo <- rep("Wuerzburg",length(veran_cairo))
street_cairo <- rep("Fred-Joseph-Platz 3",length(veran_cairo))
zip_cairo <- rep(97082,length(veran_cairo))
lat_cairo <- rep(49.7882055,length(veran_cairo))
lng_cairo <- rep(9.924654,length(veran_cairo))

veranstaltungen_cairo <- data.frame(
  date_start=time_cairo,
  date_end=time_cairo,
  time_start=uhrzeit_list,
  time_end=time_end_cairo,
  title=veran_cairo,
  description=beschreibung_list,
  price=preis_list,
  city=city_cairo,
  street=street_cairo,
  zip=zip_cairo,
  organizer = veranstalter_cairo,
  lat=lat_cairo,
  lng=lng_cairo,
  url=detail_link
)

##############################
# Theater Ensemble
##############################
url_theater <- 'http://theater-ensemble.net/spielplan/'
page_theater <- read_html(url_theater)

veran_theater_html <- html_nodes(page_theater,'.title a')
veran_theater <- html_text(veran_theater_html)
veran_theater

veran_theater_link <- html_nodes(page_theater,'.title a') %>%
  html_attr('href')
veran_theater_link

date_theater <- html_nodes(page_theater,'span:nth-child(1)') %>%
  html_text()
date_theater <- date_theater[-1]

for (datum_theater in date_theater){
  date_theater = gsub(", 18",".2018",date_theater)
  date_theater = gsub(" Jan","01",date_theater)
  date_theater = gsub(" Feb","02",date_theater)
  date_theater = gsub(" Mär","03",date_theater)
  date_theater = gsub(" Apr","04",date_theater)
  date_theater = gsub(" Mai","05",date_theater)
  date_theater = gsub(" Jun","06",date_theater)
  date_theater = gsub(" Jul","07",date_theater)
  date_theater = gsub(" Aug","08",date_theater)
  date_theater = gsub(" Sep","09",date_theater)
  date_theater = gsub(" Okt","10",date_theater)
  date_theater = gsub(" Nov","11",date_theater)
  date_theater = gsub(" Dez","12",date_theater)
}

date_theater <- as.Date(date_theater, format = "%d.%m.%Y")

time_theater <- html_nodes(page_theater,'.time') %>%
  html_text() %>%
  gsub(" Uhr","",.)
time_theater <- times(paste0(time_theater, ":00"))

preis_theater <- html_nodes(page_theater,'.meta') %>%
  html_text() %>%
  str_trim()

regex_preis_theater <- '\\d{1,2}.+'
preis_theater <- str_extract_all(preis_theater,regex_preis_theater) %>%
  as.character()

date_end_theater <- date_theater
ort_theater <- rep("Theater Ensemble",length(veran_theater))
time_end_theater <- rep(NA,length(veran_theater)) %>%
  as.numeric() %>%
  times()
veranstalter_theater <- rep("Theater Ensemble",length(veran_theater))
kurz_infor_theater <- rep(NA,length(veran_theater))
city_theater <- rep("Wuerzburg",length(veran_theater))
street_theater <- rep("Frankfurter Straße 87",length(veran_theater))
zip_theater <- rep(97082,length(veran_theater))
lat_theater <- rep(49.793289,length(veran_theater))
lng_theater <- rep(9.9283898,length(veran_theater))

veranstaltungen_theater <- data.frame(date_start=date_theater,
                                      date_end=date_end_theater,
                                      time_start=time_theater,
                                      time_end=time_end_theater,
                                      title=veran_theater,
                                      description=kurz_infor_theater,
                                      price=preis_theater,
                                      city=city_theater,
                                      street=street_theater,
                                      zip=zip_theater,
                                      organizer=veranstalter_theater,
                                      lat=lat_theater,
                                      lng=lng_theater,
                                      url=veran_theater_link)

##############################
# Uni Klinikum
##############################

klinik_link <- 'https://www.ukw.de/patienten-besucher/veranstaltungskalender/'

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(klinik_link)

# Smarter: Click button until all sites are loaded
xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "jscroll-next", " " ))]'

run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'xpath', value = xpath)$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

# Get Nodes
site %>%
  html_nodes(".ui-helper-reset") -> node_data

# Get Values
title_selector <- ".title"
from_date_selector <- ".date1"
to_date_selector <- ".date2"
time_selector <- ".time"
teaser_selector <- ".teaser"

node_data %>%
  html_node(title_selector) %>%
  html_text() -> title

title
node_data %>%
  html_node(from_date_selector) %>%
  html_text() %>%
  as.character() -> from_date

from_date <- as.Date(from_date, format = "%d.%m")

node_data %>%
  html_node(to_date_selector) %>%
  html_text() %>%
  gsub("- ","",.) -> to_date
to_date <- as.Date(to_date, format = "%d.%m.%Y")

regex_end_time <- '-\\s.+'

node_data %>%
  html_node(time_selector) %>%
  html_text() %>%
  gsub(regex_end_time,"",.) %>%
  gsub(" Uhr","",.) %>%
  str_trim() -> from_time
from_time

from_time_list = c()
for (sub_time in from_time){
  if(!is.na(sub_time)){
    sub_time <- paste0(sub_time, ":00")
  }
  from_time_list <- append(from_time_list, sub_time)
}

from_time_list <- times(from_time_list)
node_data %>%
  html_node(time_selector) %>%
  html_text() %>%
  as.character() -> time

time_end_klinik <- str_extract_all(time,regex_end_time) %>%
  as.character() %>%
  gsub(" Uhr","",.) %>%
  gsub("- ","",.) %>%
  gsub("character[\\(/]0[\\)/]",NA,.)

to_time_list <- c()
for (sub_to_time in time_end_klinik){
  if(!is.na(sub_to_time)){
    sub_to_time <- paste0(sub_to_time, ":00")
  }
  to_time_list <- append(to_time_list, sub_to_time)
}  
to_time_list <- times(to_time_list)

node_data %>%
  html_node(teaser_selector) %>%
  html_text() %>% 
  as.character() -> kurz_infor_klinik

ort_klinik <- rep(NA,length(title))
veranstalter <- rep("Universitätsklinikum Würzburg - Zentrum für Operative Medizin (ZOM)",length(title)) %>%
  as.character()

preis_klinik <- rep(NA,length(title)) %>%
  as.character()

city_klinik <- rep("Wuerzburg",length(title))
street_klinik <- rep("Oberdürrbacher Straße 6",length(title))
zip_klinik <- rep(97080, length(title))
lat_klinik <- rep(49.8058689, length(title))
lng_klinik <- rep(9.9570689, length(title))
info_link_klinik <- rep('https://www.ukw.de/patienten-besucher/veranstaltungskalender/', length(title))

# Merge
df_klinik <- data.frame(
  date_start =from_date,
  date_end=to_date,
  time_start = from_time_list,
  time_end= to_time_list,
  title = title,
  description=kurz_infor_klinik,
  price=preis_klinik,
  city=city_klinik,
  street=street_klinik,
  zip=zip_klinik,
  organizer=veranstalter,
  lat=lat_klinik,
  lng=lng_klinik,
  url=info_link_klinik
)

# Clean up
df_klinik %>%
  filter(!is.na(title)) -> df_klinik_clean


# Shut down selenium
remDr$close()
rm(rD)
gc()

##############################
# merging all calendars
##############################
final_eventcalender <- rbind(veranstaltungen_bock, veranstaltungen_cairo,veranstaltungen_theater)
final_eventcalender <- rbind(final_eventcalender, df_klinik_clean)

##############################
# Tsung_Wei
##############################

library(rvest)
library(tidyverse)
library(chron)


# Dom St. Kilian ---------------------------------------------------------------------

# First crawl
website <- "https://www.dom-wuerzburg.de/aktuelles/veranstaltungen"

website %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes("span") %>%
  html_text(trim = T) -> Zeit

raw_read %>%
  html_nodes(".eventcontent .title a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".title+ p") %>%
  html_text(trim = T) -> description

# Cleaning time info
Zeit <- Zeit[-c(1,2)]

# setting up data in iterative loop
for (x in 1:(length(Zeit))){
  # initializing
  if (x == 1){
    date_start = c()
    date_end = c()
    time_start = c()
    time_end = c()
    city = c()
    street = c()
    zip = c()
    organizer = c()
    price = c()
    lat = c()
    lng = c()
    url = c()
  }
  
  # formluating date format --------------------------------------------------------
  temp_date = trimws(strsplit(Zeit, "[,]")[[x]][2], "l")
  temp_date = gsub("\\.", "", temp_date)
  temp_date = gsub("[[:space:]]", ".", temp_date)
  
  # month tranform to date form
  { temp_date = gsub("Januar", "01", temp_date)
    temp_date = gsub("Februar", "02", temp_date)
    temp_date = gsub("M?rz", "03", temp_date)
    temp_date = gsub("April", "04", temp_date)
    temp_date = gsub("Mai", "05", temp_date)
    temp_date = gsub("Juni", "06", temp_date)
    temp_date = gsub("Juli", "07", temp_date)
    temp_date = gsub("August", "08", temp_date)
    temp_date = gsub("September", "09", temp_date)
    temp_date = gsub("Oktober", "10", temp_date)
    temp_date = gsub("November", "11", temp_date)
    temp_date = gsub("Dezember", "12", temp_date)
  }
  temp_date <- as.Date(temp_date, "%d.%m.%Y")
  date_start = c(date_start, temp_date)
  date_end = c(date_end, temp_date)
  
  
  # formluating time format --------------------------------------------------------
  temp_time = gsub("Uhr", "", strsplit(Zeit, "[,]")[[x]][3])
  temp_time = gsub("[[:space:]]", "", temp_time)
  temp_time = paste(temp_time, ":00")
  time_start = c(time_start, chron(times = times(temp_time)))
  time_end = c(time_end, NA)
  
  
  city = c(city, "W?rzburg")
  street = c(street, "DKiliansplatz 1")
  zip = c(zip, 97070)
  organizer = c(organizer, "Dom St. Kilian")
  price = c(price, NA)
  lat = c(lat, 49.79369)
  lng = c(lng, 9.93159)
  url = c(url, "https://www.dom-wuerzburg.de/aktuelles/veranstaltungen")
  
}

# final formation for date and time
date_start <- chron(dates = date_start)
date_end <- chron(dates = date_end)
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)


# creating list
eventkalendar_DSK <- data.frame(date_start, date_end, time_start, time_end, title, description, price, 
                                city, street, zip, organizer, lat, lng, url)


# Evang.-Luth. Dekanat W?rzburg -------------------------------------------

website <- "http://www.wuerzburg-evangelisch.de/aktuelles-termine"

website %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".teaserdate") %>%
  html_text(trim = T) -> Zeit

raw_read %>%
  html_nodes(".et_link_title") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".teaserplace") %>%
  html_text(trim = T) -> Ort

raw_read %>%
  html_nodes(".et_content_user") %>%
  html_text(trim = T) -> organizer


# setting up data
for (x in 1:(length(Zeit))){
  # initializing
  if (x == 1){
    date_start = c()
    date_end = c()
    time_start = c()
    time_end = c()
    city = c()
    street = c()
    zip = c()
    description = c()
    price = c()
    lat = c()
    lng = c()
    url = c()
  }
  
  # formluating date format --------------------------------------------------------
  temp_date <- paste(word(Zeit[x], 2), substr(Sys.Date(),1,4), sep = "")
  temp_date <- as.Date(temp_date, "%d.%m.%Y")
  date_start = c(date_start, temp_date)
  date_end = c(date_end, temp_date)
  
  # formluating start time --------------------------------------------------------
  temp_time_start = gsub( "-.*$", "", word(Zeit[x], 3))
  temp_time_start = if_else(grepl(":", temp_time_start), paste(temp_time_start, ":00", sep = ""), paste(temp_time_start, ":00:00", sep = ""))
  time_start = c(time_start, chron(times = times(temp_time_start)))
  
  # formluating end time--------------------------------------------------------
  temp_time_start = gsub( "-.*$", "", word(Zeit[x], 3))
  temp_time_end = gsub( ".*-", "", word(Zeit[x], 3))
  if (temp_time_end != temp_time_start){
    temp_time_end = if_else(grepl(":", temp_time_end), paste(temp_time_end, ":00", sep = ""), paste(temp_time_end, ":00:00", sep = ""))
    time_end = c(time_end, chron(times = times(temp_time_end)))
  } else {
    time_end = c(time_end, NA)
  }
  
  city = c(city, "W?rzburg")
  street = c(street, "Friedrich-Ebert-Ring 27b")
  zip = c(zip, 97072)
  description = c(description, paste("Die Veranstaltung findet in",Ort[x], "statt."))
  price = c(price, NA)
  lat = c(lat, 49.78743)
  lng = c(lng, 9.93818)
  url = c(url, "http://www.wuerzburg-evangelisch.de/aktuelles-termine")
  
}


# final formation for date and time
date_start <- chron(dates = date_start)
date_end <- chron(dates = date_end)
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# rearrange column order
eventkalendar_ELDW <- data.frame(date_start, date_end, time_start, time_end, title, description, price, 
                                 city, street, zip, organizer, lat, lng, url)


# Umweltbildung Unterfranken ----------------------------------------------

website <- "http://www.umweltbildung-unterfranken.de/index.php/veranstaltungen/week.listevents/"

website %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".ev_td_li") %>%
  html_text(trim = T) -> data_raw

raw_read %>%
  html_nodes(".jev_daysnames") %>%
  html_text(trim = T) -> days

raw_read %>%
  html_nodes(".currentmonth") %>%
  html_text(trim = T) -> month_raw


# Cleaning up
data_raw <- gsub("\t", "", data_raw)
data_raw <- gsub("\n", "", data_raw)


# Setting up data
for (x in 1:(length(data_raw))){
  # initialzing
  if (x == 1){
    date_start = c()
    date_end = c()
    time_start = c()
    time_end = c()
    title = c()
    city = c()
    street = c()
    zip = c()
    organizer = c()
    description = c()
    price = c()
    lat = c()
    lng = c()
    url = c()
    dateDetect = 0
    dateCount = 1
    number <- as.character(c(0:9))
  }
  
  # checking date
  if (substr(data_raw[x],1,1) %in% number){
    dateDetect = as.integer(substr(data_raw[x],1,2))
  } else {
    if (dateDetect > 0){
      dateDetect = 0
      dateCount = dateCount + 1
      next
    }
    ifelse(dateDetect == 0, next)
  }
  
  # setting up date
  temp_date = paste(days[dateCount], substr(month_raw, nchar(month_raw)-3, nchar(month_raw)))
  
  # month tranform to digit form
  { temp_date = gsub("Januar", "01", temp_date)
    temp_date = gsub("Februar", "02", temp_date)
    temp_date = gsub("M?rz", "03", temp_date)
    temp_date = gsub("April", "04", temp_date)
    temp_date = gsub("Mai", "05", temp_date)
    temp_date = gsub("Juni", "06", temp_date)
    temp_date = gsub("Juli", "07", temp_date)
    temp_date = gsub("August", "08", temp_date)
    temp_date = gsub("September", "09", temp_date)
    temp_date = gsub("Oktober", "10", temp_date)
    temp_date = gsub("November", "11", temp_date)
    temp_date = gsub("Dezember", "12", temp_date)
  }
  temp_date = gsub("\\.", "", temp_date)
  temp_date = gsub("[[:space:]]", ".", temp_date)
  
  # formluating date format --------------------------------------------------------
  temp_date <- as.Date(temp_date, "%d.%m.%Y")
  date_start = c(date_start, temp_date)
  date_end = c(date_end, temp_date)
  
  # formluating time format --------------------------------------------------------
  temp_time_start = substr(data_raw[x], 1, 5)
  temp_time_end = substr(data_raw[x], 9, 13)
  temp_time_start = paste(temp_time_start, ":00")
  temp_time_end = paste(temp_time_end, ":00")
  time_start = c(time_start, chron(times = times(temp_time_start)))
  time_end = c(time_end, chron(times = times(temp_time_end)))
  
  
  # setting up the rest data
  city = c(city, "W?rzburg")
  street = c(street, "Zeller Stra?e 44")
  zip = c(zip, 97082)
  description = c(description, paste("Der Veranstaltung findet in", gsub("[[:space:]][[:space:]]", "", strsplit(data_raw, "::", fixed = T)[[x]][2]), "statt"))
  price = c(price, NA)
  organizer = c(organizer, "Umweltbildung Unterfranken")
  lat = c(lat, 49.79471)
  lng = c(lng, 9.91973)
  url = c(url, "http://www.umweltbildung-unterfranken.de/index.php/veranstaltungen/week.listevents/")
  
  # setting event by removing blank spaces
  title = c(title, gsub("(^\\s+)|(\\s+$)", "", gsub("([[:punct:]])|(\\d)", "",strsplit(data_raw, "::", fixed = T)[[x]][1])))
  
}

# final formation for date and time
date_start <- chron(dates = date_start)
date_end <- chron(dates = date_end)
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)


# Creating list
eventkalendar_UU <- data.frame(date_start, date_end, time_start, time_end, title, description, price, 
                               city, street, zip, organizer, lat, lng, url)


# merging all calendars ---------------------------------------------------

eventkalendar <- rbind(eventkalendar_DSK, eventkalendar_ELDW, eventkalendar_UU)

#Set local to germany to get german month names
Sys.setlocale("LC_ALL","German_Germany")

#Create the DF for all events
events <- NULL
events <- setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("date_start",
                                                              "date_end",
                                                              "time_start",
                                                              "time_end",
                                                              "title",
                                                              "description",
                                                              "price",
                                                              "city",
                                                              "street",
                                                              "zip",
                                                              "organizer",
                                                              "lat",
                                                              "lng",
                                                              "url"))


#########################################################################################################################
#                                     BEGIN CCW
#########################################################################################################################
#put the url in a var 
url <-"https://www.wuerzburg.de/events-termine/"
#Get the select options for the location
values <- read_html(url)%>%html_nodes(xpath='//*[(@id = "loclist")]/option')
#Get the value for CCW to parse it later to the form
value <-values[grep("Congress Centrum Würzburg",values)]%>%html_attr("value")
#Get the Form to submit it with values for CCW
f <- read_html(url)%>%html_node("form#ktfrm")%>%html_form()

#Get Todays Date and break it up into dd mm yyyy

d <- as.numeric(format(Sys.Date(),'%d'))
m <- as.numeric(format(Sys.Date(),'%m'))
y <- as.numeric(format(Sys.Date(),'%Y'))


#Fill the form with value for CCW todays Date and end Date = + 1 year
f<- f%>%set_values("ev[addr]"= value,
                   "ev[start][d]"=d,
                   "ev[start][m]"=m,
                   "ev[start][y]"=y,
                   "ev[end][d]"=d,
                   "ev[end][m]" =m,
                   "ev[end][y]"=y+1,
                   "ev[cat][]" = 0)

#Open a session
s <- html_session(url)
#Submit the form
sf <- submit_form(s, f)
#access the new page build up be the form and take all event links
links <-read_html(sf$url)%>%html_nodes("a.eventblock")%>%html_attr("href")

#build a tmp df
tmp <- NULL
tmp.name <- ""
tmp <- setNames(data.frame(matrix(ncol = 14, nrow = 1)),  colnames(events))
#Access the Event page for every Event and put the Data in a DF
for(i in 1:length(links)){
  #Get the Name of the Event
  name <- read_html(paste0("https://www.wuerzburg.de/",links[i]))%>%
    html_nodes("h1")%>%
    magrittr::extract2(2)%>%html_text()
  
  text <-  read_html(paste0("https://www.wuerzburg.de/",links[i]))%>%
    html_nodes(".ktlg_teaser")%>%html_text()
  
  #Get Date and Time of the Event
  string <-read_html(paste0("https://www.wuerzburg.de/",links[i]))%>%
    html_nodes(".triggerbox-inner")%>%
    magrittr::extract2(1)%>%html_text()
  #Extract the Date out of the String
  dt <-str_extract_all(string,"[0-9]{2}.[0-9]{2}.[0-9]{4}")[[1]]%>%as.Date(format="%d.%m.%Y")
  
  #Extract the Time out of the String
  tm <- str_extract(string, "[0-9]{2}:[0-9]{2}")
  #Check if there is a time to format it correctly
  if(!is.na(tm)){
    tm <- tm%>%{paste0(.,":00")}%>%times()
  }else{
    tm <- tm%>%times()
  }
  
  #get the price
  price <- str_extract(string,"Vorverkauf\\:(.+?)(\x80)")
  
  #Chekc if I already have the event because events longer then 1 day will be posted for every day after the event with start end date
  if(tmp.name != name){
    if(length(dt)>1){
      tmp$date_start  <- dt[1]
      tmp$date_end    <- dt[2]
    }else{
      tmp$date_start  <- dt
      tmp$date_end    <- dt
    }
    tmp$time_start  <- tm
    tmp$time_end    <- NA
    tmp$title       <- name
    tmp$description <- text
    tmp$price       <- price
    tmp$city        <-"Wuerzburg"
    tmp$street      <- "Am Congress Centrum"
    tmp$zip         <- as.numeric("97070")
    tmp$organizer   <-"Congress Centrum Würzburg"
    tmp$lat         <- as.numeric(49.798477)
    tmp$lng         <- as.numeric(9.926392)
    tmp$url         <- links[i]
    events <- rbind(events, tmp)
    #if event longer then 1 day save the name to prevent the loop to put it in the df for the single days
    if(length(dt)>1){
      tmp.name <- name
    }#if
  }#if
}#for
#format time correctly
events$time_start <- events$time_start%>%times()
#########################################################################################################################
#                                     END CCW
#########################################################################################################################

#########################################################################################################################
#                                     BEGIN OMNIBUS
#########################################################################################################################
#put Url in var
url <- "http://www.omnibus-wuerzburg.de"

#Navigate to page with events
omb <- html_session(url)%>%follow_link("a",i="Programm")
#Get event Names
names <- omb%>%html_nodes("[itemprop=summary]")%>%html_text()
#Get event start Date and Time
startDate <- omb%>%html_nodes("[itemprop=startDate]")%>%html_text()
#Get event end Date and Time
endDate <- omb%>%html_nodes("[itemprop=endDate]")%>%html_text()
links <- omb%>%html_nodes(".eo-event-title")%>%html_nodes("a")%>%html_attr("href")

for(i in 1:length(names)){
  
  if(as.Date(startDate[i],format="%d. %B %Y") - as.Date(endDate[i],format="%d. %B %Y")<2){
    tmp$date_start  <- as.Date(startDate[i],format="%d. %B %Y")
    tmp$date_end    <- as.Date(endDate[i],format="%d. %B %Y")
    tmp$time_start  <- paste0(str_extract(startDate[i], "[0-9]{2}:[0-9]{2}"),":00")%>%times()
    tmp$time_end    <- paste0(str_extract(endDate[i], "[0-9]{2}:[0-9]{2}"),":00")%>%times()
    tmp$title       <- names[i]
    tmp$description <- NA
    tmp$price       <- NA
    tmp$city        <-"Wuerzburg"
    tmp$street      <- "Theaterstraße 10"
    tmp$zip         <- as.numeric("97070")
    tmp$organizer   <-"Omnibus Würzburg"
    tmp$lat         <- as.numeric(49.7963122)
    tmp$lng         <- as.numeric(9.9342999)
    tmp$url         <- links[i]
    
  }#if
  else{
    #TODO: If event longer than 1 Day make a second entry
  }#else
  
  events <-rbind(events,tmp)
}#for
events$time_end <- events$time_end%>%times()
#########################################################################################################################
#                                     END OMNIBUS
#########################################################################################################################


#########################################################################################################################
#                                     BEGIN VKU
#########################################################################################################################

url <- "http://vku-kunst.de/"

links <- read_html(url)%>%html_nodes("a.ai1ec-read-more")%>%html_attr("href")
cat <- read_html(url)%>%html_nodes(".ai1ec-color-swatch")%>%html_attr("title")
tmp.name=""
for(i in 1:length(links)){
  name <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
  if(tmp.name != name){
    if(cat[i]!="Konzert"){
      #read part where start & end Date is hidden
      dt <- read_html(links[i])%>%html_node(".su-column-inner")%>%html_nodes("p")%>%html_text()
      #get part where Date is 
      dt <- lapply(dt, function(x) if(str_detect(x,"Ausstellung")) x)%>%unlist
      #pattern for Date extraction
      p="\\s*(?:\\b\\d{4}\\b)|(?:\\b\\d{1,2}\\s*[/\\.-]\\s*\\d{1,2}\\s*[/\\.-]\\s*(?:\\d{4}|\\d{2})\\b)|\\b\\d{1,2}\\s*[/\\.-]?\\s*(?:Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember|(?:Jan|Feb|Febr|Mar|Apr|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec).?)" 
      #extract Date
      dt <-str_extract_all(dt, pattern=p)%>%unlist
      if(length(dt)>1){
        tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
        tmp$date_end    <- dt[2]%>%as.Date(format="%d. %B")
      }else{
        tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
        tmp$date_end    <- dt[1]%>%as.Date(format="%d. %B")
      }
      
      tmp$time_start <- times("11:00:00")
      tmp$time_end <- times("18:00:00")
      tmp$title <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
      string <-read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")
      tmp$description <- string[3]%>%html_text()
      tmp$price       <- NA
      tmp$city        <-"Wuerzburg"
      tmp$street      <- "Zeller Straße 1"
      tmp$zip         <- as.numeric("97082")
      tmp$organizer   <-"Spitäle"
      tmp$lat         <- as.numeric(49.79288)
      tmp$lng         <- as.numeric(9.92413)
      tmp$url         <- links[i]
    }else{
      #read part where start & end Date is hidden
      dt <- read_html(links[i])%>%html_node(".su-column-inner")%>%html_nodes("p")%>%html_text()
      #get part where Date is 
      dt <- lapply(dt, function(x) if(str_detect(x,"Konzert am")) x)%>%unlist
      #pattern for Date extraction
      p="\\s*(?:\\b\\d{4}\\b)|(?:\\b\\d{1,2}\\s*[/\\.-]\\s*\\d{1,2}\\s*[/\\.-]\\s*(?:\\d{4}|\\d{2})\\b)|\\b\\d{1,2}\\s*[/\\.-]?\\s*(?:Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember|(?:Jan|Feb|Febr|Mar|Apr|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec).?)" 
      #extract time
      tm <- str_extract_all(dt, pattern="\\b[0-9]{2} Uhr")%>%unlist
      #extract date
      dt <-str_extract_all(dt, pattern=p)%>%unlist
      tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
      tmp$date_end    <- dt[1]%>%as.Date(format="%d. %B")
      tmp$time_start <- times(paste0(str_extract(tm, "[0-9]{2}"),":00:00"))
      tmp$time_end <- NA
      tmp$title <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
      string <- read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")
      tmp$description <- string[2]%>%html_text()
      price <-read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")%>%html_text
      price <- lapply(price, function(x) if(str_detect(x,"Eintritt")) x)%>%unlist
      endpos<-regexpr(pattern = ")", text = price, ignore.case = TRUE)
      price <- substr(price, 0 , endpos)
      tmp$price       <- price
      tmp$city        <-"Wuerzburg"
      tmp$street      <- "Zeller Straße 1"
      tmp$zip         <- as.numeric("97082")
      tmp$organizer   <-"Spitäle"
      tmp$lat         <- as.numeric(49.79288)
      tmp$lng         <- as.numeric(9.92413)
      tmp$url         <- links[i]
    }#else
    events <- rbind(events, tmp)
  }#if
  
  tmp.name <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
}#for

events$time_end <- events$time_end%>%times()

#########################################################################################################################
#                                     END VKU
#########################################################################################################################


# Merging all three scripts of us

eventkalendar <- rbind(end_eventcalender,final_eventcalender, eventkalendar, events, stringsAsFactors = F)

eventkalendar$time_end <- eventkalendar$time_end%>%times()

write.csv(eventkalendar, file = "Eventkalendar.csv", row.names=FALSE)


