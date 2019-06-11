#We want to crawl all events from today to the end of the next year

#First, we need the dates to construct our base url
paste(substr(Sys.Date(),9,10),substr(Sys.Date(),6,7),substr(Sys.Date(),1,4),sep=".") -> today

Sys.Date() %>%
  format("%Y") %>%
  as.numeric()+1 -> next_year

paste("31","12",next_year,sep=".") -> end_next_year

julius_url <- paste0("https://www.juliusspital.de/aktuelles/veranstaltungskalender/index.html?ev%5Bstart%5D=",today,"&ev%5Bend%5D=",end_next_year,"&ev%5Bsearch%5D=&ev%5Bcat%5D=&ev%5Bsubcat%5D")

#crawl maximum page number from site
julius_url %>%
  read_html %>%
  html_nodes("div .pager") %>%
  html_nodes("a") %>%
  html_attr("rel") %>%
  str_extract_all("page[0-9]{1,2}") %>%
  str_extract_all("[0-9]{1,2}") %>%
  as.numeric() %>%
  max() -> max_page_no

#create event calender page urls from crawled number
paste0(julius_url, "=&page=", seq(from=0, to=(max_page_no-1))) -> julius_page_links

#function to crawl event urls from calendar pages
julius_get_event_urls <- function(url){
  url %>%
    read_html() %>%
    html_nodes(".contentwidth div") %>%
    html_nodes(".evdetails") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.list()-> cast
}

#crawl all pages for event links
#may take some time (<1 minute)
lapply(julius_page_links, julius_get_event_urls) -> julius_base_event_urls

#convert event links to list for further use
julius_base_event_urls %>%
  unlist() -> julius_base_event_urls_list

#create full urls from base urls
paste0("https://www.juliusspital.de/",julius_base_event_urls_list) -> julius_full_event_urls_list

#function to crawl event pages for details

#crawl part of event page relevant for us
url %>%
  read_html() %>%
  html_nodes("div .eventdetail") -> crawled_html

#get title
crawled_html %>%
  html_nodes(".ev-title") %>%
  html_text() -> title

#get description
crawled_html %>%
  html_nodes(".ev-content") %>%
  html_text() -> description

description %>%
  str_extract("[0-9]+,-") -> price

#Set description to NA if none is available
if(description=="Zurück"){
  description <- NA
}

#get date/time in unformatted form
crawled_html %>%
  html_nodes(".col-md-6") %>%
  html_text() -> datetime_base

#extract date
datetime_base[1] %>%
  str_extract("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}") %>%
  dmy(tz="Europe/Berlin") + (24*60*60)  -> date_start_base

date_start_base %>%
  as.Date() -> date_start

#extract time
datetime_base[1] %>%
  str_extract("[0-9]{1,2}:[0-9]{1,2}") %>%
  paste0(":00") -> time_start_base

time_start <- try(times(time_start_base))

if("try-error" %in% class(time_start)) {
  time_start <- NA
}

#get location of event
crawled_html %>%
  html_nodes(".evadress") %>%
  html_text() -> location_string

#get coordinates from google maps if address available, else choose default cooridnates
if(!is.na(str_extract(location_string, "[0-9]{5}"))){
  geocode(location_string) -> coordinates
  str_extract(location_string, "[0-9]{5}.*") -> city_base
  str_extract(city_base, "[0-9]{5}") %>%
    as.numeric() -> zip
  str_extract(city_base, "[^0-9]+.+") %>%
    as.character() -> city
  data.frame(coordinates, zip, city) -> location
}

#if no coordinates found or no address available, use default coordinates
if(is.na(str_extract(location_string, "[0-9]{5}")) || is.na(coordinates)) {
  lon <- 9.92992
  lat <- 49.7979
  zip <- 97070
  city <- "Wuerzburg"
  data.frame(lon, lat, zip, city) -> location
}

#ggmaps returns longitude in column "lon", so we have to convert names
names(location) <- c("lng","lat","zip","city")

#this information is not provided, added to get our common data format
date_end <- as.Date(NA)
time_end <- NA
organizer <- "Juliusspital Würzburg"
#bind information and cast
cbind(title,date_start,time_start,date_end,time_end,description,price,url,location, organizer) -> cast


#get all event infos and output a data frame
#may take a LONG time to crawl due to high number of events (~200 for 18 months when created)
