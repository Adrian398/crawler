library(tidyverse)
library(rvest)
library(gsubfn)
library(chron)

urls <- paste0("https://www.uni-wuerzburg.de/sonstiges/veranstaltungskalender/zeitraum/2018/", 4:12)

pages <- function(url){

url <- url 
url %>%
  read_html() %>%
  html_nodes(".news-list__item") -> node_data

##TITLE
node_data %>%
  html_node(".news-list__item-header a") %>%
  html_text() -> title

##DATE
node_data %>%
  html_node(".news-single__item-value") %>%
  html_text() -> datum

datum_bereinigt <- str_extract_all(datum, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")
datum_bereinigt <- as.matrix(as.data.frame(datum_bereinigt))

date_start <- (datum_bereinigt[1,])
date_start <- as.Date(date_start, format="%d.%m.%Y")
date_end <- (datum_bereinigt[2,])
date_end <- as.Date(date_end, format="%d.%m.%Y")


##DESCRIPTION
node_data %>%
  html_node(".news-list__item-event-location .news-list__item-value") %>%
  html_text(trim=T) -> ort
description <- paste0("Veranstaltungsort: ",ort)

##ORGANIZER
node_data %>%
  html_node(".news-list__item-event-organizer .news-list__item-value , .news-list__item-event-organizer a") %>%
  html_text(trim=T) -> organizer

##PRICE
node_data %>%
  html_node(".news-list__item-header a") %>%
  html_attr("href") ->links
url_filler <- function(x) {
  if(!grepl("http://www.",x)){
    paste0("https://www.uni-wuerzburg.de",x)
  }
  else {
    x
  }
}
lapply(links,url_filler) -> links
unlist(links) -> links

get_price <- function(link){
  url <- link
  url %>%
    read_html() %>%
    html_nodes(".bodytext") %>%
    html_text() -> body
  paste0(body, collapse = " ") -> body
  str_extract_all(body, ".+[0-9]?,?[0-9](?=\\sEuro)")
}
sapply(links, get_price) -> price
gsub("character\\(0\\)",NA,price) -> price

##TIME
time_bereinigt <- str_extract_all(datum, "\\d{1,2}\\:\\d{1,2}")
time_bereinigt <- as.matrix(as.data.frame(time_bereinigt))
time_start <- time_bereinigt[1,]
times(paste0(time_start,":00")) ->time_start
time_end <- time_bereinigt[2,]
times(paste0(time_end,":00")) -> time_end

##LOCATION INFOS
info <- (rep(url,length(title)))
link <- (rep(url,length(title)))
lng <- 9.935561
lat <- 49.787891
city <- "Wuerzburg"
street <- "Sanderring 2"
zip <- 97070

df <- data.frame(title=title,url=link,description=description,lng=lng,lat=lat,
                 city=city,street=street,zip=zip,
                 date_start=date_start,date_end=date_end,time_start=time_start,
                 time_end = time_end,price=price,organizer=organizer)


return(df)
}

df <- lapply(urls, pages)
df_uni <- rbind(df[[1]],df[[2]],df[[3]],df[[4]],df[[5]],df[[6]],df[[7]],df[[8]],df[[9]])
