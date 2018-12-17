#####---4: IGZ: Innovations-&Gründerzentrum --------------------------------------------------------------------------####

library(RCurl)
library(RJSONIO)
library(tidyverse)
library(rvest)

##TITLE
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes(".c_title a") %>%
  html_text -> title_igz

##URL (for more information)
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes(".c_title a") %>%
  html_attr("href") ->links_igz
creating_links <- function(link) {
  url <- paste0("https://www.igz.wuerzburg.de", link)
}
url_igz <- sapply(links_igz, creating_links)


## META DATA
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes(".ev_title_verysimple") %>%
  html_text() ->meta_igz

##DATE
str_extract_all(meta_igz, "[0-9]?[0-9]\\.[0-9]?[0-9]\\.2018") -> date_start_igz
unlist(date_start_igz) -> date_start_igz
date_start_igz <- as.Date(date_start_igz, format="%d.%m.%Y")
date_end_igz <- date_start_igz

##TIME
get_time_start <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("(?<=Uhrzeit:\\s)[0-9][0-9]:[0-9][0-9]") 
}
sapply(url_igz, get_time_start)-> time_igz
time_igz <- unlist(time_igz)
names(time_igz) <- NULL
time_start_igz <- times(paste0(time_igz,":00"))

get_time_end <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("((?<=Ende:\\s)[0-9]?[0-9].[0-9][0-9])|((?<=[0-9]?[0-9]\\s(h)\\s(-)\\s)[0-9]?[0-9].[0-9][0-9](?=\\s(h)))") 
}
sapply(url_igz, get_time_end)-> time_end_igz
sapply(time_end_igz,function(x){gsub("\\.",":",x)}) -> time_end_igz
times(paste0(time_end_igz,":00")) -> time_end_igz



##DESCRIPTION
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes(".ev_teaser") %>%
  html_text(trim=T) -> description_igz
  gsub("\t","",description_igz) -> description_igz
  gsub("/r","",description_igz) -> description_igz

##LOCATION
city_igz <- "Wuerzburg"
street_igz <- "Friedrich-Bergius-Ring 15" 
zip_igz <- "97076"
lng_igz <- 9.9981454
lat_igz <- 49.8034612
organizer_igz <- "Innovations- und Gründerzentrum Würzburg"

##PRICE
get_price <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("([0-9]+,[0-9]+.(?=€))|(kostenfrei)") 
}
sapply(url_igz, get_price)-> price_igz
gsub("character\\(0\\)",NA,price_igz) -> price_igz
unlist(price_igz) -> price_igz


##DATA FRAME
df_igz <- data.frame(title=title_igz, url=url_igz, description=description_igz,lng=lng_igz,lat=lat_igz,
                     city=city_igz,street=street_igz,zip=zip_igz,date_start=date_start_igz,date_end=date_end_igz,
                     time_start=time_start_igz,time_end=time_end_igz,price=price_igz,organizer=organizer_igz)
rownames(df_igz) <- NULL

#-------
##PPROBLEME
#-mal wieder: viel regex! Leider kaum Struktur. Bspw. Infos über den Eintritt sind nur indirekt im Fließtext
#erwähnt (...die "kostenlose" Veranstaltung...). Falls neue Artikel eine andere Beschreibung der Kosten haben,
#findet der simple Algo diese nicht mehr.







