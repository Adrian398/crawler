LGS_crawler <- function(){

library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(chron)

# Anzahl Seiten bestimmen
LGS_pages="https://www.lgs2018-wuerzburg.de/veranstaltungen/veranstaltungskalender/index.html?page=0"
LGS_pages %>%
   read_html() %>%
   html_nodes(".pager")%>%
   html_text()  %>% 
  str_extract("[0-9]{1,2} Seiten") %>% 
  str_extract("[0-9]{1,2}") -> pages


lgs_singlePage <- function(url) {
  
  #url <- "https://www.lgs2018-wuerzburg.de/veranstaltungen/veranstaltungskalender/index.html?page=0"
  
  url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes(".event") -> node_data
  
  title_selector <- ".ev-title"
  datum_selector <- ".period-start"
  time_start <- ".time-start"
  time_end <- ".time-end"
  #kategorie_selector <- ".eventcat"
  link_selector <- ".ev-title"
  
  
  node_data %>%
    html_nodes(title_selector) %>%
    html_text(trim = T) -> title
  
  node_data %>%
    html_nodes(datum_selector) %>%
    html_text(trim = T) -> datum
  
  node_data %>%
    html_node(time_start) %>%
    html_text(trim = T) -> time_start
  
  node_data %>%
    html_node(time_end) %>%
    html_text(trim = T) -> time_end
  
  #node_data %>%
    #html_nodes(kategorie_selector) %>%
   # html_text(trim = T) -> kategorie
    
  node_data %>%
      html_nodes(link_selector) %>%
      html_attr("href")-> raw_link
  
  url <- paste0("https://www.lgs2018-wuerzburg.de", raw_link)
  
  #time_start , entfernen
  
  time_start %>%
    str_replace(",", "") %>%
    str_replace_all("[:space:]{1,}","") -> time_start
  
  


  
  #time_end - entfernen
  
  time_end %>%
    str_replace("-", "") %>%
    str_replace_all("[:space:]{1,}","") -> time_end
  
  
  #datum zu date-type
  
  
  dmy(datum) -> date_start
  
  
  date_end <- date_start
  
  date_end
  
  
  as.Date(date_end) -> date_end
  
  
  #price für Tageskarte
  
  price <- "Tageskarte: 10,50"
  
  #function get_description
  
  get_description <- function(event_url) {
  
    #event_url = "https://www.lgs2018-wuerzburg.de/veranstaltungen/veranstaltungskalender/index.html?page=0&ev%5Bid%5D=1716049&page=0"
    event_url %>%
      read_html() -> raw_event_data
    
    raw_event_data %>%
      html_nodes(".event-teaser") %>%
      html_text(trim = T) -> description
    
    df_singlePage <- data.frame(description, event_url)
  
    return(df_singlePage)
  }
  
  map_dfr(url, get_description) -> event_daten
  
  event_daten$description -> description
  
  event_daten$event_url -> url
  
  description %>%
    as.character() -> description
  
  url %>%
    as.character() -> url
  
  #Kategorie optional
  
  organizer <- "Landesgartenschau Wuerzburg 2018"
  
  #Adresse im Moment noch hardcoded - Lookup table wird erstellt und dann nachgetragen
  
  lng <- 9.9684822
  
  lat <- 49.7867022
  
  city <- "Wuerzburg"
  
  street <- "Kitzinger Straße 100"
  
  zip <- 97074
  
  df <- data.frame(title, url, description, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)

  return(df)
}  



liste <- seq(1:pages)-1



url_pages <- "https://www.lgs2018-wuerzburg.de/veranstaltungen/veranstaltungskalender/index.html?page="

paste0(url_pages, liste) -> url_pages

map_dfr(url_pages, lgs_singlePage) -> df_pages

#time zu times format mit chron package
df_pages$time_start <- times(paste0(df_pages$time_start, ":00"))
df_pages$time_end <- times(paste0(df_pages$time_end, ":00"))

#Factors zu chr casten

df_pages$title <- as.character(df_pages$title)
df_pages$city <- as.character(df_pages$city)
df_pages$street <- as.character(df_pages$street)
df_pages$organizer <- as.character(df_pages$organizer)
df_pages$price <- as.character(df_pages$price)

return(df_pages)


}

