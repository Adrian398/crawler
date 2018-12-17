#Immherin

immerhin_crawler <- function(){
  
  library(tidyverse)
  library(rvest)
  library(lubridate)
  library(chron)
  
  url_immerhin <- "http://www.immerhin-wuerzburg.de/"
  nodes_immerhin <- "section:nth-child(1) li :nth-child(1)"
  
  raw_crawler <- function(url, nodes) {
    read_html(url) %>%
      html_nodes(nodes) %>%
      html_text(trim = TRUE) 
  } 
  
  #Raw Data
  raw_immerhin <- raw_crawler(url_immerhin, nodes_immerhin)
  
  #Dataframe erstellen
  tidy_immerhin <- raw_immerhin %>%
    as.tibble()
  tidy_immerhin <- tidy_immerhin[!apply(tidy_immerhin == "", 1, all),] 
  tidy_immerhin <- as.data.frame(split(tidy_immerhin, 1:2))
  tidy_immerhin <- tidy_immerhin %>% 
    rename(date_start = value, title = value.1) 
  tidy_immerhin$date_start <- str_sub(tidy_immerhin$date_start, 4)
  tidy_immerhin <- tidy_immerhin %>%
    separate(date_start, c("date_start", "description"), sep = " ") %>% 
    mutate(url = url_immerhin) %>%
    mutate(lng = "9.93173") %>%
    mutate(lat = "49.80186") %>%
    mutate(city = "Wuerzburg") %>%
    mutate(street = "Bahnhofplatz 2 (Posthalle)") %>% 
    mutate(zip = "97080") %>%
    mutate("date_end" = date_start) %>%
    mutate("time_start" = NA) %>%
    mutate("time_end" = NA) %>%
    mutate(price = NA) %>%
    mutate(organizer = "Immerhin") 
  
  #Anpassung des Inhalts
  tidy_immerhin$date_start <- parse_date_time(tidy_immerhin$date_start, "d!.m!.y!")
  tidy_immerhin$date_end <- parse_date_time(tidy_immerhin$date_end, "d!.m!.y!")
  
  #Anpassung der Datenformate
  tidy_immerhin$lng <- as.numeric(tidy_immerhin$lng)
  tidy_immerhin$lat <- as.numeric(tidy_immerhin$lat)
  tidy_immerhin$zip <- as.numeric(tidy_immerhin$zip)
  tidy_immerhin$time_start <- times(tidy_immerhin$time_start)
  tidy_immerhin$time_end <- times(tidy_immerhin$time_end)
  tidy_immerhin$price <- as.character(tidy_immerhin$price)
  tidy_immerhin$date_start <- as.Date(tidy_immerhin$date_start)
  tidy_immerhin$date_end <- as.Date(tidy_immerhin$date_end)
  
  #Reihenfolge der Spalten
  tidy_immerhin <- tidy_immerhin[,c(3,4,2,5,6,7,8,9,1,10,11,12,13,14)]
  
  return(tidy_immerhin)
  
}