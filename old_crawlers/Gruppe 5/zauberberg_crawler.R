#Zauberberg

zauberberg_crawler <- function(){
  
  library(tidyverse)
  library(rvest)
  library(lubridate)
  library(chron)
  
  url_zauberberg <- "http://www.zauberberg.info/cls_blu-e.cfm?cmd=04"
  nodes_zauberberg <- ".text div+ div , .teaser div"
  
  raw_crawler <- function(url, nodes) {
    read_html(url) %>%
      html_nodes(nodes) %>%
      html_text(trim = TRUE) 
  } 
  
  #Raw Data
  raw_zauberberg <- raw_crawler(url_zauberberg, nodes_zauberberg)
  
  #Dataframe erstellen
  tidy_zauberberg <- as.data.frame(split(raw_zauberberg, 1:6))
  tidy_zauberberg <- tidy_zauberberg %>%
    select(date_start = X1, title = X3, description = X5) %>%
    separate(title, c("title", "time_start"), sep = " ab ") %>%
    mutate(url = url_zauberberg) %>%
    mutate(lng = "9.9217") %>%
    mutate(lat = "49.80271") %>%
    mutate(city = "Wuerzburg") %>%
    mutate(street = "Veitshoechheimer Str. 20") %>% 
    mutate(zip = "97080") %>%
    mutate(date_end = date_start) %>%
    mutate(time_end = NA) %>%
    mutate(price = NA) %>%
    mutate(organizer = "Zauberberg")
  
  #Anpassung des Inhalts 
  tidy_zauberberg$title <- gsub("\\s+$", "", tidy_zauberberg$title)
  tidy_zauberberg$title <- substr(tidy_zauberberg$title, 1, nchar(tidy_zauberberg$title)-1)
  
  tidy_zauberberg$time_start <- gsub("^\\s+", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- str_sub(tidy_zauberberg$time_start, 1, 9) 
  tidy_zauberberg$time_start <- gsub("Uhr", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- gsub("\\s+$", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- paste0(tidy_zauberberg$time_start, ":00") 
  tidy_zauberberg$time_start <- substr(tidy_zauberberg$time_start, 0, 5)
  
  tidy_zauberberg$date_start <- str_sub(tidy_zauberberg$date_start, 4)
  tidy_zauberberg$date_start <- dmy(tidy_zauberberg$date_start)
  tidy_zauberberg$date_end <- str_sub(tidy_zauberberg$date_end, 4)
  tidy_zauberberg$date_end <- dmy(tidy_zauberberg$date_end)
  
  #Anpassung der Datenformate 
  tidy_zauberberg$description <- as.character(tidy_zauberberg$description)
  tidy_zauberberg$lng <- as.numeric(tidy_zauberberg$lng)
  tidy_zauberberg$lat <- as.numeric(tidy_zauberberg$lat)
  tidy_zauberberg$zip <- as.numeric(tidy_zauberberg$zip)
  tidy_zauberberg$time_end <- as.character(tidy_zauberberg$time_end)
  tidy_zauberberg$price <- as.character(tidy_zauberberg$price)
  tidy_zauberberg$time_start <- times(paste0(tidy_zauberberg$time_start, ":00"))
  tidy_zauberberg$time_end <- times(tidy_zauberberg$time_end)
  
  
  #Reihenfolge der Spalten
  tidy_zauberberg <- tidy_zauberberg[,c(2,5,4,6,7,8,9,10,1,11,3,12,13,14)] 
  
  return(tidy_zauberberg)

}