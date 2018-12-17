library(rvest)
library(tidyverse)
library(devtools)
library(RSelenium)
library(chron)

url = "http://www.kirche-frauenland.de"

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com")
remDr$navigate(url)

run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'css selector', ".nav-text")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

url %>%
  read_html() -> rawData1


rawData1 %>%
  html_nodes(".itemtitle a") %>% 
  html_text() -> title
title

#Suche ob bis vorhanden
rawData1 %>%
  html_nodes(".datetime") %>% 
  html_text() -> alldates
alldates
length(alldates) -> l
str_detect(alldates, "\\d+[.]\\d{2}[.]\\d{4}[ ][bis].* ") -> a
paste(a,collapse=" ") -> a
b <- c(rep(FALSE, l))
paste(b,collapse=" ") -> b

#if kein bis gefunden, alle daten auslesen; else bis vorhanden
if(a==b){
  t = TRUE
  rawData1 %>%
    html_nodes(".date") %>% 
    html_text() -> startdate
  enddate <- startdate
  
  startdate <- as.POSIXct(startdate,format="%d.%m.%Y")
  enddate <- as.POSIXct(enddate,format="%d.%m.%Y")
  
} else{
  
  rawData1 %>%
    html_nodes(".datetime") %>% 
    html_text() -> alldates
  stringr::str_extract_all(alldates, "[0-9]*[.]\\d{2}[.]\\d{4}") -> startdate
  startdate
  
  df_datum <- as.data.frame.list(startdate) 
  df_datum
  
  as.vector(df_datum[1,]) -> startdate
  as.vector(df_datum[2,]) -> enddate
  
  do.call("paste",df_datum[1,]) -> startdate
  startdate
  stringr::str_extract_all(startdate, "\\d{2}[.]\\d{2}[.]\\d{4}") -> startdate
  unlist(startdate) -> startdate

  do.call("paste",df_datum[2,]) -> enddate
  enddate
  stringr::str_extract_all(enddate, "\\d{2}[.]\\d{2}[.]\\d{4}") -> enddate
  unlist(enddate) -> enddate

  startdate <- as.POSIXct(startdate,format="%d.%m.%Y")
  enddate <- as.POSIXct(enddate,format="%d.%m.%Y")
}


rawData1 %>%
  html_nodes(".itemcontent") %>% 
  html_text() -> beschreibung

rawData1 %>%
  html_nodes(".datetime") %>% 
  html_text() -> time
time
stringr::str_extract_all(time, ".*\\d+[:]\\d{2}") -> datetime
unlist(datetime) -> datetime
stringr::str_extract_all(datetime, "\\d{2}[.]\\d{2}[.]\\d{4}") -> datum
unlist(datum) -> datum
stringr::str_extract_all(datetime, "\\d+[:]\\d{2}") -> time
unlist(time) -> time
datum
datum <- as.POSIXct(datum,format="%d.%m.%Y")
datum
time <- times(paste0(time, ":00"))
time
df_datetime <- data.frame(date_start = datum, Zeit = time)



# Link --------------------------------------------------------------------

rawData1 %>%
  html_nodes(".itemtitle a") %>% 
  html_attr("href") -> link
gsub("//", "www.", link) -> link1
length(link1) -> l


c(rep("Wuerzburg", l)) -> city

c(rep("Pfarreiengemeinschaft St. Barbara und Unsere Liebe Frau", l)) -> veranstalter



# Dataframe ---------------------------------------------------------------
df1 <- data.frame (title = title, url = link1, description = beschreibung , lng = NA, lat = NA, city = city, street = NA, zip = NA, date_start = startdate, date_end = enddate, time_end = NA, price= NA, organizer = veranstalter)

df2 <- left_join(df1, df_datetime, by="date_start")
df2[-(2:3),] -> df2

df <- data.frame (title = title, url = link1, description = beschreibung , lng = NA, lat = NA, city = city, street = NA, zip = NA, date_start = startdate, date_end = enddate,time_start = df2$Zeit, time_end = NA, price= NA, organizer = veranstalter)

# Shut down selenium
remDr$close()
rm(rD)
gc()
