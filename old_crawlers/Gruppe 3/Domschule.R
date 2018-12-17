library(tidyverse)
library(rvest)
library(tidyr)
library(plyr)
library(lubridate)
library(qdapRegex)
library(RSelenium)
library(chron)


Dom <- function(){

#---------Domschule----------------------
do_url <- "https://www.domschule-wuerzburg.de/akademie/alle-veranstaltungen"
do_titel_voll <- vector()
do_links_voll <- vector()
do_datum_voll <- vector()

rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(do_url)

#Vor Aktualisierung gab es zwei Seiten mit Veranstaltungen
run <- TRUE
i <- 1
while (i <= 1){
  tryCatch(
    remDr$findElement(using = 'css selector', "#topcontrol")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  raw_data <- read_html(remDr$getPageSource()[[1]])
  raw_data %>%
    html_nodes(".eventcontent a") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> do_titel
  do_titel_voll <- c(do_titel_voll, do_titel)
  do_titel_voll <- do_titel_voll[seq(1, length(do_titel_voll), 2)]
  
  raw_data %>%
    html_nodes(".eventcontent a") %>%
    html_attr("href")-> do_links
  do_links_voll <- c(do_links_voll, do_links)
  do_links_voll <- do_links_voll[seq(1, length(do_links_voll), 2)]
  
  raw_data %>%
    html_nodes(".blog-info li:nth-child(1)") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> do_datum
  do_datum_voll <- c(do_datum_voll, do_datum)
  
  i <- i + 1
  Sys.sleep(3)
}

# Shut down selenium
remDr$close()
rm(rD)
gc()

#Links erweitern
do_links_voll <- paste0("http:", do_links_voll)


df_do <- data.frame(title= do_titel_voll, url = do_links_voll, date_do = do_datum_voll, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205)

#Datum 
#Wochentag entfernen
df_do$date_do <- str_replace_all(df_do$date_do, "([A-z]+, )", "")
#Monate durch Zahlen ersetzen
df_do$date_do <- str_replace_all(df_do$date_do," Januar ","01.")
df_do$date_do <- str_replace_all(df_do$date_do," Februar ","02.")    
df_do$date_do <- str_replace_all(df_do$date_do," M?rz ","03.")  
df_do$date_do <- str_replace_all(df_do$date_do," April ","04.")
df_do$date_do <- str_replace_all(df_do$date_do," Mai ","05.")  
df_do$date_do <- str_replace_all(df_do$date_do," Juni ","06.")  
df_do$date_do <- str_replace_all(df_do$date_do," Juli ","07.")  
df_do$date_do <- str_replace_all(df_do$date_do," August ","08.")
df_do$date_do <- str_replace_all(df_do$date_do," September ","09.")  
df_do$date_do <- str_replace_all(df_do$date_do," Oktober ","10.")  
df_do$date_do <- str_replace_all(df_do$date_do," November ","11.") 
df_do$date_do <- str_replace_all(df_do$date_do," Dezember ","12.") 
#Aufspalten Start, Ende
df_do <- separate(data=df_do, col=date_do, into=c("date_start", "date_end"), sep="\\- ")
df_do <- separate(data=df_do, col=date_start, into=c("date_start", "time_start"), sep="\\, ")
df_do$time_end <- str_extract_all(df_do$date_end, "[0-9]+:[0-9]+")
df_do$date_end <- str_extract_all(df_do$date_end, "([0-9]+.[0-9]+.[0-9]+)")
#character(0) -> NA
df_do$date_end <- as.character(df_do$date_end)
for(i in 1:length(df_do$date_end)){
  if(identical(df_do[i,]$date_end, "character(0)")){
    df_do[i,]$date_end<- NA
  }
}
#Enddatum = Startdatum falls na
for (i in 1:nrow(df_do)){
  if (is.na(df_do[i,]$date_end)){
    df_do[i,]$date_end<- df_do[i,]$date_start
  }
}
#dates to date Format
df_do$date_end <- as.Date(df_do$date_end,format='%d.%m.%Y')
df_do$date_start <- as.Date(df_do$date_start,format='%d.%m.%Y')


#Zeit
df_do$time_start <- str_extract_all(df_do$time_start, "[0-9]+:[0-9]+")
df_do$time_start <- paste0(df_do$time_start, ":00")
df_do$time_start <- times(df_do$time_start)
df_do$time_end <- paste0(df_do$time_end, ":00")
df_do$time_end <- times(df_do$time_end)

#Ort
df_do$city = "W?rzburg"
df_do$zip = 97070
df_do$street = "Am Bruderhof 1"

#Kosten aus Link
links_d<- as.character(df_do$url)
getDataKosten <- function(links_d){
  links_d %>%
    read_html() %>%
    html_nodes(".f_cost .f_value") %>%
    html_text(trim = T) -> kosten
}
Kosten_List1 <- map(links_d, getDataKosten) 
Kosten_List1 <- unlist(Kosten_List1, use.names=FALSE)
df_do$price <- Kosten_List1

df_do$description <- NA 
df_do$organizer <- "Domschule W?rzburg"

df_do_to <- df_do[c("title","url", "description","lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price","organizer")]

#--------------------------------------------------

return(df_do_to)
}