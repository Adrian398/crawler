library(tidyverse)
library(rvest)
library(tidyr)
library(plyr)
library(lubridate) 
library(qdapRegex)
library(RSelenium)
library(chron)


Lin <- function(){
#--------------------Lindleinsmuehle----------------------------------------


li_url <- "http://lindleinsmuehle.pg-albert-jakobus.de/neuaktuelles/#page1"
li_titel_voll <- vector()
li_datum_voll <- vector()
li_links_voll <- vector()

rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(li_url)

run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'css selector', ".nav-next .ui-icons-block")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  raw_data <- read_html(remDr$getPageSource()[[1]])
  raw_data %>%
    html_nodes("#eventslist .itemtitle a") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> li_titel
  li_titel_voll <- c(li_titel_voll, li_titel)
  
  raw_data %>%
    html_nodes(".datetime") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> li_datum
  li_datum_voll <- c(li_datum_voll, li_datum)
 
  raw_data %>%
    html_nodes("#eventslist .itemtitle a") %>%
    html_attr("href")-> li_links
  li_links_voll <- c(li_links_voll, li_links)
  
  i <- i + 1
  Sys.sleep(5)
  
}

# Shut down selenium
remDr$close()
rm(rD)
gc()

#Links erweitern
li_links_voll <- paste0("http:", li_links_voll)



#Kopfzeile
li_url%>%
  read_html() %>%
  html_nodes(".calendarcontainer .itemtitle a") %>%
  html_text() -> titel2
  if(identical(titel2, character(0))) {
    titel2 <- NA
  }

li_url %>%
  read_html() %>%
  html_nodes(".calendarcontainer .date , .function") %>%
  html_text(trim = T) -> datum2
if(identical(datum2, character(0))){
  datum2 <- NA
}

  li_url %>%
    read_html() %>%
    html_nodes(".calendarcontainer .itemtitle a") %>%
    html_attr("href")-> links2
  if(identical(links2, character(0))){
    links2 <- NA
}

 
  
links2 <- paste0("http:", links2)
datum2 <- cbind(Datum = datum2[1],Startuhrzeit = datum2[2])


#Data frame Tabelle
df_li <- data.frame(title= li_titel_voll, url = li_links_voll, date = li_datum_voll, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205, organizer = "Pfarrei St. Albert W?rzburg - Lindleinsm?hle")


#Data frame Kopfzeile
df_li2 <- data.frame(title = titel2, url = links2, date = datum2, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205, organizer = "Pfarrei St. Albert Wuerzburg - Lindleinsmuehle")

colnames(df_li2)[4] <- "time_start"
colnames(df_li2)[3] <- "date"  

#Spalte Datum aufteilen
df_li<-separate(data=df_li, col=date, into=c("date", "time_start"), sep = "\\,")

#Dataframes verbinden
df_li_to <- rbind(df_li2, df_li)

#Erste Zeile, falls leer l?schen
if(is.na(df_li_to$title[1])){
  df_li_to <- df_li_to[-1,]
}

#Spalte Datum in Start- und Enddatum aufteilen
df_li_to <- separate(data=df_li_to, col=date, into=c("date_start", "date_end"), sep = "bis ")

#Spalte adresse aufspalten in city, street, zip
df_li_to <- separate(data=df_li_to, col=adresse, into=c("street", "zipcity"), sep="\\, ")
df_li_to$zip<- str_extract_all(df_li_to$zipcity, "([0-9]+)")
df_li_to$city<- str_extract_all(df_li_to$zipcity, "([A-z,?]+)")

df_li_to$date_start<- str_replace_all(df_li_to$date_start, "[A-z]*[\\,] ","")


#Spalte Beschreibung einf?gen
df_li_to$time_end <- NA
df_li_to$price <- NA
df_li_to$description <- NA
df_li_to <- df_li_to[c("title", "url", "description", "lng","lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")]


#Enddatum = Startdatum falls na
for (i in 1:nrow(df_li_to)){
  if (is.na(df_li_to[i,]$date_end)){
    df_li_to[i,]$date_end<- df_li_to[i,]$date_start
  }
}

# Enduhrzeit = Startuhrzeit falls na
for (i in 1:nrow(df_li_to)){
  if (is.na(df_li_to[i,]$time_end)){
    df_li_to[i,]$time_end <- as.character(df_li_to[i,]$time_start)
  }
}

#Zip to numeric
df_li_to$zip <- as.numeric(df_li_to$zip)

#dates to date Format
df_li_to$date_end <- as.Date(df_li_to$date_end,format='%d.%m.%Y')
df_li_to$date_start <- as.Date(df_li_to$date_start,format='%d.%m.%Y')

#time to times Format
df_li_to$time_end <- str_extract_all(df_li_to$time_end,"[0-9]{2}:[0-9]{2}")
for (i in 1:nrow(df_li_to)){
  if (is.na(df_li_to[i,]$time_end)){
  }
  else {
    df_li_to[i,]$time_end<- paste0(df_li_to[i,]$time_end,":00")
  }
}
df_li_to$time_end <- times(df_li_to$time_end)

df_li_to$time_start <- str_extract_all(df_li_to$time_start,"[0-9]{2}:[0-9]{2}")
for (i in 1:nrow(df_li_to)){
  if (is.na(df_li_to[i,]$time_start)){
  }
  else {
      df_li_to[i,]$time_start<- paste0(df_li_to[i,]$time_start,":00")
    }
  }
df_li_to$time_start <- times(df_li_to$time_start)

#----------------------------------------------------------------------
return(df_li_to)

}


