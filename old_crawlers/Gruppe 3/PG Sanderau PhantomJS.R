PGSanderau <- function() {

options(stringsAsFactors = FALSE)
library(rvest)
library(tidyverse)
library(stringr)
library(httr)
library(dplyr)


Eventtabelle <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(Eventtabelle) <- c("title", "description", "Startdatum","Enddatum", "Startzeit", "Endzeit","url","price","organizer","city","street","zip")

Events <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(Eventtabelle) <- c("Links")


  url <- paste0("http://www.pg-sanderau.de/aktuelles---termine/")
  lines <- readLines("scrape_final.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "scrape_final.js")

  system("phantomjs scrape_final.js")
  
  pg <- read_html("1.html")
  pg %>%
    html_node('.pagenumber') %>%
    html_nodes('a') %>%
    html_attr("href")-> urls

   

urls <- paste0("http://www.pg-sanderau.de/aktuelles---termine/",urls)


gettinglinks = function(urls){
  
  lines <- readLines("scrape_final.js")
  lines[1] <- paste0("var url ='", urls ,"';")
  writeLines(lines, "scrape_final.js")
  
  system("phantomjs scrape_final.js")
  
  try <- read_html("1.html")
  
  try %>%
    html_nodes(".itemtitle a")%>%
    html_attr("href") -> Links
  Links <- data.frame(Links)
  Links$Links <- paste("http:", Links$Links, sep="")
  return(Links)  
}

crawling = function(links) {
  
  linkad <- links
  versuch <- links %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("p") %>%
    html_text() -> Beschreibung
  Beschreibung  <- Beschreibung[2]
  
  raw_data %>%
    html_nodes(".datetime") %>%
    html_text() -> Datum
  Datum <- Datum[2]
  Datum <-  strsplit(Datum, ",", fixed=TRUE)

  dates <- as.character(str_extract_all(Datum,"[0-9]{2}.[0-9]{2}.[0-9]{4}", simplify = TRUE)) #
  uhrzeiten <- as.character(str_extract_all(Datum,"[0-9]+:[0-9]{2}", simplify = TRUE)) #
  
  dates <- data.frame(dates)
  colnames(dates) <- c("Datum")
  uhrzeiten <- data.frame(uhrzeiten)
  colnames(uhrzeiten) <- c("Uhrzeiten")
  startzeit <- uhrzeiten$Uhrzeiten[1]
  endzeit <- uhrzeiten$Uhrzeiten[2]
  
  Startdatum <- dates$Datum[1]
  Startdatum <- as.Date(Startdatum, format= "%d.%m.%Y")
  Enddatum <- dates$Datum[2]
  Enddatum <- as.Date(Enddatum, format= "%d.%m.%Y")
  
  if(is.na(Enddatum)){ #
    Enddatum <- Startdatum #
  } #
  
  ort <- NA #Kann nicht ausgelesen werden
  
  raw_data %>%
    html_nodes("h1") %>%
    html_text() -> Titel
  Titel <- Titel[2]
  
  price <- NA
  organizer <- "PG Sanderau"
  
  startzeit <- gsub(".",":",startzeit, fixed = TRUE)
  startzeit <- as.character(startzeit)
  endzeit <- gsub(".",":",endzeit, fixed = TRUE)
  endzeit <- as.character(endzeit)
  if(!is.na(startzeit)){
    startzeit <- paste(startzeit,':00', sep = '')
    #startzeit <- times(startzeit)
  }
  
  if(!is.na(endzeit)){
    endzeit <- paste(endzeit,':00', sep = '')
    endzeit <- as.character(times(endzeit))

  }
  city <- "Würzburg"
  street <- "Traubengasse 27"
  zip <- 97072
  
  test <- data.frame(Titel,Beschreibung,Startdatum,Enddatum, startzeit, endzeit,linkad,price,organizer,city,street,zip)
  colnames(test) <- c("title", "description", "Startdatum","Enddatum", "Startzeit", "Endzeit","url","price","organizer","city","street","zip")
  
  return(test)
}
for (i in urls) {
  Links = gettinglinks(i)
  Events <- rbind(Events,Links)
  assign('Events',Events,envir=.GlobalEnv)
}

for (i in Events$Links) {
  test = crawling(i)
  Eventtabelle <- rbind(Eventtabelle,test)
  assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
}
Eventtabelle$Long <- rep(9.93632,nrow(Eventtabelle)) 
Eventtabelle$Lat <- rep(49.78475,nrow(Eventtabelle)) 

return(Eventtabelle)
}