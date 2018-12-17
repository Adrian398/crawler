Gnadenkirche <- function(){
library(rvest)
library(tidyr)
library(tidyverse)
library(stringr)
library(rowr)
library(lubridate)
library(chron)  
library(rowr)

url <- "http://www.gnadenkirche-wuerzburg.de/Termine.html"
Eventtabelle <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Eventtabelle) <- c("X1","X2","Uhrzeit","art")

count <- c(1:10)
count2 <- c(1:10)

bereiche = function(j,url){
  
  url %>%
    read_html() -> raw_data
  colnames(Eventtabelle) <- c("X1","X2")
  string2 <- as.character(paste('//*[@id="article-290"]/div/div[',j,']', sep = ""))
  string2
  raw_data2 <- raw_data
  raw_data2 %>%
    html_nodes(xpath = string2) -> raw_data3
  return(raw_data3)
}       

tabeling = function(i,raw_data3){ 
  
  raw_data3 %>%
    html_node("h2") %>%
    html_text() -> vsb
  vsb <- data.frame(vsb)
  uhrzeit <- as.character(str_extract_all(vsb$vsb,"[0-9]{2}:[0-9]{2}"))
  art <-  gsub("\\s*\\([^\\)]+\\)","",as.character(vsb$vsb))
  
  string <- as.character(paste("table[",i+1,"]", sep = ""))
  raw_data3 %>%
    html_nodes(xpath = string) %>%
    html_table() -> text
  text <- data.frame(text)
  text$uhrzeit <- rep(uhrzeit,nrow(text)) 
  text$art <- rep(art,nrow(text))

  return(text)
}  

for (j in count2) {
  raw_data3 = bereiche(j,url)
  for (i in count) {
    text <- tabeling(i,raw_data3)
    Eventtabelle <- rbind(Eventtabelle,text)
    assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
  }
}
Eventtabelle$X1 <- as.character(str_extract(Eventtabelle$X1,"[0-9]{2}.[0-9]{2}"))


# --- Einlesen des unteren Bereiches

Eventtabelle2 <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Eventtabelle2) <- c("X1","X2","Uhrzeit","art")

url %>%
  read_html() -> raw_data

raw_data %>%
  html_node('.ce_text.last') %>%
  html_node('table') %>%
  html_table() -> vsbb
vsbb <- data.frame(vsbb)

count <-nrow(vsbb)

for (i in 1:count) {
  test <- vsbb$X2[i]
  datum <- str_extract_all(test,"[0-3][0-9][^ :-][0-3][0-9]")
  date <- unlist(datum)
  
  zaehler <- character(0)
  if(identical(date, character(0))){
    if(str_detect(test, "Sonntag")){
      searchday <- 7
    }
    if(str_detect(test, "Samstag")){
      searchday <- 6
    }
    if(str_detect(test, "Freitag")){
      searchday <- 5
    }
    if(str_detect(test, "Donnerstag")){
      searchday <- 4
    }
    if(str_detect(test, "Mittwoch")){
      searchday <- 3
    }
    if(str_detect(test, "Dienstag")){
      searchday <- 2
    }
    if(str_detect(test, "Montag")){
      searchday <- 1
    }
    heute <- Sys.Date()
    zaehler <- heute
    numdayh <- wday(zaehler)
    numnaechst <- searchday - numdayh +1
    datenaechst <- zaehler + numnaechst
    zaehler <- datenaechst
    day <- substring(datenaechst, 9,10)
    monat <- substring(datenaechst, 6,7)
    naechstevent <- paste0(day,'.',monat,collapse = NULL)
    date <- c(date, naechstevent)
  }
  
  text <- vsbb$X1[i]
  
  uhrzeit <- as.character(str_extract(test,"[0-9]+.[0-9]{2}-[0-9]{2}.[0-9]{2}"))
  
  if (is.na(uhrzeit)) {
    uhrzeit <- as.character(str_extract(test,"[0-9]+.[0-9]{2}"))
  }
  Beschreibung <- NA
  bla <- cbind.fill(date, Beschreibung, uhrzeit,text)
  colnames(bla) <- c("X1","X2","uhrzeit","art")
  Eventtabelle2 <- rbind(Eventtabelle2,bla)
}
Eventtabelle2 <- Eventtabelle2[-nrow(Eventtabelle2),] 

#----------------------------------------------------------------------
KompletteEventtabelle <- rbind(Eventtabelle,Eventtabelle2)
colnames(KompletteEventtabelle) <- c("Startdatum","description","Uhrzeit","title")
KompletteEventtabelle$Startdatum <-  gsub(",",".",KompletteEventtabelle$Startdatum)
KompletteEventtabelle$city <- rep("Würzburg",nrow(KompletteEventtabelle))
KompletteEventtabelle$zip <- rep(97072,nrow(KompletteEventtabelle))
KompletteEventtabelle$street <- rep("Danziger Straße 10",nrow(KompletteEventtabelle))
KompletteEventtabelle$Long <- rep(9.93654,nrow(KompletteEventtabelle)) 
KompletteEventtabelle$Lat <- rep(49.77727,nrow(KompletteEventtabelle))
KompletteEventtabelle$url <- rep("http://www.gnadenkirche-wuerzburg.de/Termine.html",nrow(KompletteEventtabelle))
KompletteEventtabelle$price <- rep(NA,nrow(KompletteEventtabelle))
KompletteEventtabelle$organizer <- rep("Gnadenkirche Würzburg",nrow(KompletteEventtabelle))


KompletteEventtabelle <- separate(data = KompletteEventtabelle, col = Uhrzeit, into = c("Startzeit","Endzeit"), sep = "-")
KompletteEventtabelle <- separate(data = KompletteEventtabelle, col = Startdatum, into = c("Startdatum","Enddatum"), sep = "-")

KompletteEventtabelle$Startdatum <- as.Date(KompletteEventtabelle$Startdatum, format= "%d.%m")
KompletteEventtabelle$Enddatum <- as.Date(KompletteEventtabelle$Enddatum, format= "%d.%m")

KompletteEventtabelle$Startzeit <- paste(KompletteEventtabelle$Startzeit,':00', sep = '')
#KompletteEventtabelle$Startzeit <- times(KompletteEventtabelle$Startzeit)

for (i in 1:nrow(KompletteEventtabelle)) {
  
  if(is.na(KompletteEventtabelle$Enddatum[i])){
    KompletteEventtabelle$Enddatum[i] <- KompletteEventtabelle$Startdatum[i]
  }
  assign('KompletteEventtabelle',KompletteEventtabelle,envir = .GlobalEnv)

}
for (i in 1:nrow(KompletteEventtabelle)) {
  
  if(!is.na(KompletteEventtabelle$Endzeit[i])){
    KompletteEventtabelle$Endzeit[i] <- paste(KompletteEventtabelle$Endzeit[i],':00', sep = '')
    KompletteEventtabelle$Endzeit[i] <- as.character(times(KompletteEventtabelle$Endzeit[i]))
    
  }
}

return(KompletteEventtabelle)
}