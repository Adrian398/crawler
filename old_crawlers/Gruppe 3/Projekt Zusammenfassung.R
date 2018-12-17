library(rvest)
library(tabulizer)
library(RCurl)
library(RJSONIO)
library(tidyverse)
library(readxl)
library(ggplot2)
library(GGally)
library(tidyr)
library(stringr)
library(tm)
library(RSelenium)
library(xlsx)
library(chron)


source('ZOM2.R',  encoding = "UTF-8")
source('MH.R',  encoding = "UTF-8")
source('AGK.R',  encoding = "UTF-8")
source('BOOT.R',  encoding = "UTF-8")
source('HFM.R',  encoding = "UTF-8")

Zusammenfassung_D <- function(){
EventsDennis <- data.frame(matrix(ncol=14,nrow=0))

Eventtabelle <- ZOM()
ZOM_Ev <- Eventtabelle
EventsDennis <- rbind(EventsDennis, Eventtabelle)

Eventtabelle <- MH()
ZOM_MH <- Eventtabelle
EventsDennis <- rbind(EventsDennis, Eventtabelle)

Eventtabelle <- AGK()
ZOM_AGK <- Eventtabelle
EventsDennis <- rbind(EventsDennis, Eventtabelle)

Eventtabelle <- BOOT()
ZOM_BOOT <- Eventtabelle
EventsDennis <- rbind(EventsDennis, Eventtabelle)

Eventtabelle <- HFM()
ZOM_HFM <- Eventtabelle
EventsDennis <- rbind(EventsDennis, Eventtabelle)


EventsDennis <- unique(EventsDennis)
#Dublikate entfernen

EventsDennis$date_start <- as.character(EventsDennis$date_start)
EventsDennis$date_end <- as.character(EventsDennis$date_end)

EventsDennis <- EventsDennis %>%
  mutate(date_end = if_else(is.na(date_end),date_start,date_end))
#EndDatum = Startdatum setzen, falls NA in Enddatum
Sicherung <- EventsDennis
EventsDennis <- Sicherung

#Jahr in Startdatum hinzufügen wenn in Enddatum enthalten
EventsDennis <- EventsDennis %>%
  mutate(l1 = if_else(str_sub(date_start, -4, -3) == "20", date_start, "Jahr"))

EventsDennis <- EventsDennis %>%
  mutate(lel = if_else(str_sub(date_end, -4, -3) == "20", str_sub(date_end, -4, -1), "Falsch"))

EventsDennis <- EventsDennis %>%
  mutate(date_start = if_else(l1 == "Jahr" & lel != "Falsch", paste(date_start,".",lel, sep = ""),date_start))

#Wenn kein Jahr vorhanden
currentMonth <- as.character(Sys.Date())  #for getting current system date eg:2012-11-06
Monat <- str_sub(currentMonth, 6, 7)
Monat <- as.numeric(Monat)
Jahr <- str_sub(currentMonth, 1, 4)
Jahr <- as.numeric(Jahr)

EventsDennis <- EventsDennis %>%
  mutate(date_start = if_else(l1 == "Jahr" & lel == "Falsch" & as.numeric(str_sub(date_start,4,5)) >= Monat, paste(date_start,".",Jahr,sep = ""), date_start))

EventsDennis <- EventsDennis %>%
  mutate(date_start = if_else(l1 == "Jahr" & lel == "Falsch" & as.numeric(str_sub(date_start,4,5)) < Monat, paste(date_start,".",Jahr+1,sep = ""), date_start))

EventsDennis <- EventsDennis %>%
  mutate(date_end = if_else(l1 == "Jahr" & lel == "Falsch" & as.numeric(str_sub(date_end,4,5)) >= Monat, paste(date_end,".",Jahr,sep = ""), date_end))

EventsDennis <- EventsDennis %>%
  mutate(date_end = if_else(l1 == "Jahr" & lel == "Falsch" & as.numeric(str_sub(date_end,4,5)) < Monat, paste(date_end,".",Jahr+1,sep = ""), date_end))

EventsDennis <- select(EventsDennis,-l1)
EventsDennis <- select(EventsDennis,-lel)

#Format als Date
EventsDennis$date_start <- as.character(EventsDennis$date_start)
EventsDennis$date_start <- as.Date(EventsDennis$date_start, '%d.%m.%Y')

EventsDennis$date_end <- as.character(EventsDennis$date_end)
EventsDennis$date_end <- as.Date(EventsDennis$date_end, '%d.%m.%Y')

#Format als Time
EventsDennis$time_start <- as.character(EventsDennis$time_start)
EventsDennis$time_start <- times(paste(EventsDennis$time_start,":00",sep = ""))

EventsDennis$time_end <- as.character(EventsDennis$time_end)
EventsDennis$time_end <- times(paste(EventsDennis$time_end,":00",sep = ""))

return(EventsDennis)
}

#Als Excel speichern
write.xlsx(EventsDennis, file = "C:/Users/Denni/Documents/Wü Master/Applied Data Science/Projekt/DennisEvent1706.xlsx")
write.csv(EventsDennis, file = "C:/Users/Denni/Documents/Wü Master/Applied Data Science/Projekt/FinaleAbgabe1706.csv")

