#Zusammenfassung alle Gruppen
library(xlsx)
library(googleway)
library(rowr)
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


source("Lindleinsmuehle_final.R")
source("FabLab_final.R")
source("VHS-final.R")
source("Domschule.R")
source("Kulturspeicher.R")
source("IHK.R")
source("Thomaskirche.R")
source('ZOM2.R',  encoding = "UTF-8")
source('MH.R',  encoding = "UTF-8")
source('AGK.R',  encoding = "UTF-8")
source('BOOT.R',  encoding = "UTF-8")
source('HFM.R',  encoding = "UTF-8")
source("FHWSNeu.R")
source("Gnadenkirche Wuerzburg.R")
source("PGSanderauRSelenium.R")
source("VersuchEinzelseiteBBK_Unterfranken.R")
source("CrawlAllSitesLukas.R")
source("Projekt Zusammenfassung.R")

EventsGesamt<- data.frame(matrix(ncol = 14, nrow = 0))

EventtabelleDom <- Dom()
EventsGesamt<- rbind(EventsGesamt,EventtabelleDom)

EventtabelleVHS <- VHS()
EventsGesamt <- rbind(EventsGesamt,EventtabelleVHS)

EventtabelleFab <- Fab()
EventsGesamt <- rbind(EventsGesamt,EventtabelleFab)

EventtabelleLin <- Lin()
EventsGesamt <- rbind(EventsGesamt,EventtabelleLin)

EventtabelleKultur <- Kultur()
EventsGesamt <- rbind(EventsGesamt,EventtabelleKultur)

EventtabelleIHK <- IHK()
EventtabelleIHK$time_start<- NA
EventtabelleIHK$time_start<- times(EventtabelleIHK$time_start)
EventtabelleIHK$time_end<- NA
EventtabelleIHK$time_end<- times(EventtabelleIHK$time_end)


EventsGesamt <- rbind(EventsGesamt,EventtabelleIHK)


EventtabelleThomas <- Thomas()
EventsGesamt<- rbind(EventsGesamt,EventtabelleThomas)

EventstabelleD<- data.frame(matrix(ncol = 14, nrow = 0))
EventtabelleD <- Zusammenfassung_D()
EventsGesamt <- rbind(EventsGesamt, EventtabelleD)

EventstabelleL<- data.frame(matrix(ncol = 14, nrow = 0))
EventtabelleL <- EventCrawlerLukas()
EventsGesamt <- rbind(EventsGesamt, EventtabelleL)

write.xlsx(EventsGesamt, file = "EventsGesamt.xlsx")
write.csv(EventsGesamt, file = "EventsGesamt.csv")

require(feather)
EventsGesamt$city <- unlist(EventsGesamt$city)
write_feather(EventsGesamt, "Events_Gruppe3")
