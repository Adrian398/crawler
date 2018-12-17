library(rvest)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(httr)
library(lubridate)
library(devtools)
library(RSelenium)
library(chron)
library(stringi)
#devtools::install_github("dkahle/ggmap")
library(ggmap)
library(feather)

#Set working directory
#setwd("~/Desktop/Scraper")

#Deutsche Zeit
Sys.setlocale("LC_TIME", "de_DE") #MAC
#Sys.setlocale("LC_ALL","German") #WINDOWS


# API Key 1
register_google(key = "AIzaSyC_lvgZae1kGJ5QWEz2wz3ExZJmBbOzbZ4")


# Puppentheater Kasperhaus, Uniklinikum Würzburg, Evan.-Luth. Auferstehungskirche, Galerie Gabriele Müller
# 165, 166, 168, 170
# korrekte RSelenium Version installieren
# Out: df_all
source("165-170.R")

# API Key 2
register_google(key = "AIzaSyDOjGt9LQMihlMnknjDY49Qgwe2m_vqr9g")

# Standard, Labyrinth
# 159, 167
# Facebook Scraper: Universallösung funktioniert auch mit allen anderen Veranstaltern
# Links können im Scraper selektiert werden. Momentan: Standard & Labyrinth
# Out: finalFacebookDf
source("facebook_159&167.R")

# Bind 1
df_all <- rbind(df_all, finalFacebookDf)

# Wuf Zentrum, Hofkeller, Krebsgesellschaft, Future Kids, Fischerzunft, Loma
# 173, 174, 175, 179, 180, 182
# !!! Hofkeller braucht sehr lange (~3min)
# Out: veranstaltungen3
source("173-175&179-182.R")

# Bind 2
df_all <- rbind(df_all, veranstaltungen3)

# API Key 3
register_google(key = "AIzaSyBBlmECdS2894lj2y0l3qNfVeBKF79Lmik")

# Theater Spielberg, Burkardushaus, Buchladen Neuer Weg, Deutschhaus Gymnasium, Bürgerspital
# 160, 161, 163, 164, 178
# Out: veranstaltungen
source("160-164&178.R")

# Bind 3
df_all <- rbind(df_all, veranstaltungen)

# Löschen aller Umlaute
for(i in c(1,3,6,7,14)){
  print(i)
  df_all[,i] <- stringi::stri_replace_all_fixed(df_all[,i],c("ü","ö","ä","ß"), c("ue","oe","ae","ss"),vectorize_all = F)
}

# Zeilennamen anpassen
row.names(df_all) <- c(1:nrow(df_all))

# Delete Cache
#rm(list = setdiff(ls(), "df_all"))

# Save as .csv
#write.table(df_all, file="all_events.csv", row.names=F, col.names= T,sep =",")

df_all %>%
  mutate_all(.funs = as.character) -> df_all
write_feather(df_all, "all_events")

View(df_all)

read_feather("all_events") -> df
df
