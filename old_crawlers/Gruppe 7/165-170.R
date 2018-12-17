# V 0.4
# Skript für folgende Webseiten:
# -Puppentheater Kasperhaus (165)
# -Uniklinikum Würzburg - inklusive Neurologie und Poliklinik (166)
# -Evan.-Luth. Auferstehungskirche (168)
# -Galerie Gabriele Müller (170)

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
#devtools::install_github("dkahle/ggmap")
library(ggmap)

#API Key
#register_google(key = "AIzaSyC_lvgZae1kGJ5QWEz2wz3ExZJmBbOzbZ4")

################### Empty Dataframe ################################

df_all <- data.frame(titel = character(),
                     url = character(),
                     description = character(),
                     lng = numeric(),
                     lat = numeric(),
                     city = character(),
                     street = character(),
                     zip = numeric(),
                     date_start = as.Date(character()),
                     date_end = as.Date(character()),
                     time_start =times(),
                     time_end = times(),
                     price = character(),
                     organizer = character(),
                     stringsAsFactors = F)

################### Crawler ################################

#Deutsche Zeit
Sys.setlocale("LC_TIME", "de_DE") #MAC
#Sys.setlocale("LC_ALL","German") #WINDOWS


 #############################
 ######### 166: UKW ########## # # # # # # # # # # # # # # # # # # # # # # # #
 #############################

veranstalter_ukw <- "Universitätsklinikum Würzburg"

adresse_ukw <- "Josef-Schneider-Straße 11"

plz_ukw <- "97080"

ortsname_ukw <- "Würzburg"

#Lat Long Dynamisch
lat_long_temp <- geocode(paste(veranstalter_ukw, adresse_ukw, plz_ukw, ortsname_ukw, sep = ", "))
lat_ukw <- lat_long_temp$lat
long_ukw <- lat_long_temp$lon

#URL
url_ukw <- "http://www.ukw.de/patienten-besucher/veranstaltungskalender/"

#Browser Setup (RSelenium)
rD <- rsDriver(browser = "chrome")
ukwDr <- rD[["client"]]
ukwDr$navigate(url_ukw)

#Klicke Button 
run <- TRUE
i <- 1
while (run){
  tryCatch(
    ukwDr$findElement(using = 'css selector', ".jscroll-next")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

#Selenium auslesen und als einzelne Listenelemente speichern
ukw_list <- read_html(ukwDr$getPageSource()[[1]]) %>%
  html_nodes("div.scroll-item")

#ALTERNATIV: Regulärer Seitenzugriff (erste 7 Elemente)
#ukw_list <- url_ukw %>%
#read_html() %>%
#html_nodes("div.scroll-item")

#durch alle Elemente iterieren
for(i in 1:length(ukw_list)){
  
  #Datum
  datum_temp <- ukw_list[i] %>%
    html_node(".date") %>%
    html_text
  
  #Datum auslesen
  if(grepl(".*(\\d{2}.\\d{2}.\\d?{4}).+(\\d{2}.\\d{2}.\\d{4}).*", datum_temp)){
    
    #String nach Leerzeichen trennen und als Liste speichern
    timeframe <- str_split(datum_temp, "[\n\r\t]+") %>%
      .[[1]]
    
    counter = T
    
    #für jedes Element der Liste nach Datumsformat checken
    for(t in timeframe){
      
      #das erste Element im Datumformat = Anfangsdatum
      if(grepl("\\d{2}\\.\\d{2}\\.\\d?{4}", t) && counter){ 
        
        start_date_temp <- sub(".*(\\d{2}\\.\\d{2}\\.).*", "\\1", t) #start_date
        counter <- F
      
      #das zweite Element ist das Enddatum 
      } else if(grepl("\\d{2}\\.\\d{2}\\.\\d{4}", t) && !counter){ 
        
        end_date_temp <- as.Date(sub(".*(\\d{2}\\.\\d{2}\\.\\d{4}).*", "\\1", t),"%d.%m.%Y") #end_date
        jahr <- sub(".*(\\d{4}).*", "\\1", t) #das Jahr auslesen
        
      }
    }
    
    #start_date mit Jahreszahl in ein Datumsformat überführen
    start_date_temp <- as.Date(paste(start_date_temp, jahr, sep = ""), "%d.%m.%Y")
    
    #Zeitpunkt auslesen
  } else if(grepl(".*(\\d{2}.\\d{2}.\\d{4}).*", datum_temp)) {
    
    #Single
    start_date_temp <- as.Date(sub(".*(\\d{2}.\\d{2}.\\d{4}).*", "\\1", datum_temp),"%d.%m.%Y")
    end_date_temp <- start_date_temp
    
    #kein Datum || nicht lesbar
  } else {
    
    #Single
    datum_temp <- NA
    
  }
  
  #Uhrzeit
  zeit_temp <- ukw_list[i] %>%
    html_node(".time") %>%
    html_text(trim=T)
  
  #Zeitspanne auslesen
  if(grepl(".*\\d{2}:\\d{2}.*-[[:space:]]*\\d{2}:\\d{2}.*",zeit_temp)){
    
    time_clean <- gsub("[[:space:]]|[\r\n\t]|Uhr","", zeit_temp)
    start_time_temp <- times(paste(gsub("(\\d{2}:\\d{2}).*","\\1", time_clean),"00", sep = ":"))
    end_time_temp <- times(paste(gsub(".*(\\d{2}:\\d{2})","\\1", time_clean),"00", sep = ":"))
    
  
  #Zeitpunkt auslesen    
  } else if(grepl(".*(\\d{2}:\\d{2}).*",zeit_temp)){
    
    start_time_temp <- times(paste(gsub("[[:space:]]|[\r\n\t]|Uhr","", zeit_temp),"00",sep =":"))
    end_time_temp <- times(NA)
  
  #keine Zeitangabe  
  } else {
    
    start_time_temp <- times(NA)
    end_time_temp <- times(NA)
    
  }
  
  #Beschreibung
  beschreibung_temp <- ukw_list[i] %>%
    html_node(".text") %>%
    html_text(trim = T) %>%
    noquote()
  
  #Falls als Liste vorliegt, diese Zusammenmergen mit einem Absatz
  beschreibung_temp <- paste(beschreibung_temp, collapse ="\n\n") 
  
  #zu kurze Beschreibungen rausfiltern
  if(nchar(beschreibung_temp) < 30){
    
    beschreibung_temp <- NA
    
  }
  
  #titel
  titel_temp <- ukw_list[i] %>%
    html_node(".title") %>%
    html_text(trim = T)
  
  #Link
  link_temp <- ukw_list[i] %>%
    html_node(".print a") %>%
    html_attr("href") %>%
    paste("https://www.ukw.de",., sep = "")
  
  #Veranstalter in der Schleife neu setzen
  veranstalter_ukw <- "Universitätsklinikum Würzburg"
  
  #Unterveranstalter auslesen (z.B. Poliklinik)
  veran <-  ukw_list[i] %>%
    html_node(".organizer") %>%
    html_text(trim = T)
  
  #Veranstalter zusammenfügen
  if (!is.na(veran) && veran!="Universitätsklinikum Würzburg"){
    
    veranstalter_ukw <- paste(veranstalter_ukw, veran, sep = " - ")
    
  }
  
  #Die Veranstaltung für jedes Datum abspeichern

  df_all2 <- data.frame(titel = titel_temp,
                       url = link_temp,
                       description = beschreibung_temp,
                       lng = long_ukw,
                       lat = lat_ukw,
                       city = ortsname_ukw,
                       street = adresse_ukw,
                       zip = plz_ukw,
                       date_start = start_date_temp,
                       date_end = end_date_temp,
                       time_start = start_time_temp,
                       time_end = end_time_temp,
                       price = as.character(NA),
                       organizer = veranstalter_ukw,
                       stringsAsFactors = F)
    
  df_all <- rbind(df_all, df_all2)
  
}

 #############################
 ###### 170: GABRIELE ######## # # # # # # # # # # # # # # # # # # # # # # # #
 #############################

#Öffnungszeiten
oeffnungszeiten_mueller <- data.frame(c("Mo", "Di", "Mi", "Do", "Fr", "Sa"), c("10:00-13:00","10:00-13:00 & 14:00-18:00", "10:00-13:00 & 14:00-18:00", "10:00-13:00 & 14:00-18:00", "10:00-13:00 & 14:00-18:00", "10:00-14:00"))
colnames(oeffnungszeiten_mueller) <- c("Wochentag", "Uhrzeit")

#Ort
veranstalter_mueller <- "Galerie Gabriele Müller"

adresse_mueller <- "Theaterstraße 18"

plz_mueller <- "97070"

ortsname_mueller <- "Würzburg"

#Lat Long Dynamisch
lat_long_temp <- geocode(paste(veranstalter_mueller, adresse_mueller, plz_mueller, ortsname_mueller, sep = ", "))
lat_mueller <- lat_long_temp$lat
long_mueller <- lat_long_temp$lon

#Link
url_mueller <- "https://galerie-gabriele-mueller.de/ausstellungen/"

#Alle Infos
ausstellungen <- url_mueller %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/div/div/div/div/article/div") %>%
  .[[1]]

#titel
titel_mueller <- ausstellungen %>%
  html_nodes("h1") %>%
  html_text()

#Datum Raw
datum_mueller <- ausstellungen %>%
  html_nodes("p") %>%
  html_text()

#Veranstaltungs Link
links_mueller <- ausstellungen %>%
  html_nodes("h1") %>%
  html_nodes("a") %>%
  html_attr("href")

################### Load into Dataframe ###########################

#hier gibt es über 50 Veranstaltungen in der Vergangenheit - die ersten 10 werden ausgelesen
for (d in 1:10){
  
  print(paste("Veranstaltung", as.character(d), sep=" "))
  
  #titel
  titel_temp <- titel_mueller[d]
  
  #Datum
  if(grepl("[0-9]{2}\\.[[:space:]]+[A-zä]+[[:space:]]+–[[:space:]]+[0-9]{2}\\.[[:space:]]+[A-zä]+[[:space:]]+[0-9]{4}", datum_mueller[d])){
    
    strsplit(datum_mueller[d],"[[:space:]]+") %>%
      .[[1]] -> split_temp 
    
    datum_start <- as.Date(paste(split_temp[1], split_temp[2], split_temp[6], sep ="-"), "%d.-%B-%Y")
    datum_end <- as.Date(paste(split_temp[4], split_temp[5], split_temp[6], sep ="-"), "%d.-%B-%Y")
    
    datum_temp <- seq(datum_start, datum_end, by="days")
    
  } else if(grepl("[0-9]{2}\\.[[:space:]]+–[[:space:]]+[0-9]{2}\\.[[:space:]]+[A-zä]+[[:space:]]+[0-9]{4}", datum_mueller[d])) { 
    
    sstrsplit(datum_mueller[d],"[[:space:]]+") %>%
      .[[1]] -> split_temp 
    
    date_start_temp <- as.Date(paste(split_temp[1], split_temp[4], split_temp[5], sep ="-"), "%d.-%B-%Y")
    date_end_temp <- as.Date(paste(split_temp[3], split_temp[4], split_temp[5], sep ="-"), "%d.-%B-%Y")
    
    datum_temp <- seq(date_start_temp, date_end_temp, by="days")
    
  } else {
    
    datum_temp <- as.Date(NA)
    
  }
  
  #Link
  link_temp <- links_mueller[d]
  
  #Beschreibung
  beschreibung_temp <- link_temp %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text() %>%
    noquote()
  
  #Falls als Liste vorliegt, diese Zusammenmergen mit einem Absatz
  beschreibung_temp <- paste(beschreibung_temp, collapse ="\n\n")
  
  if(grepl(".*PDF.+", beschreibung_temp)){  #falls Beschreibung in Form einer PDF, setze NA
    
    beschreibung_temp <- NA
    
  } else if(nchar(beschreibung_temp) < 30){ #falls Beschreibung zu kurz, setze NA
    
    beschreibung_temp <- NA
    
  }
  
  #Merge & Öffnungszeiten
  for (i in 1:length(datum_temp)){
    
    print(paste("Termin", as.character(i), sep=" "))
    print(as.character(Sys.time()))
    
    #Filter für Sonntage
    if(wday(datum_temp[i], label = TRUE)!="So"){ 
      
      #Öffnungszeit der anderen Tage abgleichen
      for(o in 1:length(oeffnungszeiten_mueller$Wochentag)){
        
        if(as.character(oeffnungszeiten_mueller$Wochentag[o]) == as.character(wday(datum_temp[i], label = TRUE))){
          
          time_clean <- oeffnungszeiten_mueller$Uhrzeit[o]
          
          #Check ob zwei Öffnungszeiten pro Tag
          t1 <-gsub("(.*)[[:space:]]&[[:space:]](.*)","\\1",time_clean)
          t2 <-gsub("(.*)[[:space:]]&[[:space:]](.*)","\\2",time_clean)
          
          if(t1==t2){
           
            #Wieviele Events pro Tag:
            e <- 1
            
          } else {
            
            e <- 2
            
          }
        }
      }
      
      if(wday(datum_temp[i], label = TRUE)=="Mo" || wday(datum_temp[i], label = TRUE)=="Sa"){
        
        start_date_temp <- end_date_temp <- datum_temp[i]
        
        start_time_temp <- times(paste(gsub("(\\d{2}:\\d{2}).*","\\1", t1),"00", sep = ":"))
        end_time_temp <- times(paste(gsub(".*(\\d{2}:\\d{2})","\\1", t1),"00", sep = ":"))
        
        check <- T
        
      } else if (wday(datum_temp[i], label = TRUE)=="Di"){
        
        start_date_temp <- datum_temp[i]
        
        t <- T
        k <- 3
        while(t){
          if(!is.na(datum_temp[i+k])){
            end_date_temp <- datum_temp[i+k]
            t <- F
          }
          k <- k-1
        }
        
        start_time_temp[1] <- times(paste(gsub("(\\d{2}:\\d{2}).*","\\1", t1),"00", sep = ":"))
        end_time_temp[1] <- times(paste(gsub(".*(\\d{2}:\\d{2})","\\1", t1),"00", sep = ":"))
        
        start_time_temp[2] <- times(paste(gsub("(\\d{2}:\\d{2}).*","\\1", t2),"00", sep = ":"))
        end_time_temp[2] <- times(paste(gsub(".*(\\d{2}:\\d{2})","\\1", t2),"00", sep = ":"))
        
        check <- T
          
      } else {
        
        check <- F
        
      }
      
      #Merge into Dataframe
      if(check){
        for(a in 1:e){
          df_all2 <- data.frame(titel = titel_temp,
                               url = link_temp,
                               description = beschreibung_temp,
                               lng = long_mueller,
                               lat = lat_mueller,
                               city = ortsname_mueller,
                               street = adresse_mueller,
                               zip = plz_mueller,
                               date_start = start_date_temp,
                               date_end = end_date_temp,
                               time_start = start_time_temp[a],
                               time_end = end_time_temp[a],
                               price = as.character(NA),
                               organizer = veranstalter_mueller,
                               stringsAsFactors = F)
  
          df_all <- rbind(df_all, df_all2)
        }
      }
    }
  }
  
}

 #############################
 ##### 165: KASPERHAUS ####### # # # # # # # # # # # # # # # # # # # # # # # #
 #############################

veranstalter_kasp <- "Das Kasperhaus"

adresse_kasp <- "Julius-Echter-Straße 8"

plz_kasp <- "97084"

ortsname_kasp <- "Würzburg"

#Lat Long Dynamisch
lat_long_temp <- geocode(paste(veranstalter_kasp, adresse_kasp, plz_kasp, ortsname_kasp, sep = ", "))
lat_kasp <- lat_long_temp$lat
long_kasp <- lat_long_temp$lon

#Link
url_kasp <- link_temp <- "http://www.theater-kasperhaus.de/spielplan.htm"

#Termine in den Tabellen speichern
termine <- url_kasp %>%
  read_html() %>%
  html_nodes(".TitelSeitenleiste table") %>%
  html_table()

#Das Jahr über den jeweiligen Tabellen speichern
jahre <- url_kasp %>%
  read_html() %>%
  html_nodes(".Stil156") %>%
  html_text()

#Beschreibung ist NA
beschreibung_temp <- as.character(NA)

#Durch alle Tabellen iterieren
for(i in 1:length(termine)){
  
  #Das Jahr i abspeichern
  jahr <- format(Sys.Date(), "%Y")
  check <- T
  counter <- 1
  while(check){
    if(jahre[counter]!=""){
      jahr <- sub(".*(\\d\\d\\d\\d).*","\\1",jahre[counter])
      check <- F
    }
    counter <- counter+1
    if(counter>20){check <- F}
  }
  
  #Tabelle i abspeichern
  termine_temp <- termine[[i]]
  
  #Durch alle Zeilen iterieren
  for(j in 1:nrow(termine_temp)){
    
    #titel (in der zweiten Spalte)
    titel_temp <- termine_temp[j,2]
    
    #Uhrzeit (in der dritten Spalte)
    time_clean <- sub("(\\d?\\d).(\\d\\d)","\\1:\\2",sub("(\\d?\\d.\\d\\d) Uhr","\\1",termine_temp[j,3]))
    
    start_time_temp <- times(paste(gsub("(\\d{2}:\\d{2}).+","\\1", time_clean),"00", sep = ":"))
    end_time_temp <- times(NA)
    
    #Datum (in der ersten Spalte)
    start_date_temp <- as.Date(paste(sub(".*(\\d\\d.\\d\\d.)","\\1", termine_temp[j,1]),jahr, sep =""), "%d.%m.%Y")
    end_date_temp <- as.Date(paste(sub(".*(\\d\\d.\\d\\d.)","\\1", termine_temp[j,1]),jahr, sep =""), "%d.%m.%Y")
    
    #Eintritt (grundsätzlich 6€)
    preis_temp <- 6
    
    #Merge into Datafram
    df_all2 <- data.frame(titel = titel_temp,
                          url = link_temp,
                          description = beschreibung_temp,
                          lng = long_kasp,
                          lat = lat_kasp,
                          city = ortsname_kasp,
                          street = adresse_kasp,
                          zip = plz_kasp,
                          date_start = start_date_temp,
                          date_end = end_date_temp,
                          time_start = start_time_temp,
                          time_end = end_time_temp,
                          price = preis_temp,
                          organizer = veranstalter_kasp,
                          stringsAsFactors = F)
    
    df_all <- rbind(df_all, df_all2)
    
  } 
  
}

 #############################
 #### 168: AUFERSTEHUNG ###### # # # # # # # # # # # # # # # # # # # # # # # #
 #############################

veranstalter_auf <- "Evang.-Luth. Auferstehungskirche"

adresse_auf <- "Hans-Löffler-Straße 33"

plz_auf <- "97074"

ortsname_auf <- "Würzburg"

#Lat Long Dynamisch
lat_long_temp <- geocode(paste(veranstalter_auf, adresse_auf, plz_auf, ortsname_auf, sep = ", "))
lat_auf <- lat_long_temp$lat
long_auf <- lat_long_temp$lon

url_auf <- link_temp <- "https://www.auferstehung-wue.de/?seite=15"

auferstehung <- url_auf %>%
  read_html() %>%
  html_nodes("#main div , h4") 

#Jeder zweite Eintrag aus 'auferstehung' ist wichtig
counter <- seq(from =1, to = length(auferstehung),by = 2)

#Iteriere durch alle wichtigen Einträge
for(i in counter){
  
  #Textblock abspeichern
  auf_temp <- html_text(auferstehung[i])
  
  #titel aus nächstem Eintrag auslesen
  titel_temp <- auferstehung[i+1] %>%
    html_text(trim = T)
  
  #Datum
  start_date_temp <- end_date_temp <- as.Date(sub(".*(\\d{2}.\\d{2}.\\d{4}).*","\\1", auf_temp), "%d.%m.%Y")
  
  #Falls kein Datum, setze NA
  if(!grepl(".*(\\d{2}.\\d{2}.\\d{4}).*", auf_temp)){
    start_date_temp <- as.Date(NA)
    end_date_temp <- as.Date(NA)
  }
  
  #Uhrzeit
  if(grepl(".* (\\d{2}:\\d{2}) .*", auf_temp)){
    start_time_temp <- times(paste(sub(".* (\\d{2}:\\d{2}) .*","\\1", auf_temp),"00", sep = ":"))
  } else {
    start_time_temp <- times(NA)
  }
  end_time_temp <- times(NA)
  
  
  #Links
  link_temp <- auferstehung[i] %>%
    html_nodes("a") %>%
    html_attr("href")
  
  check <- 0
  
  #Errorhandling falls kein Link existiert, setze leeren String
  if(identical(link_temp, character(0))|| is.na(link_temp)){
    
    link_temp <- ""
    check <- 1 #setze check auf 1 für die Beschreibung
    
  }
  
  #Link zusammenfügen
  link_temp <- paste("https://www.auferstehung-wue.de/",link_temp, sep = "")
  
  
  #Beschreibung (nur falls check = 0 ist, suche nach der Beschreibung, sonst NA)
  beschreibung_temp <- NA
  if(check==0){
    beschreibung_temp <- link_temp %>%
      read_html() %>%
      html_nodes("p") %>%
      html_text() %>%
      noquote() %>%
      paste(beschreibung_temp, collapse ="\n")
  }
  
  #Merge into Dataframe
  df_all2 <- data.frame(titel = titel_temp,
                        url = link_temp,
                        description = beschreibung_temp,
                        lng = long_auf,
                        lat = lat_auf,
                        city = ortsname_auf,
                        street = adresse_auf,
                        zip = plz_auf,
                        date_start = start_date_temp,
                        date_end = end_date_temp,
                        time_start = start_time_temp,
                        time_end = end_time_temp,
                        price = as.character(NA),
                        organizer = veranstalter_auf, stringsAsFactors = F)
  
  df_all <- rbind(df_all, df_all2)
  
}

colnames(df_all) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
df_all$zip <- as.numeric(df_all$zip)

#Write table Befehl.
#write.table(df_all, file="all.csv", row.names=F, sep =",")
