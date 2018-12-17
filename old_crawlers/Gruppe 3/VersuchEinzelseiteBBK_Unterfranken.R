BBKUnterfranken <- function() {
  
library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)

Eventtabelle <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(Eventtabelle) <- c("title", "description", "StartDatum", "EndDatum", "Startzeit", "Endzeit","Lat","Long","url","price","organizer","city","street","zip")

urlwerk <- "http://www.bbk-unterfranken.de/werkstattgalerie.html"
urlsbbk <- "http://www.bbk-unterfranken.de/ausstellungen_bbk.html"

urlwerk %>%
  read_html() %>%
  html_nodes("div.box.bg-white a")%>%
  html_attr("href") -> Linksa
Linksa <- data.frame(Linksa)

Linksa$Linksa <- paste("http://www.bbk-unterfranken.de/", Linksa$Linksa, sep="")

urlsbbk %>%
  read_html() %>%
  html_nodes("div.box.bg-white a")%>%
  html_attr("href") -> Linksb
Linksb <- data.frame(Linksb)

Linksb$Linksb <- paste("http://www.bbk-unterfranken.de/", Linksb$Linksb, sep="")

alllinksa <- unlist(Linksa)
alllinksb <- unlist(Linksb)
urls <- as.vector(c(alllinksa,alllinksb)) 

crawlingVen = function(urls) {
  
  adresse <- "Oskar-Laredo-Platz 1"
  lat <- 49.8158268
  long <- 9.9102608
  
  urls %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("h6") %>%
    html_text() -> Titel
  
  dftitel <- data.frame(Titel)
  colnames(dftitel) <- c("Titel")
  
  raw_data %>%
    html_nodes("h6 + p") %>%
    html_text() -> Text
  
  raw_data %>%
    html_nodes("strong") %>%
    html_text() -> Vernissage
  
  Vernissage <-  str_trim(str_replace(Vernissage, 'Vernissage: ', '')) 
  Vernissage <-  strsplit(Vernissage, ", ", fixed=TRUE)
  df2 <- data.frame(Vernissage)
  colnames(df2) <- c("Event")
  
  dates <- as.character(str_extract_all(df2$Event,"[0-9]{2}.[0-9]{2}.[0-9]{2}", simplify = TRUE)) #
  dates <- data.frame(dates)
  colnames(dates) <- c("Datum")
  
  Startdatum <- dates$Datum[1]
  Enddatum <- dates$Datum[2]
  
  if(Enddatum == ''){ #
    Enddatum <- Startdatum #
  } #
  uhrzeiten <- as.character(str_extract_all(df2$Event,"[0-9]+.[0-9]{2} Uhr", simplify = TRUE)) #
  uhrzeiten <- gsub("Uhr","",  uhrzeiten)
  uhrzeiten <- data.frame(uhrzeiten)
  colnames(uhrzeiten) <- c("Uhrzeiten")
  startzeit <- uhrzeiten$Uhrzeiten[2]
  endzeit <- uhrzeiten$Uhrzeiten[3]
  if(endzeit == ''){ #
    endzeit <- NA #
  } #
  linkad <- urls
  Titel <- paste("Vernissage ", Titel)
  price <- NA
  organizer <- "Berufsverband Bildender Künstlerinnen und Künstler Unterfranken"
  
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
    endzeit <- times(endzeit)
  }
  city <- "Würzburg"
  street <- "Oskar-Laredo-Platz 1"
  zip <- 97080
  
  Text <- paste(Text,"Veranstaltung findet statt in ",df2$Event[3])
  
  testdf <- data.frame(Titel,Text,Startdatum,Enddatum,startzeit,endzeit,lat,long,linkad,price,organizer,city,street,zip)
  colnames(testdf) <- c("title", "description", "StartDatum", "EndDatum", "Startzeit", "Endzeit","Lat","Long","url","price","organizer","city","street","zip")
  
  return(testdf)
}
crawlingAus = function(urls) {
  

  lat <- 49.8158268
  long <- 9.9102608
  
  urls %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("h6") %>%
    html_text() -> Titel
  
  raw_data %>%
    html_nodes("h6 + p") %>%
    html_text() -> Text
  
  raw_data %>%
    html_nodes("strong") %>%
    html_text() -> Vernissage
  
  Vernissage <-  str_trim(str_replace(Vernissage, 'Vernissage: ', '')) 
  Vernissage <-  strsplit(Vernissage, ", ", fixed=TRUE)
  df2 <- data.frame(Vernissage)
  colnames(df2) <- c("Event")
  
  raw_data %>%
    html_nodes(".box25-inner h3") %>%
    html_text() -> Zeitraum
  Zeitraum <- data.frame(Zeitraum)
  
  dates <- as.character(str_extract_all(Zeitraum$Zeitraum,"[0-9]{2}.[0-9]{2}.[0-9]{2}", simplify = TRUE)) #
  dates <- data.frame(dates)
  colnames(dates) <- c("Datum")
  
  Startdatum <- dates$Datum[1]
  Enddatum <- dates$Datum[2]
  
  if(Enddatum == ''){ #
    Enddatum <- Startdatum #
  } #
  uhrzeiten <- as.character(str_extract_all(Zeitraum$Zeitraum,"[0-9]+.[0-9]{2} Uhr", simplify = TRUE)) #
  uhrzeiten <- gsub("Uhr","",  uhrzeiten)
  uhrzeiten <- data.frame(uhrzeiten)
  colnames(uhrzeiten) <- c("Uhrzeiten")
  startzeit <- uhrzeiten$Uhrzeiten[2]
  endzeit <- uhrzeiten$Uhrzeiten[3]
  
  Titel <- paste("Austellung ", Titel)
  
  url <- "http://www.bbk-unterfranken.de/"
  url %>%
    read_html() %>%
    html_node(".box3 .box-inner") %>%
    html_text()-> oeff
  
  
  oeff <- str_replace_all(oeff, "[[\\r|\\n|\\t]]" , "")
  Text <- paste(Text,oeff)

  linkad <- urls
  price <- NA
  organizer <- "Berufsverband Bildender Künstlerinnen und Künstler Unterfranken"
  
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
  street <- "Oskar-Laredo-Platz 1"
  zip <- 97080
  
  Text <- paste(Text,"Veranstaltung findet statt in ",df2$Event[3])
  
  testdf <- data.frame(Titel,Text,Startdatum,Enddatum,startzeit,endzeit,lat,long,linkad,price,organizer,city,street,zip)
  colnames(testdf) <- c("title", "description", "StartDatum", "EndDatum", "Startzeit", "Endzeit","Lat","Long","url","price","organizer","city","street","zip")
  
  return(testdf)
}

for (i in urls) {
  testdf = crawlingVen(i)
  Eventtabelle <- rbind(Eventtabelle,testdf)
  assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
  
  testdf3 = crawlingAus(i)
  Eventtabelle <- rbind(Eventtabelle,testdf3)
  assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
}
colnames(Eventtabelle) <- c("title", "description", "StartDatum", "EndDatum", "Startzeit", "Endzeit","Lat","Long","url","price","organizer","city","street","zip")
Eventtabelle$StartDatum <- gsub(".18",".2018",  Eventtabelle$StartDatum, fixed = TRUE)
Eventtabelle$StartDatum <- gsub(".19",".2019",  Eventtabelle$StartDatum, fixed = TRUE)
Eventtabelle$EndDatum <- gsub(".18",".2018",  Eventtabelle$EndDatum, fixed = TRUE)
Eventtabelle$EndDatum <- gsub(".19",".2019",  Eventtabelle$EndDatum, fixed = TRUE)

Eventtabelle$EndDatum <- as.Date(Eventtabelle$EndDatum, format= "%d.%m.%Y")
Eventtabelle$StartDatum <- as.Date(Eventtabelle$StartDatum, format= "%d.%m.%Y")

colnames(Eventtabelle) <- c("title", "description", "Startdatum", "Enddatum", "Startzeit", "Endzeit","Lat","Long","url","price","organizer","city","street","zip")



return(Eventtabelle)
}

