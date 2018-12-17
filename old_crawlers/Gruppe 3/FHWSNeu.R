FHWS <- function(){
library(rvest)
library(tidyverse)
library(stringr)
library(chron)


Eventtabelle <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(Eventtabelle) <- c("title", "description", "Startdatum","Enddatum", "Startzeit", "Endzeit","url","price","organizer","city","street","zip","Long","Lat")

url = 'https://www.fhws.de/termine/'

url %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes('section') %>%
  html_nodes('div.col-md-9') %>%
  html_text()-> section
section <- str_replace_all(section, "[[\\r|\\n|\\t]]" , "")
section <- data.frame(section)
ind <- seq(1, nrow(section), by=2)
section2 <- section[ind,]
section2 <- data.frame(section2)
count <-nrow(section2)
test1 <- vector()
for (i in 1:count) {
  test12 <- paste('//*[@id="accordion_news_events"]/section[',i,']', sep = "")
  test1 <- c(test1,test12)
}
test1

catchingsec = function(string){

  raw_data %>%
    html_nodes(xpath = string) %>%
    html_nodes('p')-> test
  test <- test[length(test)]
  
  dates <- as.character(str_extract_all(test,"[0-9]{2}.[0-9]{2}.[0-9]{4}", simplify = TRUE)) #
  uhrzeiten <- as.character(str_extract_all(test,"[0-9]+:[0-9]{2}", simplify = TRUE)) #
  
  dates <- data.frame(dates)
  colnames(dates) <- c("Datum")
  uhrzeiten <- data.frame(uhrzeiten)
  colnames(uhrzeiten) <- c("Uhrzeiten")
  startzeit <- uhrzeiten$Uhrzeiten[1]
  endzeit <- uhrzeiten$Uhrzeiten[2]
  
  Startdatum <- dates$Datum[1]
  Enddatum <- dates$Datum[2]
  
  if(is.na(Enddatum)){ #
    Enddatum <- Startdatum #
  } #
  
  Startdatum <- as.Date(Startdatum, format= "%d.%m.%Y")
  Enddatum <- as.Date(Enddatum, format= "%d.%m.%Y")
  
  
  raw_data %>%
    html_nodes(xpath = string) %>%
    html_nodes('div.col-md-9') %>%
    html_text()-> section
  section <- str_replace_all(section, "[[\\r|\\n|\\t]]" , "")
  section <- data.frame(section)
  ind <- seq(1, nrow(section), by=2)
  section2 <- section[ind,]
  section2 <- data.frame(section2)
  
  raw_data %>%
    html_nodes(xpath = string) %>%
    html_nodes(".vst-detail") %>%
    html_text() -> orte
  
  city <- ""
  street <- ""
  zip <- ""
  Long <- NA
  Lat <- NA
  
  
  orte %>%
    str_extract("Anschrift.+Veranstalter") %>%
    gsub("Anschrift:","",.) %>%
    gsub("Veranstalter","",.) -> Adresse # sollte wieder r
  
  # if(!is.na(Adresse)){
  # library(googleway)  
  # googleapikey <- "AIzaSyC283OAvKOK7PW3vQuQVhijCV1LTbgH8q8"  
  # set_key(googleapikey)
  # res <- google_geocode(Adresse)
  # geocords <- geocode_coordinates(res)
  # Lat <- geocords$lat[1]
  # Long <- geocords$lng[1]
  # adresse <- geocode_address(res)
  # Kompletteadresse <- data.frame(adresse)
  # colnames(Kompletteadresse) <- c("Adresse")
  # Versuch123 <- separate(data= Kompletteadresse, col =Adresse,into = c("Stra?e","Ort","Land"),sep = "\\," )
  # zip <- str_extract(Versuch123$Ort,"[0-9]+")
  # city <- gsub("[0-9]+","",Versuch123$Ort)
  # street <- Versuch123$Stra?e
  # 
  # street <- tail(street,n=1)
  # zip <- tail(zip,n=1)
  # city <- tail(city,n=1)
  # 
  # begin <- str_extract(zip, ".{2}")
  # 
  # if(!begin=="97"){
  #   
  #   city <- "W?rzburg"
  #   street <- "M?nzstra?e 12"
  #   zip <- 97070
  #   Long <- 9.932649
  #   Lat <- 49.787609 
  #   }
  # }  
  
    

  orte%>%
    str_extract("Veranstaltungsort.+Veranstalter") %>%
    str_extract("Veranstaltungsort.+Anschrift") %>%
    gsub("Veranstaltungsort:","",.) %>%
    gsub("Anschrift","",.) -> ort
    
#  if(is.na(Adresse)){
    
    city <- "Wuerzburg"
    street <- "Muenzstra?e 12"
    zip <- 97070
    Long <- 9.932649
    Lat <- 49.787609 
    
#  }
  
  raw_data %>%
    html_nodes(xpath = string) %>%
    html_nodes('p')-> test2
  test2 <- test2[-length(test2)]
  test2[2]%>%
    html_text() -> beschreibung
  
  if(!is.na(Adresse)){
    
    beschreibung <- paste(beschreibung,"Adresse der Veranstaltung abweichend stattfinden in",Adresse,collapse=" ")   
  }
  if(is.na(Adresse)){
    
    beschreibung <- paste(beschreibung,collapse=" ")   
  }
  
  
  linkad <- 'https://www.fhws.de/termine/'
  price <- NA
  organizer <- "Hochschule fuer angewandte Wissenschaften Wuerzburg-Schweinfurt" 

  startzeit <- as.character(startzeit)
  Try <- class(endzeit)
  endzeit <- as.character(endzeit)
  if(!is.na(startzeit)){
  startzeit <- paste(startzeit,':00', sep = '')
  #startzeit <- times(startzeit)
  }
  if(!is.na(endzeit)){
    endzeit <- paste(endzeit,':00', sep = '')
 #   endzeit <- times(endzeit)
  }
  
  Event <- data.frame(section2$section2,beschreibung,Startdatum,Enddatum, startzeit, endzeit,linkad,price,organizer,city,street,zip,Long,Lat)
  colnames(Event) <- c("title", "description", "Startdatum","Enddatum", "Startzeit", "Endzeit","url","price","organizer","city","street","zip","Long","Lat")
  
  return(Event)
}
for (i in test1) {
  testdf = catchingsec(i)
  Eventtabelle <- rbind(Eventtabelle,testdf)
  assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
}
class(Eventtabelle$Endzeit)




for (i in 1:nrow(Eventtabelle)) {
  Eventtabelle$Endzeit[i] <- as.character(Eventtabelle$Endzeit[i])
  if(!is.na(Eventtabelle$Endzeit[i])){
    Eventtabelle$Endzeit[i] <- as.character(times(Eventtabelle$Endzeit[i]))
    
  }
}



return(Eventtabelle)
}