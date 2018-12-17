#crawler Sternwarte
#http://www.sternwarte-wuerzburg.de/
#http://www.sternwarte-wuerzburg.de/vswfhrg0.htm


sternwarte_wuerzburg_crawler <- function(){
  library(tidyverse)
  library(rvest)
  require(stringr)
  library(lubridate)
  library(chron)#fuer die Uhrzeit
  
  #---------
  #event_url
  #---------
  event_url <- "http://www.sternwarte-wuerzburg.de/vswfhrg0.htm"
  
  event_url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_node("table")%>%
    html_table( fill = TRUE)->event_tbl
  
  #-----------
  #tidy table
  #-----------
  event_tbl <- data.frame(event_tbl$X1 , event_tbl$X2, event_tbl$X3)
  
  j=0
  i=TRUE
  while(i == TRUE){
    j = j +1
    
    if(event_tbl$event_tbl.X1[j] == "Datum" && event_tbl$event_tbl.X2[j] == "Beginn" && event_tbl$event_tbl.X3[j] == "Thema"){
      #print("Überschriftenzeile")
      i = FALSE
    }
  }
  
  event_tbl = event_tbl[-(1:j-1), ] # alle zeilen bis j löschen
  colnames(event_tbl) <- as.character(unlist(event_tbl[1,]))
  event_tbl = event_tbl[-1, ] # erste zeile löschen
  rownames(event_tbl) <- 1:nrow(event_tbl) # indexzeile durchnummerieren
  
  event_tbl = event_tbl[-(   nrow(event_tbl) ), ] # letzte zeile löschen
  event_tbl = event_tbl[-(   nrow(event_tbl) ), ]#letzte zeile löschen
  event_tbl = event_tbl[-(   nrow(event_tbl) ), ]#letzte zeile löschen
  
  #---------
  #Datum
  #---------
  raw_datum <- event_tbl$Datum
  
  raw_datum%>%
    str_replace_all("^[A-Z,a-z]{1,2}","")%>%
    str_replace_all("\\.", "")%>%
    str_replace_all(",", "")-> datum_von_clean 
  
  datum_von <- parse_datetime(datum_von_clean, "%d  %B %Y", locale = locale("de"))
  as.Date(datum_von) -> datum_von #testzweck
  
  datum_bis <- NA
  as.Date(datum_bis) -> datum_bis
  
  #-----------
  #Uhrzeit
  #-----------
  raw_uhr <- event_tbl$Beginn
  
  regex_uhrzeit = "[0-9]{1,2}[:]{1}[0-9]{1,2}"
  
  raw_uhr %>%
    str_extract_all(regex_uhrzeit, simplify = TRUE)->uhrzeit #nur ersten wert entnehmen und als von nehmen zweiter wert als bis nehmen
  
  
  #hm(uhrzeit[,1]) -> uhr_von #typecast in date format
  uhrzeit[,1] -> uhr_von
  as.character(uhr_von) -> uhr_von
  uhr_von <- times(paste0(uhr_von, ":00"))
  
  #hm(uhrzeit[,2]) -> uhr_bis
  uhrzeit[,2]-> uhr_bis
  as.character(uhr_bis) -> uhr_bis
  uhr_bis <- times(paste0(uhr_bis, ":00"))

  #---------
  #Preis
  #---------
  preis <- as.character(preis <- "4 Erwachsener, 2,50 Kinder")
  
  #-------
  #Titel
  #------
  title <- as.character(title <- event_tbl$Thema)
  
  #-----
  #Ort
  #-----
  ort <- as.character(ort <- NA)
  strasse <- "Cronthalstrasse 25"
  plz <- "97074"
  ort <- "Wuerzburg"
  

  #------------
  #beschreibung
  #------------
  #es ist keine Beschreibung vorhanden. Es kann der Titel als beschreibung verwendet werden 
  beschreibung <- NA #event_tbl$Thema
  
  
  #-------------
  #Veranstalter
  #-------------
  Veranstalter <- "Volkssternwarte Wuerzburg e.V."
  
  
  #-------------------
  #datentypen und Spaltennamen für den Data Frame anpassen
  #-------------------
    lng <- NA
    lat <- NA
  
    title <- as.character(title)
    url <- as.character(event_url)
    description <- as.character(beschreibung)
    lng <- as.numeric(lng)
    lat <- as.numeric(lat)
    city <- as.character(ort)
    street <- as.character(strasse)
    zip <- as.numeric(plz)
    date_start <- as.Date(datum_von)
    date_end <- as.Date(datum_bis)
    time_start <- times(uhr_von)
    time_end <- times(uhr_bis)
    price <- as.character(preis)
    organizer <- as.character(Veranstalter)
  
  #-------------------------------
  #Data Frame der Events erstellen
  #--------------------------------
  df<- data.frame(
    title,
    url,
    description,
    lng,
    lat,
    city,
    street,
    zip,
    date_start,
    date_end,
    time_start,
    time_end,
    price,
    organizer)
    
    
    #-------------
    #lat lng 
    #-------------
    #lat <- as.numeric("49.773609")
    #lng <- as.numeric("9.95975")
    
    df%>%
      mutate(lng = 49.773609)%>%
      mutate(lat = 9.95975) ->df
  
  str(df)
    
  
  return(df)
}











