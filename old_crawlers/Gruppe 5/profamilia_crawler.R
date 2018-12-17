#crawler für profamilia
#https://www.profamilia.de/
#https://www.profamilia.de/index.php?id=328

profamilia_crawler <- function(){
  library(tidyverse)
  library(rvest)
  require(stringr)
  library(lubridate)
  library(chron)#fuer die Uhrzeit
  source("geo_code_function.R")
  library(dplyr)#um spalten zusammenzufügen
  
  #zu crawlende Website
  url <- "https://www.profamilia.de/index.php?id=328"
  event_url <- "https://www.profamilia.de/index.php?id=7081&tx_seminars_pi1%5BshowUid%5D=195"
  # Get HTML
  url %>% 
    read_html() -> raw_data
  
  raw_data%>%
    html_nodes(".table-hover a")%>%
    html_attr("href")-> raw_links
  
  links <- paste0("https://www.profamilia.de", raw_links)
  
  get_event_data <- function(event_url){
    #----------------------------------------------------------
    #Alle werte auf  NA gesetzt falls einer nicht vorhanden ist
    #----------------------------------------------------------
    title <- NA
    datum_von <- NA
    datum_bis <- NA
    uhr_von <- NA
    uhr_bis <- NA
    ort <- NA
    preis <- NA
    beschreibung <- NA
    
    #event_url <- "https://www.profamilia.de/index.php?id=7081&tx_seminars_pi1%5BshowUid%5D=195"#Testzweck für die funktion
    event_url %>%
      read_html() -> raw_data
    
    title_selector <- "h2"
    
    #--------
    #Titel
    #--------
    raw_data %>%
      html_node(title_selector) %>%
      html_text(trim = TRUE) -> title
    
    raw_data %>%
      html_node("table")%>%
      html_table( fill = TRUE)->event_tbl
    
    #------------------------------------------------------
    #Tabelle in wide format bringen und richtig beschriften
    #------------------------------------------------------
    event_tbl <- data.frame(event_tbl$X1 , event_tbl$X2)
    
    event_tbl <- t(event_tbl)#dataframe tabelle transponieren
    event_tbl <- data.frame(event_tbl)
    
    colnames(event_tbl) <- as.character(unlist(event_tbl[1,])) # erste zeile als Spaltenname verwenden
    names(event_tbl)[names(event_tbl) == "Ziele/Inhalte"] <- "beschreibung" #column ziele/Inhalte in beschreibung umbenennen 
    event_tbl = event_tbl[-1, ] # erste zeile löschen
    rownames(event_tbl) <- 1:nrow(event_tbl) # indexzeile durchnummerieren
    
    #Felder aus df selectieren
    event_tbl <- event_tbl[ , c("Beginn","Ende","Ort", "beschreibung" , "Teilnahmebeitrag")]
    
    
    #-----------------
    #datum uhrzeit von
    #-----------------
    event_tbl$Beginn%>%
      str_replace_all(",","")%>%
      str_replace_all("[a-z]","")%>%
      str_replace_all("[A-Z]","")%>%
      str_replace_all("[:space:]{2,}", " ") -> datum_zeit_von
    
    datum_zeit_von <- dmy_hm(datum_zeit_von)
    datum_von <- as.Date(datum_zeit_von)
    uhr_von <- format(datum_zeit_von, format = "%H:%M")#gibt die Uhrzeit zurück
    uhr_von <- times(paste0(uhr_von, ":00"))
    #event_uhrzeit_von <- hm(event_uhrzeit)
    
    
    #-----------------
    #datum uhrzeit bis
    #-----------------
    event_tbl$Ende%>%
      str_replace_all(",","")%>%
      str_replace_all("[a-z]","")%>%
      str_replace_all("[A-Z]","")%>%
      str_replace_all("[:space:]{2,}", " ") -> datum_zeit_bis
    
    datum_zeit_bis <- dmy_hm(datum_zeit_bis)
    datum_bis <- as.Date(datum_zeit_bis)
    uhr_bis <- format(datum_zeit_bis, format = "%H:%M")#gibt die Uhrzeit zurück
    uhr_bis <- times(paste0(uhr_bis, ":00"))
    #uhr_von <- hm(uhr)
    
    
    #--------
    #Ort
    #--------
    ort <- event_tbl$Ort
    
    #--------
    #Preis
    #--------
    preis <- event_tbl$Teilnahmebeitrag
    
    #------------
    #Beschreibung
    #------------
    beschreibung <- event_tbl$beschreibung
    beschreibung%>%
      #str_replace_all("<p>","")%>%
      str_replace_all("\n","")%>%
      str_replace_all("[:space:]{2,}", " ") -> beschreibung
    
    #------------
    #Veranstalter
    #------------
    Veranstalter <- "profamilia"
    
    
    #df <- data.frame(title, datum_von, datum_bis, uhr_von, uhr_bis, ort, preis, beschreibung, event_url)
    df <- data.frame(Veranstalter,"Titel"= title, "Datum_von" = datum_von, "Datum_bis" = datum_bis ,"Uhrzeit_von"= uhr_von, "Uhrzeit_bis"= uhr_bis,"Ort"= ort,"Preis"= preis,"Beschreibung"= beschreibung,"Event_URL" = event_url)
    
    return(df)
  }
  
  map_dfr(links, get_event_data) -> df_events
  
  #-----------------------------------
  #Ort in Strasse, PLZ, Ort auftrennen
  #-----------------------------------
  df_events$Ort -> ort_raw
  
  data.frame(ort_raw)->df_ort_raw
  
  df_ort_raw%>%
    mutate(ort_raw = str_replace(ort_raw,".+",""))%>% #bis zum ersten Absatz alles entfernen -> entfernen der Unterkunft /hotel etc.
    mutate(ort_raw = str_replace_all(ort_raw,"[:space:]{2}"," "))%>% #mehr als 2 leerzeichen entfernen
    mutate(ort_raw = str_replace_all(ort_raw,"\n",""))%>%#zeilenumbrüche entfernen
    mutate(ort_raw = str_replace(ort_raw,"^[:space:]{1}", ""))%>%
    mutate(ort_raw = str_replace_all(ort_raw,"-", " "))%>%
    mutate(ort_raw = str_replace_all(ort_raw,"ß", "ss"))-> df_ort_raw_1 #Leerzeichen am anfang entfernen
  
  str_split_fixed(df_ort_raw_1$ort_raw, "," ,3 )->df_ort_raw_2 #String in 3 Spalten anhand des Kommas teilen
 
  data.frame(df_ort_raw_2) -> df_ort_raw_3
  
  mutate(df_ort_raw_3, plz = str_extract(df_ort_raw_3$X3, "[0-9]{5}"))-> df_ort_raw_4 #PLZ in separate Spalte schreiben
  
  lapply(df_ort_raw_4, function(f){is.na(f)<-which(f == '');f})-> df_ort_raw_5 #leere zellen mit NA versehen
  data.frame(df_ort_raw_5)->df_ort_raw_5
  
  df_ort_raw_5[,c(1,3,4)]-> df_adresse #nur die drei benötiten spalten auswählen
  
  #wenn keine PLZ (5-stellige zah) eingetragen ist wird die Strasse und der ort ebenfalls auf NA gesetzt da vermutlich die adresse nicht korrekt eingegeben wurde.
  mutate(df_adresse,strasse = ifelse(is.na(df_adresse$plz) ,NA , as.character(df_adresse$X1)))-> df_adresse_1
  mutate(df_adresse_1,city = ifelse(is.na(df_adresse_1$plz) ,NA , as.character(df_adresse_1$X3)))-> df_adresse_1
  #aus der Spalte Strasse wird die plz entfernt da diese bereits in einer seperaten spalte sich befindet
  mutate(df_adresse_1, city = str_replace(df_adresse_1$city,"[0-9]{5}",""))->df_adresse_1
  
  df_adresse_1[,c("strasse","plz","city")]-> df_adresse_split#nur die benötigten Spalten werden ausgewählt

  #neuen data frame erstellen
  data.frame(df_events,df_adresse_split)->df_neu
  
  
  #----------------
  #lng lat
  #----------------
  
  lng <- "9,93072" #kann auch auf NA gesetzt werden
  lat <- "49,79692"
  
  #-------------------
  #datentypen und Spaltennamen für den Data Frame anpassen
  #-------------------
  title <- as.character(df_neu$Titel)
  url <- as.character(df_neu$Event_URL)
  description <- as.character(df_neu$Beschreibung)
  lng <- as.numeric(lng)#nochmal anpassen
  lat <- as.numeric(lat)#nochmal anpassen
  city <- as.character(df_neu$city)
  street <- as.character(df_neu$strasse)
  zip <- as.numeric(as.character(df_neu$plz))#ist in einem faktor gespeichert. dieser muss zuerst ein char-typecast erfolgen. anschliesend is.numeric typecast
  date_start <- as.Date(df_neu$Datum_von)
  date_end <- as.Date(df_neu$Datum_bis)
  time_start <- times(df_neu$Uhrzeit_von)
  time_end <- times(df_neu$Uhrzeit_bis)
  price <- as.character(df_neu$Preis)
  organizer <- as.character(df_neu$Veranstalter)
  
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
  
  #df übergeben und es wird lng und lat in df eingetragen
  geo_code_function(df)->df_komplett
  
  str(df_komplett)
  
  return(df_komplett)
}#für die gesamte funktion




