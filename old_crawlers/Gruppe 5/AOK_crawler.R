#Veranstaltungs-Crawler
#AOK crawler
#http://www.aok-wuerzburg.de/
#https://bayern.aok.de/landingpages/gesundheitsangebote/?id=941&q=w%C3%BCrzburg&r=25

AOK_crawler <- function(){

  library(tidyverse)
  library(rvest)
  require(stringr)
  library(RSelenium)
  library(lubridate)
  library(chron)
  library(plyr)
  source("geo_code_function.R")
  
  #---------------------
  # Setting up RSelenium
  #---------------------
  #library(devtools)
  #install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
  #install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
  #install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
  
  #zu crawlende Website
  url <- "https://bayern.aok.de/landingpages/gesundheitsangebote/?id=941&q=w%C3%BCrzburg&r=25" # ort und radius angegeben # plz 97074
  
  
  # Start Browser mit Selenium
  #----------------------
  rD <- rsDriver()
  remDr <- rD[["client"]]
  #remDr$navigate("http://www.google.com")
  remDr$navigate(url)
  
  #Ende der Webseite ermitteln
  webElem <- remDr$findElement("css", "body")
  
  #Website mehrmals nach unten scrollen
  i <- 1
  while (i<5){
    try(webElem$sendKeysToElement(list(key = "end")))#Website nach unten scrollen
    i <- i + 1
    Sys.sleep(2)
    #finally = print(paste("Page scrolled", i, "times"))
  }
  
  # Get HTML
  raw_data <- read_html(remDr$getPageSource()[[1]])#geladene Seite verwenden
  
  raw_data %>%
    html_nodes(".offer-search-item") -> node_data
  
  title_selector <- ".offer-search-item__name a"
  datum_selector <- ".details-row:nth-child(7) .details-column:nth-child(1) p"
  ort_selector <- ".details-row:nth-child(6) p"
  preis_selector <- ".details-column:nth-child(2) p"
  
  link_selector <- ".icon-f-arrow-right-circle"
  
  #zu crawlende objekte auf der Page
  node_data %>%
    html_node(title_selector) %>%
    html_text(trim = TRUE) -> title
  node_data %>%
    html_node(datum_selector) %>%
    html_text(trim = TRUE) -> raw_datum
  node_data %>%
    html_node(ort_selector) %>%
    html_text(trim = TRUE) -> raw_ort
  node_data%>%
    html_node(preis_selector)%>%
    html_text(trim = TRUE)-> raw_preis
  node_data%>%
    html_nodes(link_selector)%>%
    html_attr("href")-> raw_link
  
  
  #-------
  #Datum
  #-------
  regex_datum = "[0-9]{1,2}[\\./]{1}[0-9]{1,2}[\\./]?(20[1-2]{1}[0-9]{1})?"
  raw_datum %>%
    str_extract_all(regex_datum, simplify = TRUE)->datum #nur ersten wert entnehmen und als von nehmen zweiter wert als bis nehmen
  
  dmy(datum[,1]) -> datum_von #datum string in datum date umwandeln (Typecast)
  dmy(datum[,2]) -> datum_bis
  
  #--------
  #Uhrzeit
  #--------
  regex_uhrzeit = "[0-9]{1,2}[:]{1}[0-9]{1,2}"
  raw_datum %>%
    str_extract_all(regex_uhrzeit, simplify = TRUE)->uhrzeit #nur ersten wert entnehmen und als von nehmen zweiter wert als bis nehmen
  
  #uhrzeit
  uhrzeit[,1]-> uhr_von
  uhr_von <- times(paste0(uhr_von, ":00"))
  #as.character(uhr_von) -> uhr_von
  uhrzeit[,2]-> uhr_bis
  uhr_bis <- times(paste0(uhr_bis, ":00"))
  #as.character(uhr_bis) -> uhr_bis
  
  #hm(uhrzeit[,1]) -> uhr_von #typecast in date format
  
  #as.character(uhr_von) -> uhr_von
  #hm(uhrzeit[,2]) -> uhr_bis
  
  #as.character(uhr_bis) -> uhr_bis
  
  
  #--------------------
  #Preis
  #---------------------
  regex_preis = "\\(?[0-9,.]+\\)?"
  raw_preis%>%
    str_extract(regex_preis)->preis
  
  
  #--------------------
  #Links bearbeiten wenn nötig
  #---------------------------
  raw_link -> link
  
  #-------------------------
  #Veranstalter
  #------------------------
  Veranstalter <- "AOK"
  
  #--------------------
  #Beschreibung mit linkverweis
  #---------------------
  #Beschreibung zu der Veranstaltung ist auf der eigens daf?r vorgesehenen Seite. 
  #Es wirt mit einem Link auf die weiter Seite verwiesen.
  
  get_event_daten <- function(event_url){
    event_url %>%
      read_html() -> raw_event_data
    
    beschreibung_selector <- ".content-text__content p"
    strasse_selector <- ".value div span:nth-child(1)"
    plz_selector <- "br+ span"
    stadtname_selector <- ".value span+ span"
    
    raw_event_data %>%
      html_node(beschreibung_selector) %>%
      html_text(trim = TRUE) -> beschreibung
    raw_event_data %>%
      html_node(strasse_selector) %>%
      html_text(trim = TRUE) -> strasse
    raw_event_data %>%
      html_node(plz_selector) %>%
      html_text(trim = TRUE) -> plz
    raw_event_data%>%
      html_node(stadtname_selector)%>%
      html_text(trim = TRUE)-> stadtname
    
    
    
    strasse%>%
      str_replace("Ort: ","")%>%
      str_replace_all("ä","ae")%>%
      str_replace_all("ö","oe")%>%
      str_replace_all("ü","ue")%>%
      str_replace_all("ß","ss")%>%
      str_replace_all("[:space:]${1,}", "")-> strasse #entfernt  das Leerzeichen am Ende des Strings
    
    stadtname%>%
      str_replace("Ort: ","")%>%
      str_replace_all("ä","ae")%>%
      str_replace_all("ö","oe")%>%
      str_replace_all("ü","ue")%>%
      str_replace_all("ß","ss")%>%
      str_replace_all("[:space:]${1,}", "")-> stadtname #entfernt  das Leerzeichen am Ende des Strings
    
    #-----------------
    #beschreibung
    #----------------
    beschreibung%>%
      str_replace_all("<p>","")%>%
      str_replace_all("</p>","")%>%
      str_replace_all("[:space:]{2,}", " ") -> beschreibung
    
    df_event_daten<- data.frame(beschreibung, strasse, plz, stadtname)
    
    return(df_event_daten)
  }
  
  map_dfr(raw_link, get_event_daten) -> event_daten
  #as.character(beschreibung)-> beschreibung # um den Vector in einen eindimensionalen vector zu bringen
  
  
  #-------------------
  #DataFrame erstellen
  #-------------------
  df_events <- data.frame(Veranstalter, title, datum_von,  datum_bis , uhr_von,  uhr_bis, preis, "beschreibung" = event_daten$beschreibung, link, "strasse" = event_daten$strasse, "plz" = event_daten$plz, "city" = event_daten$stadtname)
  #df_events
  #str(df_events)
  #--------------------
  # Shut down selenium
  #--------------------
  remDr$close()
  rm(rD)
  gc()
  
  lng <- "9,93072" #kann auch auf NA gesetzt werden
  lat <- "49,79692"
  

  #-------------------
  #datentypen und Spaltennamen für den Data Frame anpassen
  #-------------------
  title <- as.character(df_events$title)
  url <- as.character(df_events$link)
  description <- as.character(df_events$beschreibung)
  lng <- as.numeric(lng)#nochmal anpassen
  lat <- as.numeric(lat)#nochmal anpassen
  city <- as.character(df_events$city)
  street <- as.character(df_events$strasse)
  zip <- as.numeric(as.character(df_events$plz))#ist in einem faktor gespeichert. dieser muss zuerst ein char-typecast erfolgen. anschliesend is.numeric typecast
  date_start <- as.Date(df_events$datum_von)
  date_end <- as.Date(df_events$datum_bis)
  time_start <- times(df_events$uhr_von)
  time_end <- times(df_events$uhr_bis)
  price <- as.character(df_events$preis)
  organizer <- as.character(df_events$Veranstalter)
  
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
  
    geo_code_function(df)->df_event_komplett
  
  #if(FALSE){#auskommentiert
  #  lookup_tablle <- function(df){
    
   #   df_adressen <- read.csv2("address_list_group5.csv")
   #   mutate(df_adressen,key = paste0(df_adressen$street, " ",df_adressen$zip , " " , df_adressen$city))->df_adress_key #schlüssel erstellen für jon
      
   #   mutate(df ,key = paste0(df$street, " ",df$zip , " " , df$city))->df_event_key #schlüssel erstellen für join
      
   #   join_adresse_all<- left_join(df_event_key , df_adress_key , by = c("key" = "key"))#tbl events und tbl adressen werden zusammengefügt. mit dem zuvor generierten schlüssel
      
   #   join_adresse_all[,c("title","url","description","city" = "city.x","street"= "street.x","zip"="zip.x","date_start","date_end","time_start","time_end","price","lng" ,"lat", "organizer")]-> df_event_komplett#nur die benötigten Spalten werden ausgewählt
        
   #   df_event_komplett<- rename(df_event_komplett, c("city.x" = "city","street.x"= "street","zip.x"="zip"))#rename col with x
    
   # return(df_event_komplett)
   # }
  
  #df_event_komplett <- lookup_tablle(df)
  #}
    
#return(df_event_komplett)

}












