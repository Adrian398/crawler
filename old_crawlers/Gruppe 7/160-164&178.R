#library(tidyverse)
#library(rvest)
#library(lubridate)
#library(chron)
#library(ggmap)
#library(devtools)

#devtools::install_github("dkahle/ggmap")

#register_google(key = "AIzaSyBBlmECdS2894lj2y0l3qNfVeBKF79Lmik")


# Allgemeines Data Frame

df <- data.frame(title = character(),
                 url = character(),
                 description = character(),
                 lng = numeric(),
                 lat = numeric(),
                 city = character(),
                 street = character(),
                 zip = numeric(),
                 date_start = as.Date(character()),
                 date_end = as.Date(character()),
                 time_start = times(),
                 time_end = times(),
                 price = character(),
                 organizer = character(),
                 stringsAsFactors = F
)


# Theater Spielberg -------------------------------------------------------

# Anfang: Veranstalterinformationen herausfinden 
url_spielberg <- "https://www.theater-spielberg.de/wordpress/impressum/"

url_spielberg %>%
  read_html() %>%
  html_nodes(".textwidget p") %>%
  html_text() -> impressum

impressum %>%
  str_split("\\\n") %>%
  unlist() -> impressum2

veranstalter <- impressum2[5]
strasse <- impressum2[6]
ort2 <- impressum2[7]

ort2 %>%
  str_extract("\\d{5}") -> PLZ
ort2 %>%
  str_extract(" .*") %>%
  str_replace_all(" ", "") -> ortsname

preVeranstaltrUndAdresse <- str_c(veranstalter, strasse, ort2, sep = ",")

LatAndLong <- geocode(preVeranstaltrUndAdresse)

lat <- LatAndLong[2]
lon <- LatAndLong[1]
# Ende: Veranstalterinformationen herausfinden 

url_spielberg <- "https://www.theater-spielberg.de/wordpress/events/list/"

url_spielberg %>%
  read_html() -> raw_data_sb

raw_data_sb %>% 
  html_nodes(".tribe-event-url") %>%
  html_attr("href") -> links_sb

titel_selector <- ".tribe-events-single-event-title"
datum_selector <- "#tribe-events-content .tribe-event-date-start"
beschreibung_selector <- ".tribe-events-content"

url <- links_sb[1]

getInfo_sb <- function(url){
  
  url %>%
    read_html() -> raw
  
  raw %>%
    html_nodes(titel_selector) %>%
    html_text() -> titel2
  raw %>%
    html_nodes(datum_selector) %>%
    html_text() -> datum2
  raw %>%
    html_nodes(beschreibung_selector) %>%
    html_text(trim = T) -> beschreibung2
  
  datum2 %>%
    str_split("um") %>%
    unlist() %>%
    str_replace_all(" ", "") -> datum3
  
  curr_year <- str_extract(Sys.Date(), "^....")
  
  datum4 <- as.Date(datum3[1], format = '%d.%B')
  
  monat_veranstaltung <- month(as.POSIXlt(datum4, format="%Y/%m/%d"))
  monat_aktuell <- month(as.POSIXlt(Sys.Date(), format="%Y/%m/%d"))
  
  if(monat_veranstaltung < monat_aktuell){
    year(datum4) <- year(datum4) + 1
  }
  
  datum_final <- datum4
  
  uhrzeit <- datum3[2]
  
  uhrzeit_final <- paste0(uhrzeit, ":00")
  
  df2 <- data.frame(title = titel2,
                   url = NA, 
                   description = beschreibung2,
                   lng = lon[1,1],
                   lat = lat[1,1],
                   city = ortsname,
                   street = strasse,
                   zip = PLZ,
                   date_start = datum_final,
                   date_end = datum_final,
                   time_start = times(uhrzeit_final),
                   time_end = times(NA),
                   price = NA,
                   organizer = veranstalter,
                   stringsAsFactors = F
  )
  df <- rbind(df, df2)
  
}

data_sb <- map_df(links_sb, getInfo_sb)
data_sb$url <- links_sb
data_sb$time_start <- times(data_sb$time_start)
# data_sb$time_end <- times(data_sb$time_end) macht NA's zu string NA


# Burkardushaus -----------------------------------------------------------
# kommen so gut wie keine veranstaltungen vor, lediglich berichte über vergangene
#url <- "https://www.burkardushaus.de/seiten/aktuell/index.html?pageindex=30&component-id=1795bffb-424d-468e-b92d-a01a5244f679#1795bffb-424d-468e-b92d-a01a5244f679"
# obere url ist ein Event (man muss schon ein bisschen suchen bis man überhaupt eins findet)

# Definition: Event liegt vor wenn es einen subtitle gibt der mit "Am" beginnt

# Anfang: Veranstalterinformationen herausfinden 
url <- "https://www.burkardushaus.de/kontakt-und-anfahrt"

url %>%
  read_html() %>%
  html_node(".eventcontent") %>%
  html_text() -> impressum

impressum %>%
  str_replace_all("\\t", "") %>%
  str_replace_all("\\n", "") %>%
  str_split("\\\r") %>%
  unlist() -> impressum2

veranstalter <- impressum2[7]

impressum2[13] %>%
  str_replace_all(",", "") %>%
  trimws("both") -> strasse

PLZ <- impressum2[14]
ortsname <- impressum2[15]

ort2 <- str_c(PLZ, " ", ortsname)

preVeranstaltrUndAdresse <- str_c(veranstalter, strasse, ort2, sep = ",")

LatAndLong <- geocode(preVeranstaltrUndAdresse)

lat <- LatAndLong[2]
lon <- LatAndLong[1]
# Ende: Veranstalterinformationen herausfinden


url1_burkhaus <- "https://www.burkardushaus.de/seiten/aktuell/index.html?pageindex="
url2_burkhaus <- as.character(seq(0,40,by=5))
url3_burkhaus <- "&component-id=1795bffb-424d-468e-b92d-a01a5244f679#1795bffb-424d-468e-b92d-a01a5244f679"

url_burkhaus <- str_c(url1_burkhaus, url2_burkhaus, url3_burkhaus)


getContent_bh <- function(url){
  url %>%
    read_html() -> raw_data_bh
  
  raw_data_bh %>%
    html_nodes(".newscontent") -> node_data_bh
  
  title_selector <- ".title a"
  event_selector <- ".shorttext~ .subtitle"
  
  node_data_bh %>%
    html_node(title_selector) %>%
    html_attr("href") -> title
  node_data_bh %>%
    html_node(event_selector)%>%
    html_text() -> event
  
  df <- data.frame(title = title, event = event)
  
}

data_bh1 <- map_df(url_burkhaus, getContent_bh)
data_bh1 %>%
  filter(!is.na(event)) %>%
  filter(str_detect(event, "^Am"))-> data_bh2
# in data_bh2 stehen jetzt nur die Events die mit Am anfangen und dadurch als tatsächliche Events gelten


if(nrow(data_bh2)==0){
  print("keine aktuellen Events gefunden")
} else{
  links_bh <- data_bh2[,1]
  links_bh <- str_c("https:", links_bh)
  
  
  getContent2_bh <- function(url){
    url %>%
      read_html() -> node_data_bh2
    
    datum_selector <- ".subtitle"
    titel_selector <- ".title"
    beschreibung_selector <- "span h2"
    
    node_data_bh2 %>%
      html_node(titel_selector) %>%
      html_text() -> titel 
    node_data_bh2 %>%
      html_node(datum_selector) %>%
      html_text() -> datum
    node_data_bh2 %>%
      html_node(beschreibung_selector) %>%
      html_text() -> beschreibung
    
    datum_match <- "\\d{2}\\..+\\d{4}"
    
    datum %>%
      str_extract(datum_match) %>%
      str_replace(" ", "") %>%
      str_replace(" ", ".") %>%
      as.Date(format='%d. %B. %Y') -> datum_final
    
    uhrzeit_match <- "\\d{1,2}\\:\\d{2}"
    
    datum %>%
      str_extract(uhrzeit_match) %>%
      str_c(":00") -> uhrzeit_final
    
    
    df2 <- data.frame(title = titel,
                      url = NA, #data_bh2$title, 
                      description = beschreibung,
                      lng = lon[1,1],
                      lat = lat[1,1],
                      city = ortsname,
                      street = strasse,
                      zip = PLZ,
                      date_start = datum_final,
                      date_end = datum_final,
                      time_start = uhrzeit_final,
                      time_end = times(NA),
                      price = NA,
                      organizer = veranstalter,
                      stringsAsFactors = F
    )
    
    df <- rbind(df, df2)
    
  }
  
  data_bh <- map_df(links_bh, getContent2_bh)
  data_bh$url <- links_bh
  data_bh$time_start <- times(data_bh$time_start)
  #data_bh$time_end <- times(data_bh$time_end)
  
}


# Buchladen Neuer Weg -----------------------------------------------------
# zur Zeit des Aufrufs nur eine Veranstaltung; keine Ahnung wie sich zweite Veranstaltung eingliedert

# so gut wie unmöglich die Veranstalterinformationen (strasse, ort, usw) dynamisch abzugreifen
# beim einladen der Daten erfolgen keinerlei Umbrüche an denen gesplittet werden könnte 
# siehe code
#
# url <- "https://www.neuer-weg.com/impressum"
# 
# url %>%
#   read_html() %>%
#   html_nodes("#footer-area") %>%
#   html_text()

# gleiche ergebnisse mit #block-block-3 p oder p:nth-child(2) 


veranstalter <- "Buchladen Neuer Weg"
strasse <- "Sanderstraße 23-25"
PLZ <- "97070"
ortsname <- "Würzburg"
lat <- 49.78852
lon <- 9.931

url_nw <- "https://www.neuer-weg.com/Veranstaltungen/Aktuell"

url_nw <- "https://www.neuer-weg.com/node/6148"

url_nw %>% 
  read_html -> raw_nw

info_selector <- ".title"
beschreibung_selector_nw <- "p:nth-child(6) , p:nth-child(7) , p:nth-child(5)"

raw_nw %>% 
  html_nodes(info_selector) %>% 
  html_text() -> info_nw
raw_nw %>% 
  html_nodes(beschreibung_selector_nw) %>% 
  html_text() -> beschreibung_nw

# haben Veranstaltung inzwischen entfernt, welche ich am Anfang noch gefunden habe
# habe den Code jetzt angepasst aber konnte es nicht mehr testen da zum Aufrufzeitpunkt keine Events gepflegt sind
if(identical(info_nw, character(0))){
  print("keine Veranstaltungen bei Buchladen Neuer Weg")
}else{
  beschreibung2_nw <- NULL
  for(i in 1:length(beschreibung_nw)){
    beschreibung2_nw <- str_c(beschreibung2_nw," ", beschreibung_nw[i])
  }
  
  datum_match <- "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}"
  titel_match <- "Uhr.*"
  uhr_match <- "([[:digit:]]|:){1,5}(Uhr| Uhr)"
  
  info_nw %>% 
    str_extract(datum_match) -> datum
  info_nw %>% 
    str_extract(titel_match) %>%
    str_replace("Uhr", "") %>% 
    str_trim(side ="left") -> titel
  info_nw %>%
    str_extract(uhr_match) -> uhrzeit
  
  uhrzeit %>%
    str_replace_all("(Uhr| Uhr)", "") -> uhrzeit2
  
  #"20 Uhr"    "20:15 Uhr" "20 Uhr"    NA          "20 Uhr"    "20:Uhr"  kommt mit allen klar
  uhrzeit3 <- ifelse(grepl("\\:$", uhrzeit2), str_c(uhrzeit2,"00"), uhrzeit2)
  uhrzeit_final <- ifelse(grepl("^\\d{2}$", uhrzeit3), str_c(uhrzeit3,":00"), uhrzeit3)
  uhrzeit_final <- str_c(uhrzeit_final, ":00")
  
  datum_final <- as.Date(datum, format='%d. %M. %Y')
  
  data_nw <- data.frame(title = titel,
                    url = NA, 
                    description = beschreibung2_nw,
                    lng = lon[1,1],
                    lat = lat[1,1],
                    city = ortsname,
                    street = strasse,
                    zip = PLZ,
                    date_start = datum_final,
                    date_end = datum_final,
                    time_start = uhrzeit_final,
                    time_end = NA,
                    price = NA,
                    organizer = veranstalter,
                    stringsAsFactors = F)
  
  data_nw$time_start <- times(data_nw$time_start)
  data_nw$time_end <- times(data_nw$time_end)
}



# Deutschhaus Gymnasium ---------------------------------------------------

# Anfang: Veranstalterinformationen herausfinden 
url_dh <- "http://www.deutschhaus.de/veranstaltungen-amp-news/"

url_dh %>%
  read_html() %>%
  html_nodes("#Logo div") %>%
  html_text() -> impressum

impressum %>%
  str_replace_all("\\t", "") %>%
  str_replace_all("\\n", "") %>%
  str_split("\\\r") %>%
  unlist() -> impressum2

veranstalter <- impressum2[2]
strasse <- impressum2[3]
ort2 <- impressum2[4]

ort2 %>%
  str_extract("\\d{5}") -> PLZ
ort2 %>%
  str_extract(" .+") %>%
  trimws("both") -> ortsname

preVeranstaltrUndAdresse <- str_c(veranstalter, strasse, ort2, sep = ",")

LatAndLong <- geocode(preVeranstaltrUndAdresse)

lat <- LatAndLong[2]
lon <- LatAndLong[1]
# Ende: Veranstalterinformationen herausfinden 


url_dh %>% 
  read_html() -> raw_dh

titel_selector <- "#Content a"
datum_selector <- ".cal_list_date"

raw_dh %>%
  html_nodes(titel_selector) %>%
  html_text(trim = T) -> titel
raw_dh %>%
  html_nodes(datum_selector) %>%
  html_text() -> datum

titel %>%
  str_replace_all("[:space:]{4,}", "|") %>%
  str_split_fixed("\\|", 2) -> data_dh

# muss klammer am ende sein und die klammer muss mit zwei zahlen starten
uhrzeit_match <- "\\((\\d{2})(.*)\\)$"
titel %>%
  str_extract_all(uhrzeit_match) -> uhrzeit2 

uhrzeit3 <- unlist(lapply(uhrzeit2,function(x) if(identical(x,character(0))) ' ' else x))
uhrzeit3 %>%
  str_replace_all("\\(", "") %>%
  str_replace_all("\\)", "") %>%
  str_replace_all(" ", "") -> uhrzeit3

# eigentlich relevanter match, hole nur die Zeitspannen die klar definiert sind 
uhrzeit_match3 <- "((([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9])[:space:]?-[:space:]?(([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]))"

uhrzeit3 %>%
  str_extract_all(uhrzeit_match3) -> uhrzeit4
uhrzeit4 <- unlist(lapply(uhrzeit4,function(x) if(identical(x,character(0))) ' ' else x))

# aufsplitten der Zeiten / quasi in von und bis feld
uhrzeit4 %>%
  str_replace_all(" ", "") %>%
  str_split_fixed("-", 2) -> uhrzeit5

# :00 bei verfügbaren Zeiten anhängen
for(i in 1:nrow(uhrzeit5)){
  for(j in 1:ncol(uhrzeit5)){
    if(uhrzeit5[i,j]!=""){
      uhrzeit5[i,j] <- paste0(uhrzeit5[i,j], ":00")
    }
  }
}

start_uhrzeit <- uhrzeit5[,1]
end_uhrzeit <- uhrzeit5[,2]

# NA eintrag entfernen
#data_dh <- data_dh[-1,]

data_dh2 <- cbind(data_dh, start_uhrzeit, end_uhrzeit)

colnames(data_dh2) <- c("Datum", "Titel", "Start_Uhrzeit", "End_Uhrzeit")

data_dh2 <- as.data.frame(data_dh2,stringsAsFactors=FALSE)

data_dh2$Datum <- str_replace_all(data_dh2$Datum, " ", "")

data_dh2 %>%
  separate(Datum, into = c("von", "bis"), sep= "\\-") -> data_dh3

# Test für Jahreswechsel: data_dh3 <- rbind(data_dh3, c("30.12","03.01"))

# fülle leere felder mit NA auf
for(i in 1:nrow(data_dh3)){
  for(j in 1:ncol(data_dh3)){
    if(data_dh3[i,j]== " "||data_dh3[i,j]== ""||is.na(data_dh3[i,j])){
      data_dh3[i,j] <- NA
    }
  }
}

# füge enddatum gleich anfangsdatum wenn kein zeitraum
for(i in 1:nrow(data_dh3)){
  if(is.na(data_dh3[i,"bis"])){
    data_dh3[i,"bis"] <- data_dh3[i,"von"]
  }
}

monat_aktuell <- month(as.POSIXlt(Sys.Date(), format="%Y/%m/%d"))

# konvertiere von/bis datum zu Date
data_dh3$von <- as.Date(data_dh3$von, format ='%d.%m')
data_dh3$bis <- as.Date(data_dh3$bis, format ='%d.%m')

# Jahreswechsel für "von" Spalte
for(i in 1:nrow(data_dh3)){
  monat_veranstaltung <- month(as.POSIXlt(data_dh3[i, "von"], format="%Y/%m/%d"))
  
  if(!is.na(data_dh3[i, "von"])){
    if(monat_veranstaltung < monat_aktuell){
      year(data_dh3[i, "von"]) <- year(data_dh3[i, "von"]) + 1
    }
  }
}
# Jahreswechsel für "bis" Spalte
for(i in 1:nrow(data_dh3)){
  monat_veranstaltung <- month(as.POSIXlt(data_dh3[i, "bis"], format="%Y/%m/%d"))
  
  if(!is.na(data_dh3[i, "bis"])){
    if(monat_veranstaltung < monat_aktuell){
      year(data_dh3[i, "bis"]) <- year(data_dh3[i, "bis"]) + 1
    }
  }
}


df_dh <- data.frame(title = data_dh3[,"Titel"],
                    url = url_dh,
                   description = NA,
                   lng = lon[1,1],
                   lat = lat[1,1],
                   city = ortsname,
                   street = strasse,
                   zip = PLZ,
                   date_start = data_dh3[,"von"],
                   date_end = as.Date(data_dh3[, "bis"]),
                   time_start = data_dh3[,"Start_Uhrzeit"],
                   time_end = data_dh3[, "End_Uhrzeit"],
                   price = NA,
                   organizer = veranstalter,
                   stringsAsFactors = F
)

df_dh <- df_dh[-1,]
data_dh <- df_dh

# Bürgerspital ------------------------------------------------------------

# Anfang: Veranstalterinformationen herausfinden 
url <- "https://www.buergerspital.de/wir-ueber-uns/index.html"

url %>%
  read_html() %>%
  html_nodes(".a1") %>%
  html_text() -> impressum

impressum %>%
  str_replace_all("[:space:]{3,}", "|") %>%
  str_split("\\|") %>%
  unlist() -> impressum2

veranstalter <- impressum2[1]
strasse <- impressum2[2]
ort2 <- impressum2[3]

ort2 %>%
  str_extract("\\d{5}") -> PLZ
ort2 %>%
  str_extract(" .+") %>%
  trimws("both") -> ortsname

preVeranstaltrUndAdresse <- str_c(veranstalter, strasse, ort2, sep = ",")

LatAndLong <- geocode(preVeranstaltrUndAdresse)

lat <- LatAndLong[2]
lon <- LatAndLong[1]
# Ende: Veranstalterinformationen herausfinden 

url_bs <- "https://www.buergerspital.de/aktuelles/termine/index.html"

url_bs2 <- "?ev%5Bsearch%5D=&ev%5Bcat%5D=&ev%5Bstart%5D="
url_bs3 <- format(Sys.time(), '%d.%m.%Y')
url_bs4 <- "&ev%5Bend%5D="

# auf 60 Tage spanne gesetzt, heißt Zeitraum von aktuellen Daten bis 60 Tage in die Zukunft wird abgefragt
zeitspanne <- Sys.Date() + 60

url_bs5 <- format(zeitspanne, '%d.%m.%Y')
url_bs6 <- "&ev%5Bsubcat%5D="

start_link <- str_c(url_bs, url_bs2, url_bs3, url_bs4, url_bs5, url_bs6)

start_link %>%
  read_html() %>%
  html_node("br+ .pager center") %>%
  html_text() -> anz_seiten

anz_seiten %>%
  str_extract("\\(\\d{1,2} Seiten\\)") %>%
  str_replace("Seiten", "") %>%
  str_replace("\\(", "") %>%
  str_replace("\\)", "") %>%
  str_replace(" ", "") %>%
  as.integer() -> anz_seiten

url_bs7 <- "&page="

# falls es mehrere Seiten für die Ergebnisse gibt
if(!is.na(anz_seiten)){
  # - 1 fängt bei 0 an
  url_bs8 <- seq(0, anz_seiten -1, 1)
}else{
  url_bs8 <- ""
}

url_bs <- str_c(url_bs, url_bs2, url_bs3, url_bs4, url_bs5, url_bs6, url_bs7, url_bs8)

#url_bs <- "https://www.buergerspital.de/aktuelles/termine/index.html?ev%5Bsearch%5D=&ev%5Bcat%5D=&ev%5Bstart%5D=12.06.2018&ev%5Bend%5D=11.08.2018&ev%5Bsubcat%5D=&page=0"

df2_sb <- data.frame(start_zeit = character(), end_zeit = character(), link = character())

#url <- url_bs[1]

getWebsites <- function(url){
  url %>%
    read_html() -> raw_data_bs 
  
  raw_data_bs %>%
    html_nodes(".title a") %>%
    html_attr("href") -> links_bs
  
  #letzten zwei Einträge sind Links für News und Jobs und keine Events (konnten nicht mit Gadget Selector abgewählt werden) 
  rm1 <- length(links_bs) -2
  
  links_bs <- links_bs[c(1:rm1)]
  
  links_bs <- str_c("https://www.buergerspital.de/", links_bs)
  
  raw_data_bs %>%
    html_nodes(".ev_datetime") %>%
    html_text() -> uhrzeit_bs
  
  uhrzeit_match <- "((([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9])[:space:]?-[:space:]?(([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]))"
  
  uhrzeit_bs %>%
    str_extract(uhrzeit_match) -> uhrzeit
  
  uhrzeit %>%
    str_replace_all(" ", "") %>%
    str_split_fixed("-", 2) -> uhrzeit2
  
  for(i in 1:nrow(uhrzeit2)){
    for(j in 1:ncol(uhrzeit2)){
      if(uhrzeit2[i,j]!=""){
        uhrzeit2[i,j] <- paste0(uhrzeit2[i,j], ":00")
      }
    }
  }

  start_zeit <- uhrzeit2[,1]
  end_zeit <- uhrzeit2[,2]

  # raw_data_bs %>%
  #   html_nodes(".eventrowday b") %>%
  #   html_text() -> datum_bs
  # 
  # datum_match <- "\\d{2}\\.\\d{2}\\.\\d{4}"
  # 
  # datum_bs %>%
  #   str_extract(datum_match) -> datum
  
  df1_sb <- data.frame(start_zeit = start_zeit, end_zeit = end_zeit , link = links_bs)
  df2_sb <- rbind(df2_sb, df1_sb)
  
}

df3_sb <- map_df(url_bs, getWebsites)

for(i in 1:nrow(df3_sb)){
  for(j in 1:ncol(df3_sb)){
    if(df3_sb[i,j]== " "||df3_sb[i,j]== ""){
      df3_sb[i,j] <- NA
    }
  }
}

#df3_sb$datum <- as.Date(df3_sb$datum, format = '%d.%m.%Y')
df3_sb$start_zeit <- as.character(df3_sb$start_zeit)
df3_sb$end_zeit <- as.character(df3_sb$end_zeit)
df3_sb$link <- as.character(df3_sb$link)

links2_sb <- df3_sb[, "link"]

getContent_bs <- function(url){
  url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_node(".artikelansicht_classic div b") %>%
    html_text() %>% 
    .[1] -> titel
  
  raw_data %>%
    html_nodes(".artikelansicht_classic p") %>%
    html_text() -> beschreibung
  
  raw_data %>%
    html_nodes(".artikelansicht_classic p") %>%
    grep("\\€", value = T,.) -> prePreis
  
  if(identical(prePreis, character(0))){
    PREISPREIS <- NA
  } else {
    prePreis %>% 
    read_html() %>% 
      html_text() %>% 
      str_extract("[0-9]{1,4}(\\.|,)?([0-9]{2})? €") %>% 
      str_remove_all(" \\€") %>% 
      as.character() -> PREISPREIS  
  }
  
  raw_data %>%
    html_nodes("td") %>%
    html_text() %>% 
    str_extract("[0-9]{2}\\.[0-9]{2}\\.20[0-9]{2}") %>% 
    .[1] %>% 
    as.Date("%d.%m.%Y") -> datum
  
  beschreibung2_bs <- NULL
  for(i in 1:length(beschreibung)){
    beschreibung2_bs <- str_c(beschreibung2_bs," ", beschreibung[i])
  }
  
  df <- data.frame(title = titel, description= beschreibung2_bs, date_start = datum, price = PREISPREIS)
}

df_bs <- map_df(links2_sb, getContent_bs)

df_bs$time_start <- df3_sb$start_zeit
df_bs$time_end <- df3_sb$end_zeit
#df_bs$date_start <- df_bs$datum
df_bs$date_end <- df_bs$date_start
df_bs$url <- df3_sb$link
df_bs$organizer <- veranstalter
df_bs$street <- strasse
df_bs$zip <- PLZ
df_bs$city <- ortsname
df_bs$lat <- lat[1,1]
df_bs$lng <- lon[1,1]
#df_bs$price <- NA

data_bs <- df_bs



# Merge -------------------------------------------------------------------

if(exists("data_nw")){
  veranstaltungen <- rbind(data_sb, data_dh, data_bs, data_bh, data_nw)
}else{
  veranstaltungen <- rbind(data_sb, data_dh, data_bs, data_bh)
}

veranstaltungen$time_end <- times(veranstaltungen$time_end)
veranstaltungen$price <- as.character(veranstaltungen$price)

#rm(list = setdiff(ls(), "veranstaltungen"))

