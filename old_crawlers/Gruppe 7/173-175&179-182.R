#library(rvest)
#library(tidyverse)
# devtools::install_github("dkahle/ggmap")
#library(ggmap)
#library(chron)

# devtools::install_github("dkahle/ggmap")
#register_google(key = "AIzaSyDOjGt9LQMihlMnknjDY49Qgwe2m_vqr9g")



# LOMA --------------------------------------------------------------------

lomaVeranstalter = lomaDatumVon = lomaDatumBis = lomaStartzeit = lomaEndzeit = lomaTitel = lomaBeschreibung = 
  lomaAdresse = lomaPLZ = lomaOrtsname = lomaLat = lomaLong = lomaLink = lomaEintritt = NA


lomaDf <- data.frame()

"http://www.loma-bar.com/wochenprogramm.html" -> lomaLink

lomaLink %>% 
  read_html() -> lomaURL

lomaVeranstalter <- "LOMA"

"http://www.loma-bar.com/kontakt.html" %>% 
  read_html() %>% 
  html_nodes(".Stil6+ .Stil5") %>% 
  html_text() %>% 
  str_split("\\n|Telefon") %>% 
  unlist() %>%
  str_trim("both") -> lomaAnschrift

lomaAnschrift %>% 
  grep("str\\.|(S|s)tra(ss|ß)e", value = T,.) %>% 
  .[1] -> lomaAdresse

lomaAnschrift %>% 
  grep("^[0-9]{5} ", value = T,.) %>%
  .[1] %>% 
  gsub("([0-9]{5}) (.*)", "\\1", .) -> lomaPLZ

lomaAnschrift %>% 
  grep("^[0-9]{5} ", value = T,.) %>% 
  .[1] %>% 
  gsub("([0-9]{5}) (.*)", "\\2", .) -> lomaOrtsname

preLomaVeranstaltrUndAdresse <- paste(lomaVeranstalter, lomaAdresse, lomaPLZ, lomaOrtsname, sep = ", ")

lomaLatAndLong <- geocode(preLomaVeranstaltrUndAdresse)

lomaLat <- lomaLatAndLong[2]
lomaLong <- lomaLatAndLong[1]


# gib mir das Programm der nächsten 7 Tage (inkl heute)
for (x in 0:6) {
  # Öffnungszeiten auf der Homepage nicht angegeben, daher hardcoded
  lomaStartzeit <- "19:00"
  lomaStartzeit <- times(paste0(lomaStartzeit, ":00"))
  lomaEndzeit <- "03:00"
  lomaEndzeit <- times(paste0(lomaEndzeit, ":00"))
  lomaDatumVon <- Sys.Date()+x
  lomaDatumBis <- lomaDatumVon
  tagKuerzel <- substr(weekdays(Sys.Date()+x), 1,2) 
  
  bildName <- paste0("cal_", tagKuerzel,".jpg")
  
  # lomaURL %>% 
  #  html_node("td td table") %>%
  #   html_table() -> a
  
  xml_find_all(lomaURL, './/tr') %>% 
    grep(bildName, value = T, ignore.case = T, .) %>% 
    tail(n=1) %>% 
    str_replace_all("<.*?>", " ") %>%
    str_replace_all("[[:space:]]{2,}", " ") %>% 
    str_trim("both") -> lomaTitelundBeschreibung
  
  lomaTitelundBeschreibung %>% 
    str_extract(".*:") %>% 
    str_replace(":", "") -> lomaTitel
  
  #print(lomaTitel)
  
  lomaTitelundBeschreibung %>% 
    str_extract(":.*") %>% 
    str_replace(":", "") %>% 
    str_trim("both") -> lomaBeschreibung
  
  #print(lomaBeschreibung)
  
  
  lomaDf2 <- data.frame(title = lomaTitel, url = lomaLink, description = lomaBeschreibung, lng = lomaLong, 
                        lat = lomaLat, city = lomaOrtsname, street = lomaAdresse, zip = lomaPLZ, 
                        date_start = lomaDatumVon, date_end = lomaDatumBis, time_start = lomaStartzeit, 
                        time_end = lomaEndzeit, price = lomaEintritt, organizer = lomaVeranstalter, 
                        stringsAsFactors = F)
  
  lomaDf <- rbind(lomaDf, lomaDf2)
  
}

colnames(lomaDf) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")

if( !all(is.na(lomaDf$time_start)) ){
  lomaDf$time_start <- times(lomaDf$time_start)
}
if( !all(is.na(lomaDf$time_end)) ){
  lomaDf$time_end <- times(lomaDf$time_end)
}





# Hofkeller ---------------------------------------------------------------

hofkellerVeranstalter = hofkellerDatumVon = hofkellerDatumBis = hofkellerStartzeit = hofkellerEndzeit = hofkellerTitel = hofkellerBeschreibung = 
  hofkellerAdresse = hofkellerPLZ = hofkellerOrtsname = hofkellerLat = hofkellerLong = hofkellerLink = hofkellerEintritt = NA


hofkellerVeranstalter <- "Staatlicher Hofkeller Würzburg"

"https://www.hofkeller.de/kontakt" %>% 
  read_html() %>% 
  html_nodes("p:nth-child(1)") %>% 
  str_split("<br>\n") %>% 
  unlist() %>% 
  str_remove_all("<[^>]*>") -> hofkellerAnschrift

hofkellerAnschrift %>% 
  tail(n=2) %>% 
  .[1] -> hofkellerAdresse

hofkellerAnschrift %>% 
  tail(n=2) %>% 
  .[2] %>% 
  gsub("([0-9]{5}) (.*)", "\\1", .) -> hofkellerPLZ

hofkellerAnschrift %>% 
  tail(n=2) %>% 
  .[2] %>% 
  gsub("([0-9]{5}) (.*)", "\\2", .) -> hofkellerOrtsname

if ( identical(hofkellerAdresse, character(0)) ){
  hofkellerAdresse <- NA
}
if ( identical(hofkellerPLZ, character(0)) ){
  hofkellerPLZ <- NA
}
if ( identical(hofkellerOrtsname, character(0)) ){
  hofkellerOrtsname <- NA
}

preHofkellerVeranstaltrUndAdresse <- paste(hofkellerVeranstalter, hofkellerAdresse, hofkellerPLZ, hofkellerOrtsname, sep = ", ")

hofkellerLatAndLong <- geocode(preHofkellerVeranstaltrUndAdresse)

hofkellerLat <- hofkellerLatAndLong[2]
hofkellerLong <- hofkellerLatAndLong[1]



getHofkellerEvents <- function(url) {
  
  hofkellerLink <- url
  
  url %>% 
    read_html() %>% 
    html_nodes("#product_addtocart_form h1") %>% 
    html_text() -> hofkellerDatumUndTitel
  
  if( grepl("[0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}", hofkellerDatumUndTitel) ){
    hofkellerDatumUndTitel %>% 
      gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", "\\1", .) %>% 
      parse_date("%d.%m.%Y",locale=locale("de")) -> hofkellerDatumVon
    
    hofkellerDatumBis <- hofkellerDatumVon
    
  } else if( grepl("[0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2} ", hofkellerDatumUndTitel) ){
    hofkellerDatumUndTitel %>% 
      gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}) (.*)", "\\1", .) %>% 
      parse_date("%d.%m.%y",locale=locale("de")) -> hofkellerDatumVon
    
    hofkellerDatumBis <- hofkellerDatumVon
  } else {
    hofkellerDatumVon <- as.Date(NA)
    hofkellerDatumBis <- as.Date(NA)
  }
  
  if( grepl("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", hofkellerDatumUndTitel) ) {
    hofkellerDatumUndTitel %>%
      gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", "\\2", .) -> hofkellerTitel
    
  } else if( grepl("[0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2}", hofkellerDatumUndTitel) ) {
    hofkellerDatumUndTitel %>%
      gsub("([0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2}) (.*)", "\\2", .) -> hofkellerTitel
  } else{
    hofkellerTitel <- NA
  }
  
  
  url %>% 
    read_html() %>% 
    html_nodes("#product_addtocart_form .std") %>% 
    html_text() -> preHofkellerZeit
  
  if( grepl("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}", preHofkellerZeit) )  {
    preHofkellerZeit %>% 
      str_extract("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}") %>% 
      gsub("([0-9]{1,2}(\\.|:)[0-9]{2}) bis ([0-9]{1,2}(\\.|:)[0-9]{2})", "\\1",.) %>% 
      str_replace_all("\\.", ":") -> hofkellerStartzeit
    
    hofkellerStartzeit <- times(paste0(hofkellerStartzeit, ":00"))
    
    preHofkellerZeit %>% 
      str_extract("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}") %>% 
      gsub("([0-9]{1,2}(\\.|:)[0-9]{2}) bis ([0-9]{1,2}(\\.|:)[0-9]{2})", "\\3",.) %>% 
      str_replace_all("\\.", ":") -> hofkellerEndzeit
    
    hofkellerEndzeit <- times(paste0(hofkellerEndzeit, ":00"))
    
  } else if( grepl("ab [0-9]{1,2}(\\.|:)[0-9]{2}", preHofkellerZeit) ){
    preHofkellerZeit %>% 
      gsub(".* ab ([0-9]{1,2}(\\.|:)[0-9]{2}) .*", "\\1",.) %>% 
      str_replace_all("\\.", ":") -> hofkellerStartzeit
    
    hofkellerStartzeit <- times(paste0(hofkellerStartzeit, ":00"))
    
    hofkellerEndzeit <- times(NA)
    
  } else {
    hofkellerStartzeit <- times(NA)
    hofkellerEndzeit <- times(NA)
  }
  
  if(hofkellerTitel == "ÖWP" | hofkellerTitel == "Öwp"){
    hofkellerTitel <- "Öffentliche Weinprobe"
  }
  
  url %>%
    read_html() %>% 
    html_nodes(".box-description .std") %>% 
    html_text() %>% 
    unlist() %>% 
    str_replace_all("[[:space:]]{2,}", " ") %>% 
    str_trim("both") %>% 
    noquote() -> hofkellerBeschreibung
  
  
  url %>% 
    read_html() %>% 
    html_nodes(".price") %>% 
    html_text() %>% 
    str_extract("[0-9]{1,4},[0-9]{2}") %>% 
    str_replace(",", "\\.") %>% 
    as.character() -> hofkellerEintritt
  
  hofkellerDf2 <- data.frame(title = hofkellerTitel, url = hofkellerLink, description = hofkellerBeschreibung, lng = hofkellerLong, 
                             lat = hofkellerLat, city = hofkellerOrtsname, street = hofkellerAdresse, zip = hofkellerPLZ, 
                             date_start = hofkellerDatumVon, date_end = hofkellerDatumBis, time_start = hofkellerStartzeit, 
                             time_end = hofkellerEndzeit, price = hofkellerEintritt, organizer = hofkellerVeranstalter, 
                             stringsAsFactors = F)
  colnames(hofkellerDf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
  hofkellerDf2
}

"https://shop.hofkeller.de/veranstaltungen?dir=asc&limit=100&order=position" %>% 
  read_html() %>%
  html_nodes(".link-learn") %>% 
  html_attr("href") -> hofkellerLinksammlung

hofkellerDf <- map_df(hofkellerLinksammlung, getHofkellerEvents)

if( !all(is.na(hofkellerDf$time_start)) ){
  hofkellerDf$time_start <- times(hofkellerDf$time_start)
}
if( !all(is.na(hofkellerDf$time_end)) ){
  hofkellerDf$time_end <- times(hofkellerDf$time_end)
}





# Krebsgesellschaft -------------------------------------------------------

krebsVeranstalter = krebsDatumVon = krebsDatumBis = krebsStartzeit = krebsEndzeit = krebsTitel = krebsBeschreibung = 
  krebsAdresse = krebsPLZ = krebsOrtsname = krebsLat = krebsLong = krebsLink = krebsEintritt = NA

krebsVeranstalter <- "Bayrische Krebsgesellschaft"

krebsDf <- data.frame(krebsVeranstalter = as.character(), krebsDatumVon = times(), krebsStartzeit = times(),
                      krebsEndzeit = times(), krebsTitel = as.character, krebsBeschreibung = as.character(),
                      krebsAdresse = as.character(), krebsPLZ = as.numeric, krebsOrtsname = as.character(),
                      krebsLat = as.numeric, krebsLong = as.numeric, krebsLink = as.character(),
                      krebsEintritt = as.character, stringsAsFactors = F)


getKrebsEvents <- function(url) {
  
  krebsLink <- url
  
  url %>% 
    read_html() -> url
  
  
  url %>%
    html_nodes("h2") %>% 
    html_text() %>% 
    str_trim("both") %>% 
    noquote() -> krebsTitel
  
  url %>% 
    html_nodes("time") %>% 
    html_text() %>% 
    str_remove_all("[:space:]") %>% 
    parse_date("%d.%m.%Y",locale=locale("de")) -> krebsDatumVon
  
  krebsDatumBis <- krebsDatumVon
  
  url %>% 
    html_nodes(".news-single-city") %>% 
    html_text() %>% 
    str_remove_all("[[:space:]]{2,}") -> preKrebsZeit
  
  if( grepl(" bis ", preKrebsZeit) ){
    
    preKrebsZeit %>% 
      gsub("([0-9]{1,2}(:|\\.)[0-9]{2}) bis ([0-9]{1,2}(:|\\.)[0-9]{2}).*", "\\1", .) %>% 
      str_replace("\\.", ":") -> krebsStartzeit
    
    krebsStartzeit <- times(paste0(krebsStartzeit, ":00"))
    
    preKrebsZeit %>% 
      gsub("([0-9]{1,2}(:|\\.)[0-9]{2}) bis ([0-9]{1,2}(:|\\.)[0-9]{2}).*", "\\3", .) %>% 
      str_replace("\\.", ":") -> krebsEndzeit
    
    krebsEndzeit <- times(paste0(krebsEndzeit, ":00"))
    
  } else if( grepl("[0-9]{1,2}(:|\\.)[0-9]{2} - [0-9]{1,2}(:|\\.)[0-9]{2}", preKrebsZeit) ){
    
    preKrebsZeit %>% 
      gsub("([0-9]{1,2}(:|\\.)[0-9]{2}) - ([0-9]{1,2}(:|\\.)[0-9]{2}).*", "\\1",.) %>% 
      str_replace("\\.", ":") -> krebsStartzeit
    
    krebsStartzeit <- times(paste0(krebsStartzeit, ":00"))
    
    preKrebsZeit %>% 
      gsub("([0-9]{1,2}(:|\\.)[0-9]{2}) - ([0-9]{1,2}(:|\\.)[0-9]{2}).*", "\\3",.) %>% 
      str_replace("\\.", ":") -> krebsEndzeit
    
    krebsEndzeit <- times(paste0(krebsEndzeit, ":00"))
    
  } else if ( grepl("(A|a)b [0-9]{1,2}", preKrebsZeit) ){
    
    preKrebsZeit %>% 
      str_extract("[0-9]{1,2}(:|\\.)[0-9]{2}") %>% 
      str_replace("\\.", ":") -> krebsStartzeit
    
    krebsStartzeit <- times(paste0(krebsStartzeit, ":00"))
    
    krebsEndzeit <- times(NA)
    
  } else if ( grepl("[0-9]{1,2}(:|\\.)[0-9]{2}", preKrebsZeit) ){
    
    preKrebsZeit %>% 
      str_extract("[0-9]{1,2}(:|\\.)[0-9]{2}") %>% 
      str_replace("\\.", ":") -> krebsStartzeit
    
    krebsStartzeit <- times(paste0(krebsStartzeit, ":00"))
    
    krebsEndzeit <- times(NA)
    
    
  } else {
    
    krebsStartzeit <- times(NA)
    krebsEndzeit <- times(NA)
    
  }
  
  url %>% 
    html_nodes("p:nth-child(2)") %>% 
    html_text() %>% 
    .[2] %>% 
    noquote() -> krebsBeschreibung
  
  
  url %>% 
    html_nodes("p") %>% 
    grep("Veranstalter:", value = T,. ) %>% 
    str_split("<br>") %>% 
    unlist() %>% 
    str_remove_all("<[^>]*>") %>% 
    str_trim("both") -> krebsAnschrift
  
  krebsAnschrift %>% 
    tail(n=2) %>% 
    .[1] -> krebsAdresse
  
  krebsAnschrift %>% 
    tail(n=2) %>% 
    .[2] %>% 
    gsub("([0-9]{5}) (.*)", "\\1",.) %>% 
    as.numeric() -> krebsPLZ
  
  krebsAnschrift %>% 
    tail(n=2) %>% 
    .[2] %>% 
    gsub("([0-9]{5}) (.*)", "\\2",.) -> krebsOrtsname
  
  
  if ( identical(krebsAdresse, character(0)) ){
    krebsAdresse <- NA
  }
  if ( identical(krebsPLZ, character(0)) ){
    krebsPLZ <- NA
  }
  if ( identical(krebsOrtsname, character(0)) ){
    krebsOrtsname <- NA
  }
  
  preKrebsVeranstaltrUndAdresse <- paste(krebsVeranstalter, krebsAdresse, krebsPLZ, krebsOrtsname, sep = ", ")
  
  krebsLatAndLong <- geocode(preKrebsVeranstaltrUndAdresse)
  
  krebsLat <- krebsLatAndLong[2]
  krebsLong <- krebsLatAndLong[1]
  
  
  
  krebsDf2 <- data.frame(title = krebsTitel, url = krebsLink, description = krebsBeschreibung, lng = krebsLong, lat = krebsLat, city = krebsOrtsname, 
                         street = krebsAdresse, zip = krebsPLZ, date_start = krebsDatumVon, date_end = krebsDatumBis, time_start = krebsStartzeit, 
                         time_end = krebsEndzeit, price = krebsEintritt, organizer = krebsVeranstalter, stringsAsFactors = F)
  colnames(krebsDf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
  
  krebsDf <- rbind(krebsDf, krebsDf2)
  
}


"https://www.bayerische-krebsgesellschaft.de/krebsberatungstellen/wuerzburg/veranstaltungen-wuerzburg/?L=0" %>% 
  read_html() -> krebsVeranstaltungsuebersicht

krebsVeranstaltungsuebersicht %>% 
  html_nodes(".odd") %>% 
  str_match_all("/krebsberatungstellen/veranstaltungen-verweis/.*?\\?") %>% 
  unlist() %>% 
  str_remove("\\?$")-> preKrebsLinksammlungOdd

krebsVeranstaltungsuebersicht %>% 
  html_nodes(".even") %>% 
  str_match_all("/krebsberatungstellen/veranstaltungen-verweis/.*?\\?") %>% 
  unlist() %>% 
  str_remove("\\?$")-> preKrebsLinksammlungEven

rbind(preKrebsLinksammlungOdd, preKrebsLinksammlungEven) %>% 
  paste0("https://www.bayerische-krebsgesellschaft.de",.) -> krebsLinksammlung


krebsDf <- map_df(krebsLinksammlung, getKrebsEvents)

if( !all(is.na(krebsDf$time_start)) ){
  krebsDf$time_start <- times(krebsDf$time_start)
}
if( !all(is.na(krebsDf$time_end)) ){
  krebsDf$time_end <- times(krebsDf$time_end)
}





# Schwulesbisches Zentrum Würzburg ----------------------------------------

"http://www.wufzentrum.de/index.php" %>% 
  read_html() -> urlWUF

# fuer die naechsten 15 Jahre >>> CSS-Tag wird vom Admin monatlich erhöht
seq(1994, 2180) %>% 
  paste0("#c", .) %>% 
  paste(collapse = ' , ') -> monate_div

urlWUF %>% 
  html_nodes(monate_div) %>% 
  html_text(trim = T) -> rawWUF

"https://www.wufzentrum.de/index.php?id=78" %>% 
  read_html() %>% 
  html_nodes("#rightbar .bodytext:nth-child(3)")  %>%
  as.character() %>% 
  strsplit("\">|<br>") %>% 
  unlist() -> wufLocation

wufLocation %>% 
  .[2] -> wufAdresse

wufLocation %>% 
  .[3] %>% 
  gsub("([0-9]{5}) (.*)", "\\1", .) %>% 
  as.numeric()-> wufPLZ

wufLocation %>% 
  .[3] %>% 
  gsub("([0-9]{5}) (.*)", "\\2", .) -> wufOrtsname

wufVeranstalter <- "Schwulesbisches Zentrum Würzburg"

wufLink <- "http://www.wuf-zentrum.de"

preWufVeranstaltrUndAdresse <- paste(wufVeranstalter, wufAdresse, wufPLZ, wufOrtsname, sep = ", ")

wufLatAndLong <- geocode(preWufVeranstaltrUndAdresse)

wufLat <- wufLatAndLong[2]
wufLong <- wufLatAndLong[1]

wufDatumVon = wufDatumBis = wufZeitVon = wufZeitBis = wufTitel = wufBeschreibung = wufEintritt = NA

wufdf <- data.frame(stringsAsFactors = F)

for (i in 1:length(rawWUF)){
  year <- gsub("^[A-z]{3,9} (20.{2}).*", "\\1", rawWUF[i])
  rawWUF[i] %>% 
    gsub("[A-zä]{3,9} 20.{2}", "", .) %>% 
    gsub("(\\n|\\t)", "", .) %>%
    str_split("\\r", simplify = F) %>% 
    unlist() %>% 
    gsub("!^[\\d]", "", .) %>% 
    str_trim("both") %>% 
    grep("^[0-9]{2}\\.[0-9]{2}", value = T, .)  -> rawSplitWufDigits
  
  #rawSplitWufDigits <- rawSplitWuf[rawSplitWuf %in% grep(paste0("\\d", collapse = "|"), rawSplitWuf, value = T)] 
  
  
  for (variable in rawSplitWufDigits) {
    
    variable %>% 
      as.character() -> variable
    
    #print(variable)
    
    variable %>% 
      noquote() %>% 
      str_trim() %>% 
      str_replace_all("\\s", " ") -> variable
    
    #print(variable)
    
    # DATUMRAUM
    if(grepl("^([0-9]{2}\\.[0-9]{2}\\.?)(-[0-9]{2}\\.[0-9]{2}\\.?) .*", variable)){
      
      wufDatumVon <- as.Date(gsub("\\.", "/",paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.?)(-[0-9]{2}\\.[0-9]{2}\\.?) .*", "\\1", variable), year)), "%d/%m/%Y")
      wufDatumBis <- as.Date(gsub("\\.", "/",paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.?)-([0-9]{2}\\.[0-9]{2}\\.?) .*", "\\2", variable), year)), "%d/%m/%Y")
      
      # Datumraum + Zeitraum
      if(grepl("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2}).*", variable)){
        wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
        wufZeitVon <- times(paste0(wufZeitVon, ":00"))
        wufZeitBis <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\2", variable)
        wufZeitBis <- times(paste0(wufZeitBis, ":00"))
        
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? [0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
        
        # Datumraum + Zeitpunkt
      } else if (grepl("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}).*", variable)){
        wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)  
        wufZeitVon <- times(paste0(wufZeitVon, ":00"))
        wufZeitBis <- times(NA)
        
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? [0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
        
        # Datenraum + Zeitpunkt/Zeitraum unbekannt
      } else {
        wufZeitVon <- times(NA)
        wufZeitBis <- times(NA)
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? (.*)", "\\1", variable)
        
      }
      
      wufdf2 <- data.frame(title = wufTitel, url = wufLink, description = wufBeschreibung, lng = wufLong, lat = wufLat, city = wufOrtsname, 
                           street = wufAdresse, zip = wufPLZ, date_start = wufDatumVon, date_end = wufDatumBis, time_start = wufZeitVon, 
                           time_end = wufZeitBis, price = wufEintritt, organizer = wufVeranstalter, stringsAsFactors = F)    
      colnames(wufdf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
      wufdf <- rbind(wufdf, wufdf2)
      
      # DATUMSPUNKT
    } else if (grepl("^([0-9]{2}\\.[0-9]{2}\\.) .*", variable)){
      
      wufDatumVon <- as.Date(gsub("\\.", "/", paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.).*", "\\1", variable), year)), "%d/%m/%Y")
      wufDatumBis <- as.Date(gsub("\\.", "/", paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.).*", "\\1", variable), year)), "%d/%m/%Y")
      
      
      # Datumspunkt + Zeitraum
      if(grepl("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2}).*", variable)){
        wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
        wufZeitVon <- times(paste0(wufZeitVon, ":00"))
        wufZeitBis <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\2", variable)
        wufZeitBis <- times(paste0(wufZeitBis, ":00"))
        
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. [0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
        # Datumpunkt + Zeitpunkt
      } else if(grepl("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}).*", variable)){
        wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
        wufZeitVon <- times(paste0(wufZeitVon, ":00"))
        wufZeitBis <- times(NA)
        
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. [0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
      } else {
        wufZeitVon <- times(NA)
        wufZeitBis <- times(NA)
        
        wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. (.*)", "\\1", variable)
      }
      ###
      wufdf2 <- data.frame(title = wufTitel, url = wufLink, description = wufBeschreibung, lng = wufLong, lat = wufLat, city = wufOrtsname, 
                           street = wufAdresse, zip = wufPLZ, date_start = wufDatumVon, date_end = wufDatumBis, time_start = wufZeitVon, 
                           time_end = wufZeitBis, price = wufEintritt, organizer = wufVeranstalter, stringsAsFactors = F)
      colnames(wufdf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
      wufdf <- rbind(wufdf, wufdf2)
      # NANANA
    } else{
      #print("no")
    }    
    
  } 
  
}





# FutureKids --------------------------------------------------------------

fututoFuturo <- function(url) {
  
  
  futureVeranstalter = futureDatumVon = futureDatumBis = futureStartzeit = futureEndzeit = futureTitel = futureBeschreibung = 
    futureAdresse = futurePLZ = futureOrtsname = futureLat = futureLong = futureLink = futureEintritt = NA
  
  
  futureVeranstalter <- "Futurekids Computerkurse"
  
  futureDf <- data.frame()
  
  futureLink <- url
  
  futureLink %>% 
    read_html() -> futureURL
  
  futureURL %>% 
    html_nodes(".uk-accordion-title") %>% 
    html_text()-> futureKurstitelSammlung
  
  futureURL %>% 
    html_nodes(".uk-accordion-content") -> futureKurse
  
  "http://www.futurekids.wue.de/ueber-uns.html" %>% 
    read_html() %>% 
    html_nodes(".extradiv") %>% 
    grep("970[0-9]{2} W", value = T, .) %>% 
    gsub("<.*?>", "", .) %>% 
    str_split("\\n") %>% 
    unlist() -> futureAnschrift
  
  futureAnschrift %>% 
    grep("([A-zöäü]+str.)|(Strasse)|(Straße)|(strasse)|(straße)", value = T,.) %>% 
    str_trim("both") -> futureAdresse 
  
  futureAnschrift %>% 
    grep("970[0-9]{2} W", value = T, .) %>%
    str_trim("both") %>% 
    #str_match("970[0-9]{2}")
    gsub("^(970[0-9]{2}) ([A-zü]*)", "\\1", .) -> futurePLZ
  
  futureAnschrift %>% 
    grep("970[0-9]{2} W", value = T, .) %>%
    str_trim("both") %>% 
    #str_match("970[0-9]{2}")
    gsub("^(970[0-9]{2}) ([A-zäöü]*)", "\\2", .) -> futureOrtsname
  
  preFutureVeranstaltrUndAdresse <- paste(futureVeranstalter, futureAdresse, futurePLZ, futureOrtsname, sep = ", ")
  
  futureLatAndLong <- geocode(preFutureVeranstaltrUndAdresse)
  
  futureLat <- futureLatAndLong[2]
  futureLong <- futureLatAndLong[1]
  
  
  for (n in 1:length(futureKurse)) {
    
    futureKurse[n] %>%  
      html_nodes(".agetimediv") %>% 
      html_text(trim = T) -> testObFixerTermin 
    
    if( any(grepl("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", testObFixerTermin)) ){
      
      futureTitel <- futureKurstitelSammlung[n] %>% 
        as.character() %>% 
        noquote()
      
      testObFixerTermin %>% 
        str_split("\\n") %>% 
        #gsub("[[:space:]]{2,}", "", .) %>% 
        unlist() %>% 
        str_replace_all("[[:space:]]{2,}","") %>% 
        str_replace_all(" oder$", "") -> futureDatumUndAltersempfehlung
      
      futureDatumUndAltersempfehlung %>% 
        grep("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", value = T,.) -> DatumsListeChar
      
      futureDatumUndAltersempfehlung %>% 
        grep("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", value = T, invert = T, .) -> futureAltersempfehlung
      
      futureKurse[n] %>% 
        html_nodes("p") %>% 
        html_text(trim = T) %>% 
        gsub("  ", "",.) %>% 
        gsub("\\n", "",.) %>% 
        noquote() -> futurePreBeschreibung
      
      futureKurse[n] %>% 
        html_nodes("strong:nth-child(3)") %>% 
        html_text() %>% 
        str_replace_all("\\.- ", "\\.00 ") %>% 
        str_match_all("[0-9]{1,4}\\.[0-9]{2}") %>% 
        unlist() %>% 
        as.numeric() %>% 
        sum() %>% 
        as.character() -> futureEintritt
      
      for (variable in DatumsListeChar) {
        
        variable %>% 
          str_match("[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}") %>% 
          as.Date("%d.%m.%y") -> futureDatumVon
        
        futureDatumBis <- futureDatumVon
        
        variable %>% 
          str_match("[0-9]{2}:[0-9]{2}.{1,4}[0-9]{2}:[0-9]{2}") %>%
          gsub("([0-9]{2}:[0-9]{2}) . ({1,4}[0-9]{2}:[0-9]{2})", "\\1",.) %>% 
          gsub(" ", "", .) %>% 
          as.character()-> futureStartzeit
        
        futureStartzeit <- times(paste0(futureStartzeit, ":00"))
        
        variable %>% 
          str_match("[0-9]{2}:[0-9]{2}.{1,4}[0-9]{2}:[0-9]{2}") %>% 
          gsub("([0-9]{2}:[0-9]{2}) . ({1,4}[0-9]{2}:[0-9]{2})", "\\2",.) %>% 
          gsub(" ", "", .) %>% 
          as.character()-> futureEndzeit
        
        futureEndzeit <- times(paste0(futureEndzeit, ":00"))
        
        futureBeschreibung <- paste(futureAltersempfehlung[1], futurePreBeschreibung, "Weiterfolgende Termine:", sep = ";  ") %>% as.character()
        futureBeschreibung <- paste(futureBeschreibung, variable, sep = " ") %>% as.character()
        
        futureDf2 <- data.frame(title = futureTitel, url = futureLink, description = futureBeschreibung, lng = futureLong, 
                                lat = futureLat, city = futureOrtsname, street = futureAdresse, zip = futurePLZ, 
                                date_start = futureDatumVon, date_end = futureDatumBis, time_start = futureStartzeit, 
                                time_end = futureEndzeit, price = futureEintritt, organizer = futureVeranstalter, 
                                stringsAsFactors = F)
        
        futureDf <- rbind(futureDf, futureDf2)
        
      }
      
      
    } 
    
  }
  futureDf
}

futureAlleLinks <- c("http://www.futurekids.wue.de/kids.html", "http://www.futurekids.wue.de/erwachsene.html")


futureDfFINAL <- map_df(futureAlleLinks, fututoFuturo)

colnames(futureDfFINAL) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")

if( !all(is.na(futureDfFINAL$time_start)) ){
  futureDfFINAL$time_start <- times(futureDfFINAL$time_start)
}
if( !all(is.na(futureDfFINAL$time_end)) ){
  futureDfFINAL$time_end <- times(futureDfFINAL$time_end)
}





# Fischerzunft ------------------------------------------------------------

fischerVeranstalter = fischerDatumVon = fischerDatumBis = fischerStartzeit = fischerEndzeit = fischerTitel = fischerBeschreibung = 
  fischerAdresse = fischerPLZ = fischerOrtsname = fischerLat = fischerLong = fischerLink = fischerEintritt = NA

fischerVeranstalter <- "Fischerzunft Würzburg"
fischerLink <- "www.fischerzunft-wuerzburg.de"
fischerEintritt <- as.character(0)

"http://www.fischerzunft-wuerzburg.de/Zunftsaal.htm" %>%
  read_html() -> fischerUrl

fischerUrl %>% 
  html_nodes(".inh-ueb") %>% 
  html_text() %>% 
  as.character()-> fischerTitel

fischerUrl %>% 
  html_nodes(".inh-normal:nth-child(7)") %>% 
  html_text(trim = T) %>% 
  str_split("\r\n\t\t\t") %>% 
  unlist() -> fischerLocation

fischerLocation[1] %>% 
  str_trim() -> fischerAdresse

# PLZ
fischerLocation[2] %>% 
  gsub(".*(97\\d{3}).*", "\\1" ,.) -> fischerPLZ

# Ortsname
fischerLocation[2] %>% 
  gsub(".*97\\d{3}\\s(.*)", "\\1", .) -> fischerOrtsname

preFischerVeranstaltrUndAdresse <- paste(fischerVeranstalter, fischerAdresse, fischerPLZ, fischerOrtsname, sep = ", ")
fischerLatAndLong <- geocode(preFischerVeranstaltrUndAdresse)
fischerLat <- fischerLatAndLong[2]
fischerLong <- fischerLatAndLong[1]


fischerUrl %>% 
  html_nodes(".inh-normal:nth-child(5)") %>% 
  html_text(trim = T) -> rawFischer

fischerUrl %>%
  html_nodes("br+ .inh-normal") %>% 
  html_text(trim = T) %>% 
  str_trim() %>% 
  gsub("[[:space:]]{2,}", "",.) %>% 
  noquote() -> fischerBeschreibung

str_split(rawFischer, "\r\n\t\t\t") %>% 
  unlist() -> rawFischerDatumraumZeitraum

rawFischerDatumraumZeitraum %>% 
  .[1] -> rawFischerZeitraum.1

#fischerStartZeit <- gsub(".* von ([0-9]{2}:[0-9]{2}) bis [0-9]{2}:[0-9]{2} .*", "\\1", rawFischerZeitraum.1)
#fischerEndZeit <- gsub(".* von [0-9]{2}:[0-9]{2} bis ([0-9]{2}:[0-9]{2}) .*", "\\1", rawFischerZeitraum.1)
#fischerZeitraum <- paste(fischerStartZeit, fischerEndZeit, sep = "-")

rawFischerZeitraum.1 %>% 
  gsub(" bis ", "-", .) %>% 
  gsub(".* ([0-9]{1,2}:[0-9]{2})-([0-9]{2}:[0-9]{1,2}) .*", "\\1", .) -> fischerStartzeit

fischerStartzeit <- times(paste0(fischerStartzeit, ":00"))

rawFischerZeitraum.1 %>% 
  gsub(" bis ", "-", .) %>% 
  gsub(".* ([0-9]{1,2}:[0-9]{2})-([0-9]{2}:[0-9]{1,2}) .*", "\\2", .) -> fischerEndzeit

fischerEndzeit <- times(paste0(fischerEndzeit, ":00"))

rawFischerDatumraumZeitraum %>% 
  #gsub("[[:space:]]|\\W", "", .) %>% 
  str_trim() %>% 
  grep("^[0-9]",., value = T) -> fischerDatumspunkte

fischerdf <- data.frame()


for (variable in fischerDatumspunkte) {
  
  fischerdf2 <- data.frame()
  
  fischerDatumVon <- as.Date(gsub("\\.", "/", gsub("^([0-9]{2}\\.[0-9]{2}\\.?)(-[0-9]{2}\\.[0-9]{2}\\.?) .*", "\\1", variable)), "%d/%m/%Y")
  fischerDatumBis <- fischerDatumVon
  fischerdf2 <- data.frame(title = fischerTitel, url = fischerLink, description = fischerBeschreibung, lng = fischerLong, 
                           lat = fischerLat, city = fischerOrtsname, street = fischerAdresse, zip = fischerPLZ, 
                           date_start = fischerDatumVon, date_end = fischerDatumBis, time_start = fischerStartzeit, 
                           time_end = fischerEndzeit, price = fischerEintritt, organizer = fischerVeranstalter, 
                           stringsAsFactors = F)
  fischerdf <- rbind(fischerdf, fischerdf2)
}


colnames(fischerdf) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")

if( !all(is.na(fischerdf$time_start)) ){
  fischerdf$time_start <- times(fischerdf$time_start)
}
if( !all(is.na(fischerdf$time_end)) ){
  fischerdf$time_end <- times(fischerdf$time_end)
}





# MERGE -------------------------------------------------------------------

#rm(list=setdiff(ls(), c("krebsDf", "wufdf", "hofkellerDf", "lomaDf", "fischerdf", "futureDfFINAL", "finalFacebookDf")))

veranstaltungen3 <- rbind(wufdf, lomaDf, krebsDf, hofkellerDf, futureDfFINAL, fischerdf)

veranstaltungen3$zip <- as.numeric(veranstaltungen3$zip)

#"krebsDf", "wufdf", "hofkellerDf", "lomaDf", "fischerdf", "futureDfFINAL"
