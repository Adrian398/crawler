
# Facebook ----------------------------------------------------------------


#library(rvest)
#library(tidyverse)
# devtools::install_github("dkahle/ggmap")
#library(ggmap)
#library(chron)

# Funktioniert nur, wenn eingelesene URLs auf deutsch
# kein Zugriff auf Beschreibung der Veranstaltung auf Facebooks mobiler Version, daher Beschreibung = NA

#register_google(key = "AIzaSyDOjGt9LQMihlMnknjDY49Qgwe2m_vqr9g")

getFacebookEvents <- function(url) {
  
  facebookDatumVon = facebookDatumBis = facebookStartzeit = facebookEndzeit = facebookTitel = facebookBeschreibung = 
    facebookAdresse = facebookPLZ = facebookOrtsname = facebookLat = facebookLong = facebookCITYLat = facebookCITYLong = 
    facebookLink = facebookEintritt = preFacebookAdresse = facebookDatumListe = NA
  
  
  
  url %>%
    str_replace("d\\.", "") -> facebookLink

  url %>%
    read_html() -> facebookRaw
  
  
  xml_find_all(facebookRaw, './/div') %>%
    # html_nodes(".de") %>%                               
    html_text()  %>%
    grep("[0-9]{5} ", value = T, .) %>%
    tail(n = 1) -> preFacebookAdresse
  
  
  if (identical(preFacebookAdresse, character(0))) {
    preFacebookAdresse <- NA
  }
  
  
  xml_find_all(facebookRaw, './/title') %>%
    html_text() -> facebookTitel
  
  
  xml_find_all(facebookRaw, './/div') -> facebookZeitoderZeitraum
  
  
  aktuellerMonat <- as.numeric(format(Sys.Date(), "%m"))
  aktuellesJahr <- as.numeric(format(Sys.Date(), "%Y"))
  
  
  # Zeitpunkt oder Zeitraum?
  
  # Datumraum
  if( any(grepl("(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )([0-9]{1,2}\\. [A-zä]{3,9}) ([0-9]{2}:[0-9]{2} - [0-9]{2}:[0-9]{2}.*)", facebookZeitoderZeitraum ))){
    
    facebookZeitoderZeitraum %>%
      html_attr("title") %>% 
      grep("^(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )", value = T,.) %>% 
      gsub("^(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )", "", .) %>% 
      gsub(" UTC.*", "", .) -> facebookDatumUndZeit
    
    print(facebookDatumUndZeit)
    print(1)
    
    facebookDatumUndZeit %>% 
      gsub("([0-9]{1,2}\\. [A-zä]{3,9}) ([0-9]{2}:[0-9]{2} - [0-9]{2}:[0-9]{2})", "\\1", .) -> facebookDatumChar
    
    #############
    #############
    
    
    facebookDatumChar %>%
      gsub("\\.", "", .) %>%
      paste(aktuellesJahr, sep = " ") %>%
      parse_date("%d %B %Y", locale = locale("de")) -> facebookDatumVon
    
    #
    if (aktuellerMonat > as.numeric(format(facebookDatumVon, "%m"))) {
      facebookDatumChar %>%
        gsub("\\.", "", .) %>%
        paste(aktuellesJahr + 1, sep = " ") %>%
        parse_date("%d %B %Y", locale = locale("de")) -> facebookDatumVon
    }
    #
    
    facebookDatumListe <- facebookDatumVon
    
    facebookDatumBis <- facebookDatumVon

    #############
    #############
    
    facebookDatumUndZeit %>% 
      gsub("([0-9]{1,2}\\. [A-zä]{3,9}) ([0-9]{2}:[0-9]{2}) - ([0-9]{2}:[0-9]{2})", "\\2", .) %>% 
      gsub(" ", "", .)-> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
    
    facebookDatumUndZeit %>% 
      gsub("([0-9]{1,2}\\. [A-zä]{3,9}) ([0-9]{2}:[0-9]{2}) - ([0-9]{2}:[0-9]{2})", "\\3", .) %>% 
      gsub(" ", "", .)-> facebookEndzeit
    
    facebookEndzeit <- times(paste0(facebookEndzeit, ":00"))
    
    
    # Zeitpunkt  
  } else if( any(grepl("(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )[0-9]{1,2}\\. [A-zä]{3,9} um [0-9]{1,2}:[0-9]{2}.*", facebookZeitoderZeitraum))) {
    
    facebookZeitoderZeitraum %>% 
      html_attr("title") %>% 
      grep("^(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )", value = T,.) %>% 
      gsub("^(Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )", "", .) %>% 
      gsub(" UTC.*", "", .) -> facebookDatumUndZeit
    
    print(facebookDatumUndZeit)
    print(2)
    
    facebookDatumUndZeit %>% 
      gsub("^([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\1", .) -> facebookDatumChar
    
    facebookDatumChar %>%
      gsub("\\.", "", .) %>%
      paste(aktuellesJahr, sep = " ") %>%
      parse_date("%d %B %Y", locale = locale("de")) -> facebookDatumVon
    
    #
    if (aktuellerMonat > as.numeric(format(facebookDatumVon, "%m"))) {
      facebookDatumChar %>%
        gsub("\\.", "", .) %>%
        paste(aktuellesJahr + 1, sep = " ") %>%
        parse_date("%d %B %Y", locale = locale("de")) -> facebookDatumVon
    }
    
    facebookDatumListe <- facebookDatumVon
    
    facebookDatumBis <- facebookDatumVon
    #
    
    facebookDatumUndZeit %>% 
      gsub("^([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\2", .) %>% 
      gsub(" ", "", .)-> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
    facebookEndzeit <- times(NA)
    
    # Zeitraum
  } else if ( any(grepl("Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", facebookZeitoderZeitraum)) ) {
    
    facebookZeitoderZeitraum %>%
      html_text() %>% 
      grep("Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", value = T, .) %>% 
      tail(n=1)  -> facebookDatumUndZeit
    
    print(facebookDatumUndZeit)
    print(3)
    
    facebookDatumUndZeit %>% 
      gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\1", .) %>% 
      gsub("\\.", "",.) %>% 
      paste(aktuellesJahr, sep = " ") %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumVon
    
    #
    if(  aktuellerMonat > as.numeric(format(facebookDatumVon, "%m")) ){
      facebookDatumUndZeit %>% 
        gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\1", .) %>% 
        gsub("\\.", "",.) %>% 
        paste(aktuellesJahr+1, sep = " ") %>% 
        parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumVon
    }
    #
    
    facebookDatumListe <- facebookDatumVon
    
    facebookDatumUndZeit %>% 
      gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\3", .) %>% 
      gsub("\\.", "",.) %>% 
      paste(aktuellesJahr, sep = " ") %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumBis
    
    #
    if(  aktuellerMonat > as.numeric(format(facebookDatumBis, "%m")) ){
      facebookDatumUndZeit %>% 
        gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\3", .) %>% 
        gsub("\\.", "",.) %>% 
        paste(aktuellesJahr+1, sep = " ") %>% 
        parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumBis
    }
    #
    
    
    facebookDatumUndZeit %>% 
       gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\2", .) -> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
     
     facebookDatumUndZeit %>% 
       gsub("^Von ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{2}:[0-9]{2}).*", "\\4", .) -> facebookEndzeit
    
     facebookEndzeit <- times(paste0(facebookEndzeit, ":00"))
     
     
  } else if ( any(grepl("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", facebookZeitoderZeitraum)) ) {
    
    facebookZeitoderZeitraum %>%
      html_text() %>% 
      grep("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", value = T, .) %>% 
      tail(n=1)  -> facebookDatumUndZeit
    
    print(facebookDatumUndZeit)
    print(4)
    
    facebookDatumUndZeit %>% 
      gsub("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", "\\1", .) %>% 
      gsub("\\.", "",.) %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumVon
    
    facebookDatumListe <- facebookDatumVon
    
    facebookDatumUndZeit %>% 
      gsub("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", "\\3", .) %>% 
      gsub("\\.", "",.) %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumBis
    

    facebookDatumUndZeit %>% 
      gsub("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", "\\2", .) -> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
    
    
    facebookDatumUndZeit %>% 
      gsub("Von ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}) bis ([0-9]{1,2}\\. [A-zä]{3,9} 20[0-9]{2}) um ([0-9]{2}:[0-9]{2}).*", "\\4", .) -> facebookEndzeit
    
    facebookEndzeit <- times(paste0(facebookEndzeit, ":00"))
    
    
  } else if( any(grepl("Jeden (Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )bis [0-9]{2}\\. [A-zä]{3,9} .*", facebookZeitoderZeitraum )) ) {
    # facebookZeitoderZeitraum %>% 
    #   html_text() %>% 
    #   str_extract_all("(Mo, |Di, |Mi, |Do, |Fr, |Sa, |So, )[A-zä]{3,9} [0-9]{1,2}") %>% 
    #   unique() %>% 
    #   .[[1]] %>% 
    #   str_replace_all("(Mo, |Di, |Mi, |Do, |Fr, |Sa, |So, )", "") %>% 
    #   paste(aktuellesJahr, sep = " ") 
    
    facebookZeitoderZeitraum %>% 
      html_text() %>% 
      str_extract("Jeden (Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )bis ([0-9]{2}\\. [A-zä]{3,9}) .*") %>% 
      unique() %>% 
      tail(n = 1) -> preFacebookDatum
    
    preFacebookDatum %>% 
      gsub("Jeden (Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )bis ([0-9]{2}\\. [A-zä]{3,9}) .*", "\\2", .) %>% 
      gsub("\\.", "",.) %>% 
      paste(aktuellesJahr, sep = " ") %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumEnde
    
    if(  aktuellerMonat > as.numeric(format(facebookDatumEnde, "%m")) ){
      preFacebookDatum %>% 
        gsub("Jeden (Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )bis ([0-9]{2}\\. [A-zä]{3,9}) .*", "\\2", .) %>% 
        gsub("\\.", "",.) %>% 
        paste(aktuellesJahr, sep = " ") %>% 
        parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumEnde
    }
    
    print(facebookDatumEnde)
    print(5)
    
    preFacebookDatum %>% 
      gsub("Jeden (Montag, |Dienstag, |Mittwoch, |Donnerstag, |Freitag, |Samstag, |Sonntag, )bis ([0-9]{2}\\. [A-zä]{3,9}) .*", "\\1", .) %>% 
      str_replace(", ", "") -> facebookWochentag
    
    prefacebookDatumListe <- seq(Sys.Date(), facebookDatumEnde, by="days")
    
    facebookDatumListe <- prefacebookDatumListe[weekdays(prefacebookDatumListe) == facebookWochentag]
    
    facebookZeitoderZeitraum %>% 
      html_text() %>% 
      str_extract("[0-9]{2}:[0-9]{2} – [0-9]{2}:[0-9]{2}") %>% 
      unique() %>% 
      .[1] -> facebookZeit
    
    facebookZeit %>% 
      gsub("([0-9]{2}:[0-9]{2}) – ([0-9]{2}:[0-9]{2})", "\\1", .) -> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
    
    
    facebookZeit %>% 
      gsub("([0-9]{2}:[0-9]{2}) – ([0-9]{2}:[0-9]{2})", "\\2", .) -> facebookEndzeit
    
    facebookEndzeit <- times(paste0(facebookEndzeit, ":00"))
    
    
  } else if ( any(grepl("[0-9]{1,2}\\. [A-zä]{3,9} um [0-9]{1,2}:[0-9]{2} (–|-) [0-9]{1,2}:[0-9]{2}", facebookZeitoderZeitraum )) ){
    facebookZeitoderZeitraum %>% 
      html_text() %>% 
      str_extract_all("[0-9]{1,2}\\. [A-zä]{3,9} um [0-9]{1,2}:[0-9]{2} (–|-) [0-9]{1,2}:[0-9]{2}") %>% 
      unique() %>% 
      .[1] -> facebookDatumundZeitraum
    
    print(facebookDatumundZeitraum)
    print(6)
    
    facebookDatumundZeitraum %>% 
      gsub("([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{1,2}:[0-9]{2} (–|-) [0-9]{1,2}:[0-9]{2})", "\\1", .) %>% 
      gsub("\\.", "",.) %>% 
      paste(aktuellesJahr, sep = " ") %>% 
      parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumVon
    
    if( aktuellerMonat > as.numeric(format(facebookDatumVon, "%m")) ){
      facebookDatumundZeitraum %>% 
        gsub("([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{1,2}:[0-9]{2} (–|-) [0-9]{1,2}:[0-9]{2})", "\\1", .) %>% 
        gsub("\\.", "",.) %>% 
        paste(aktuellesJahr, sep = " ") %>% 
        parse_date("%d %B %Y",locale=locale("de")) -> facebookDatumVon
    }
    
    facebookDatumBis <- facebookDatumVon
    
    facebookDatumListe <- facebookDatumVon
    
    facebookDatumundZeitraum %>% 
      gsub("([0-9]{1,2}\\. [A-zä]{3,9}) um ([0-9]{1,2}:[0-9]{2} (–|-) [0-9]{1,2}:[0-9]{2})", "\\2", .) %>% 
      str_replace_all("–", "-") %>% 
      str_remove_all("[[:space:]]") -> facebookZeit
    
    facebookZeit %>% 
      gsub("([0-9]{2}:[0-9]{2})-([0-9]{2}:[0-9]{2})", "\\1", .) -> facebookStartzeit
    
    facebookStartzeit <- times(paste0(facebookStartzeit, ":00"))
    
    facebookZeit %>% 
      gsub("([0-9]{2}:[0-9]{2})-([0-9]{2}:[0-9]{2})", "\\2", .) -> facebookEndzeit
    
    facebookEndzeit <- times(paste0(facebookEndzeit, ":00"))
    
  }
  
  
  preFacebookAdresse %>%
    str_split(", ") %>%
    unlist() %>%
    .[1] -> facebookAdresse
  
  
  preFacebookAdresse %>%
    str_split(", ") %>%
    unlist() %>%
    .[2] %>%
    gsub("([0-9]{5}) (.*)", "\\1", .) -> facebookPLZ
  
  preFacebookAdresse %>%
    str_split(", ") %>%
    unlist() %>%
    .[2] %>%
    gsub("([0-9]{5}) (.*)", "\\2", .) -> facebookOrtsname
  
  # hole lat/long des Veranstaltungsorts und der Stadt der Veranstaltung
  if(!is.na(preFacebookAdresse)){
    
    counter <- 0
    
    while((is.na(facebookLat) | is.na(facebookLong)) & counter < 8){
      
      preFacebookVeranstalterUndAdresse <- paste(facebookVeranstalter, preFacebookAdresse, sep = ", ")
      
      facebookLatAndLong <- geocode(preFacebookVeranstalterUndAdresse)
      
      facebookLat <- facebookLatAndLong[2]
      facebookLong <- facebookLatAndLong[1]
      
      # facebookLat <- geocode(preFacebookVeranstalterUndAdresse)[2]
      # facebookLong <- geocode(preFacebookVeranstalterUndAdresse)[1]
      counter = counter + 1
      
      if(is.na(facebookLat) | is.na(facebookLong)){
        Sys.sleep(1) 
      }
    }
    
    while((is.na(facebookCITYLat) | is.na(facebookCITYLong)) & counter < 8){
      facebookCITYLatAndLong <- geocode(paste(facebookPLZ, facebookOrtsname, sep = ' '))
      
      facebookCITYLat <- facebookCITYLatAndLong[2]
      facebookCITYLong <- facebookCITYLatAndLong[1]
      
      
      # facebookCITYLat <- geocode(paste(facebookPLZ, facebookOrtsname, sep = ' '))[2]
      # facebookCITYLong <- geocode(paste(facebookPLZ, facebookOrtsname, sep = ' '))[1]
      counter = counter + 1
      
      if (is.na(facebookCITYLat) | is.na(facebookCITYLong)) {
        Sys.sleep(1) 
      }
    }
    
  }else {
    facebookLat <- NA
    facebookLong <- NA
  }
  
  # vergleiche ob Veranstaltungsort die gleiche lat/long hat der Stadt
  if(!( is.na(facebookLat) & is.na(facebookLong) & is.na(facebookCITYLat) & is.na(facebookCITYLong) )){
    
    if( (gsub("(^-?\\d{1,3})\\..*", "\\1", facebookLat) != gsub("(^-?\\d{1,3})\\..*", "\\1", facebookCITYLat)) | (gsub("(^-?\\d{1,3})\\..*", "\\1", facebookLong) != gsub("(^-?\\d{1,3})\\..*", "\\1", facebookCITYLong))){
      facebookLat <- 0
      facebookLong <- 0
    }
  }
  
  facebookEintritt <- NA
  facebookBeschreibung <- NA
  facebookdf <- data.frame()
  facebookdf2 <- data.frame()
  
  
  # überspringe die Veranstaltung, falls NULLs enthalten
  if(!(identical(preFacebookAdresse, character(0)) | identical(facebookVeranstalter, character(0)) | identical(facebookDatumVon, character(0)) | 
       identical(facebookDatumBis, character(0)) | identical(facebookStartzeit, character(0)) | identical(facebookEndzeit, character(0)) |
       identical(facebookTitel, character(0)) | identical(facebookBeschreibung, character(0)) | identical(facebookLink, character(0)) |
       identical(facebookAdresse, character(0)) | identical(facebookPLZ, character(0)) | identical(facebookOrtsname, character(0)) | 
       identical(facebookLat, character(0)) | identical(facebookLong, character(0)) | identical(facebookEintritt, character(0)))) {
   
    for(n in 1:length(facebookDatumListe)){
      
      if(is.na(facebookDatumBis)){
        facebookDatumBis <- facebookDatumListe[n]
      }
      
      facebookdf2 <- data.frame(facebookTitel, facebookLink, facebookBeschreibung, facebookLong, facebookLat, facebookOrtsname, facebookAdresse,
                                facebookPLZ, facebookDatumListe[n], facebookDatumBis, facebookStartzeit, facebookEndzeit, facebookEintritt, 
                                facebookVeranstalter, stringsAsFactors = F)
      facebookdf <- rbind(facebookdf, facebookdf2)
      #print(facebookdf)
    }
    colnames(facebookdf) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
    
  } else {
    print("error")
  }
  # Ausgabe
  geocodeQueryCheck()
  facebookdf
}


facebookAlleLinks <- c("https://mbasic.facebook.com/standard.wuerzburg/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fstandard.wuerzburg%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/daslabyrinth/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fdaslabyrinth%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/ClubL/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2FClubL%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/club.ludwig/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fclub.ludwig%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/BuergerspitalWeingut/events/?mt_nav=1&__xts__%5B0%5D=33.%7B%22logging_data%22%3A%7B%22event_type%22%3A%22tapped_all_events%22%2C%22impression_info%22%3A%22eyJmIjp7InBhZ2VfaWQiOiIxMjU2Mjk0ODc0NzcxMTIiLCJpdGVtX2NvdW50IjoiMyJ9fQ%22%2C%22surface%22%3A%22mobile_page_home%22%2C%22interacted_story_type%22%3A%22402279559961068%22%2C%22session_id%22%3A%225abfd4a9e31e5168d201183cddd9704e%22%7D%7D",
                       "https://mbasic.facebook.com/BerghainPanoramaBarOfficial/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2FBerghainPanoramaBarOfficial%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/SchlosskellerDarmstadt/events/?mt_nav=1&__xts__%5B0%5D=33.%7B%22logging_data%22%3A%7B%22event_type%22%3A%22tapped_all_events%22%2C%22impression_info%22%3A%22eyJmIjp7InBhZ2VfaWQiOiIyNjk1Njg1NDkwNTgiLCJpdGVtX2NvdW50IjoiMyJ9fQ%22%2C%22surface%22%3A%22mobile_page_home%22%2C%22interacted_story_type%22%3A%22402279559961068%22%2C%22session_id%22%3A%2224960675af6dc579e8786347310a06cd%22%7D%7D",
                       "https://mbasic.facebook.com/kurtundkomisch/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fkurtundkomisch%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/ODEON.Lounge.Wuerzburg/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2FODEON.Lounge.Wuerzburg%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/silbergoldclub/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fsilbergoldclub%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/airport.wue/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fairport.wue%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/TanzhausWest/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2FTanzhausWest%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/lomabarwuerzburg/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Flomabarwuerzburg%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/bombewue/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fbombewue%2Fevents%2F%3Fref%3Dpage_internal&_rdr", 
                       "https://mbasic.facebook.com/das.boot.wuerzburg/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fdas.boot.wuerzburg%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/kellerperle.fanpage/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fkellerperle.fanpage%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/posthalle/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fposthalle%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/mszufriedenheit/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fmszufriedenheit%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/waldschenkedornheim/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fwaldschenkedornheim%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/discotheklaviva/events/?ref=page_internal&refsrc=https%3A%2F%2Fd.facebook.com%2Fpg%2Fdiscotheklaviva%2Fevents%2F%3Fref%3Dpage_internal&_rdr",
                       "https://mbasic.facebook.com/stadtcafejenseits/events/?ref=page_internal&refsrc=https%3A%2F%2Fmbasic.facebook.com%2Fpg%2Fstadtcafejenseits%2Fevents%2F%3Fref%3Dpage_internal&_rdr"
)


finalFacebookDf <- data.frame()

# Hier zu scrapende Links auswählen 
# [1] Standard  [2] Labyrinth  [3] L  [4] Ludwig  [5] Bürgerspital  [6] Berghain  [7] Schlosskeller  [8] Kurt  [9] Odeon  [10] Silbergold  
# [11] Airport  [12] THW  [13] Loma  [14] Bombe  [15] Boot  [16] Kellerperle  [17] Posthalle  [18] MSZ  [19] Dornheim  [20] LaViva  [21] Jenseits
for (z in c(1:2)) {

facebookAlleLinks[z] %>%
  read_html() -> AllEventsRaw

AllEventsRaw %>%
  xml_find_all('.//title') %>%
  html_text() %>%
  str_split(" - ") %>%
  unlist() %>%
  .[1] -> facebookVeranstalter

# getAllLinks
# https://mbasic.facebook.com......
xml_find_all(AllEventsRaw, './/a') %>%
  html_attr("href") %>%
  grep("/events/[0-9]", value = T, .) -> facebookAllEventsCut

if(identical(facebookAllEventsCut, character(0))) {
  print("Keine zuknünftigen Veranstaltungen gelistet!")
  
} else {
  facebookAllEvents <- paste0("https://d.facebook.com", facebookAllEventsCut)
  
  ### Gogogogo! ###
  preFinalFacebookDf <- map_df(facebookAllEvents , getFacebookEvents)
}

finalFacebookDf <- rbind(finalFacebookDf, preFinalFacebookDf)

finalFacebookDf$time_start <- times(finalFacebookDf$time_start)
finalFacebookDf$time_end <- times(finalFacebookDf$time_end)

}
  
# DEN DF NACH DEM FINALEN MERGEN LOESCHEN, SONST WIRD ER EWIG LANG

#finalFacebookDf