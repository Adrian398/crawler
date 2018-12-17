# test
library(plyr)
library(rvest)
library(tidyverse)
library(purrr)
library(gsubfn)
library(stringr)
library(chron)

url <- "http://p27687.ngcobalt117.manitu.net/jo2018/index.php/veranstaltungen"

# ziehe Daten in Tabellenform und füge sie in eine Liste ein
url %>%
  read_html() %>%
  html_nodes("#g-mainbar a") %>%
  html_attr('href') -> links

url %>%
  read_html() %>%
  html_nodes(".sprocket-strips-s-title a") %>%
  html_text(trim = T)  -> veranstaltungen

# füge http Zusatz hinzu
links <- sapply(links, function(x) paste0("http://p27687.ngcobalt117.manitu.net",x))

laenge <- length(veranstaltungen)
# links kommen doppelt, wandele sie in Matrix um und lösche dann eine der Spalten
links <- matrix(c(links), nrow= (length(links)/2), ncol=2, byrow = TRUE)
links <- links[,1]

# die letzte veranstaltung scheint immer die sommerpause zu sein
links <- data.frame(links)
veranstaltungen <- data.frame(veranstaltungen)

bhof_df <- data.frame( veranstaltungen, links)

# lösche letzte und erste Zeile
bhof_df <- bhof_df[c(-1,-laenge), ]

# Funktion zur prüfung des letzten Characters
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ist letzter character KEIN punkt? Falls true, dann füg einen hinzu
point_miss <- function(string){
  
  if(substrRight(string, 1)!= "."){
    paste0(string, ".")
  }
  else string = string
}

colon_miss <- function(string){
  
  if(length(string) == 0){
    
  }  
  
  else if(grepl(":",string)) string = string 
  
  else paste0(string, ":00")
  
}

# ziehe Titel aus Titelzeile. .* bedeutet: alles nach \\.\\s wird gezogen
titles <- sapply(bhof_df$veranstaltungen, function(x) sub('.*\\.\\s|.*\\.\\d{1,2}\\s', '',x))

# ziehe Datum aus Titelzeile
dati <- strapply(as.character(bhof_df$veranstaltungen), "\\d{1,2}\\.\\d{1,2}\\.?")
dati <- as.matrix(as.data.frame(dati))

# ziehe Start und End-Datum und untersuche auf Fehlenden Punkt am Ende
dati_start <- sapply(dati[1,], point_miss)
dati_end <- sapply(dati[2,], point_miss)

# Füge Jahreszahl dazu
dati_start <- sapply(dati_start, function(x) paste0(x, "2018"))
dati_end <- sapply(dati_end,function(x) paste0(x, "2018") )

# Titel und dati ins Dataframe
bhof_df$title <- titles
bhof_df$start_datum <- dati_start
bhof_df$end_datum <- dati_end

bhof_df$links <- as.character(bhof_df$links)

zusatzinfos <- function(url){
 
url %>%
  read_html() %>%
  html_nodes("#g-mainbar p") -> inhalte 
#%>%
#  html_text(trim = T)  -> inhalte

# verhindere Zerstückelung
inhalte <- paste(inhalte, collapse = " ")

#uhrzeit <- regexpr("\d{1,2}\\:\d{1,2}", inhalte)
#uhrzeit <- regexpr("[0-9]", inhalte)
#regmatches(inhalte, uhrzeit)

# Zeit nach Einlass ist Einlasszeit. Nimmt den Ausdruck\\d{1,2}:\\d{1,2} nach 'Einlass:'
#einlass <- str_extract(inhalte[2], '(?<=Einlass:\\s)\\d{1,2}:\\d{1,2}|(?<=Einlass:\\s)\\d{1,2}')
beginn <- str_extract(inhalte, '(?<=Beginn:\\s)\\d{1,2}:\\d{1,2}(?=\\s)|(?<=Beginn:\\s)\\d{1,2}(?=\\s)')

# zieht 15 Zeichen nach Zeitraum oder Zeit, danach alle Zeiten separat und umwandeln in df
zeitraum <- str_extract(inhalte, '(?<=Zeitraum:\\s).{15}|(?<=Zeit:\\s).{15}')

# falls es das 00:00 Format hat, dann hol mir die Uhrzeiten, falls nicht, hol mir die ganzen Uhrzeiten

timespan <- function(infos){
  if(!is.na(infos)){
    zeiten <- ifelse(grepl("\\d{1,2}\\:\\d{1,2}",zeitraum),strapply(zeitraum,"\\d{1,2}\\:\\d{1,2}"),strapply(zeitraum,"\\d{1,2}"))
    zeiten <- sapply(zeiten, colon_miss)
    zeiten <- paste(unlist(zeiten), collapse=" - ")
  }
  else zeiten <- NA
  return(zeiten)
}

zeiten <- timespan(zeitraum)
#zeiten <- as.data.frame(zeiten)

preis <- str_extract(inhalte, '(?<=Eintritt:\\s).+?(?=<)')

url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text(trim = TRUE) -> info

info <- paste(info, collapse = " ")
info <- paste0(substr(info, start = 1, stop = 300), "...")

return(list(beginn = beginn, zeiten=zeiten, preis=preis, info=info))
}

zusatz <- sapply(bhof_df$links, zusatzinfos)

tabelle <- matrix(c(zusatz), nrow= (length(zusatz)/4), ncol=4, byrow = TRUE)
zusatz_df <- as.data.frame(tabelle)
colnames(zusatz_df) <- c("beginn", "zeitraum", "preis", "info" )

bhof_df$ID <- seq(1, length(bhof_df$links), 1)
zusatz_df$ID <- seq(1, length(zusatz_df$preis), 1)

total_df <- merge(bhof_df, zusatz_df,by="ID")
total_df$beginn[is.na(total_df$beginn)] <- ""
total_df$zeitraum[is.na(total_df$zeitraum)] <- ""

total_df$zeit <- paste0(total_df$beginn, total_df$zeitraum)
total_df$zeit <- substr(total_df$zeit, start = 1, stop = 5)
total_df$zeit_ende <- substr(total_df$zeitraum, start = 9, stop = 13)

total_df$organizer <- rep("B-Hof", length(total_df$title))
total_df$city <- "Wuerzburg"
total_df$street <- "Hofstr. 16"
total_df$zip <- 97070
total_df$lat <- 49.79309
total_df$lng <- 9.93549

keep <- c("title",  "links",  "info" , "lng", "lat", "city", "street", "zip","start_datum", "end_datum", "zeit","zeit_ende","preis", "organizer")
total_df <- total_df[ , keep, drop = FALSE]

colnames(total_df) <- c("title",  "url", "description", "lng", "lat", "city", "street", "zip","date_start", "date_end", "time_start","time_end","price", "organizer")

total_df$time_start <- times(paste0(total_df$time_start,":00"))
total_df$time_end <- times(paste0(total_df$time_end,":00"))
total_df$date_start <- as.Date(total_df$date_start, format="%d.%m.%Y")
total_df$date_end <- as.Date(total_df$date_end, format="%d.%m.%Y")

rownames(total_df) <- NULL
# finales df heißt total_df