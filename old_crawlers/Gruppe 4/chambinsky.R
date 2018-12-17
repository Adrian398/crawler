library(plyr)
library(rvest)
library(tidyverse)
library(purrr)
library(gsubfn)
library(stringr)
library(chron)

url <- "https://chambinzky.com/spielplan2"

# ziehe Tag und Uhrzeit
url %>%
  read_html() %>%
  html_nodes(".uk-h2") %>%
  html_text(trim = T) -> title

# ziehe generelle Infos
url %>%
  read_html() %>%
  html_nodes(".el-title") %>%
  html_text(trim = T) -> infos

# in Dataframe umwandeln
infos_df <- as.data.frame(infos)

infos_df$infos <- as.character(infos_df$infos)
# speichert Satzlänge
satz_laenge <- sapply(infos_df$infos, nchar)

# speichere kurze Sätze um sie später rauszuwerfen
indices_short <- c(which(satz_laenge< 30))

# werfe zu kurze Sätze raus
infos <- infos[-indices_short]

url %>%
  read_html() %>%
  html_nodes(".uk-margin-remove-vertical ") %>%
  html_text(trim = T) -> dates


dati <- strapply(as.character(dates), "\\d{1,2}\\.\\d{2}.")
dati <- as.matrix(as.data.frame(dati))

# ziehe Start und End-Datum und untersuche auf Fehlenden Punkt am Ende
dati_start <- dati[1,]
dati_end <- dati[2,]

# Füge Jahreszahl dazu
dati_start <- sapply(dati_start, function(x) paste0(x, "2018"))
dati_end <- sapply(dati_end,function(x) paste0(x, "2018") )

location <- rep("Chambinzky", length(title))
link <- rep("https://chambinzky.com/spielplan2", length(title))



time_beginn <- function(string){
  if(nchar(string)>=2){
    string <- as.numeric(substr(string, start = 1, stop = 2))
  }
  else string
}


url %>%
  read_html() %>%
  html_nodes("span ") %>%
  html_text(trim = T) -> times
  

# entferne 1. und letzte 3 Zeilen

# finde indices von times string, in denen Zeiten vorkommen 
indices <- which(grepl("\\d{2}:\\d{2}", times) )

# nehme nur diese Einträge raus
times <- times[c(indices)]
# reine Zeitinfo-Strings

# die ersten 6 sind relevant
relevant <- seq(1, length(title), 1)
times <- times[c(relevant)]

# bereinigen

times_1 <- function(infos){
  if(!is.na(infos)){
    time <- strapply(infos, "\\d{1,2}\\:\\d{2}")
  }
  else time <- NA
  return(time)
}

time <- sapply(times, function (x) times_1(x))

# falls Beginn im Satz vorkommt, zieh die Zahl die bis "Uhr" kommt
beginn <- str_extract(times, "(?<=Beginn).+?(?=Uhr)")


beginn <- sapply(beginn, function(x) times_1(x))

# ersetze NULL durch nichts
beginn <- gsub("NULL", "", beginn)
beginn <- gsub("NA", "", beginn)

# erste Zeit ist vor "und"
erste_zeit <- str_extract(times, "\\d{2}:\\d{2}(?=\\sund)")

# wo nichts steht, schreib nichts, um später Listen kombinieren zu können
erste_zeit <- ifelse(is.na(erste_zeit), '', erste_zeit)

# zweite Zeit steht nach und
zweite_zeit <- str_extract(times, "(?<=und\\s)\\d{2}:\\d{2}")

# doppelte Events sind solche, an denen bei erste/zweite Zeit KEIN NA-Wert drin steht
double_events <- which(!is.na(zweite_zeit))

normale_zeiten <- sapply(time, function(x) ifelse(length(x)>1, "", x))  

# zeiten erster durchlauf
zeiten <- paste0(normale_zeiten, beginn, erste_zeit)

time_end <- rep(NA, length(title))

# Preis

price <- strapply(times, "\\d{1,3},.+?")
price <- as.character(price)
price <- sapply(price, function(x) gsub("NULL", NA, x))

# neues df, welches die Infos der doppelten Events aufnimmt
double_df <- data.frame(title[c(double_events)] , link[c(double_events)], infos[c(double_events)],
                        dati_start[c(double_events)], dati_end[c(double_events)] , 
                        zweite_zeit[c(double_events)] , time_end[c(double_events)] ,
                        price[c(double_events)],  location[c(double_events)])

colnames(double_df)<- c("title","link", "infos",  "dati_start", "dati_end","zeiten", "time_end" ,"price", "location")
cham_df <- data.frame( title,  link,infos,dati_start, dati_end,  zeiten , time_end, price, location)

# kombiniere beide dfs
chambi_df <- rbind(cham_df, double_df)

# wandle in Datumsformat um und sortiere sie später
chambi_df$dati_start <- as.Date(chambi_df$dati_start, format="%d.%m.%Y")
chambi_df$dati_end <- as.Date(chambi_df$dati_end, format="%d.%m.%Y")
chambi_df$zeiten <- times(paste0(chambi_df$zeiten,":00"))
chambi_df$time_end <- times(paste0(chambi_df$time_end,":00"))



chambi_df <- chambi_df[order(chambi_df$dati_start),]

colnames(chambi_df) <- c("title",  "url", "description", "date_start", "date_end", "time_start","time_end","price", "organizer")
#"lng", "lat", "city", "street", "zip",
chambi_df$city <- "Wuerzburg"
chambi_df$street <- "Valentin-Becker-Straße 2"
chambi_df$zip <- 97072
chambi_df$lat <- 49.7909
chambi_df$lng <- 9.94384

chambi_df <- chambi_df[, c("title",  "url", "description", "lng", "lat", "city", "street", "zip","date_start", "date_end", "time_start","time_end","price", "organizer")]

rownames(chambi_df) <- NULL
# finales dataframe heißt chambi_df 