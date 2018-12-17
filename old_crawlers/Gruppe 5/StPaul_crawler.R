
StPaul_crawler <- function(){

library(tidyverse)
require(rvest)
library(chron)

stpauldaten="http://www.heidingsfeld-evangelisch.de/termine.php"

### Datum crawlen

stpauldaten %>%
  read_html() %>%
  html_nodes("td:nth-child(1) .MsoTitle+ .MsoTitle span:nth-child(1)")%>%
  html_text() -> stpaul_datum

# Inhalt anpassen
stpaul_datum %>%
  str_replace_all("[:space:]{2,}", " ") -> stpaul_datum

# Datumsformat anpassen
stpaul_datum %>% 
  strptime(format="%a., %d.%m.,") -> stpaul_datum


### Titel crawlen
stpauldaten %>% 
  read_html() %>% 
  html_nodes(".MsoTableGrid td:nth-child(2) .MsoTitle+ .MsoTitle span:nth-child(1)")%>% 
  html_text() -> stpaul_titel

# Inhalt anpassen
stpaul_titel %>%
  str_replace_all("[:space:]{2,}", " ") %>% 
  str_replace_all(",", "")-> stpaul_titel


# Ort und Zeit crawlen (ist zusammen in einem Node)
stpauldaten %>% 
  read_html() %>% 
  html_nodes("td+ td span+ span") %>% 
  html_text() -> stpaul_ort_zeit

# Inhalt anpassen
stpaul_ort_zeit %>%
  gsub(",", "", .) %>% 
  str_replace_all("[:space:]{2,}", " ") %>% 
  str_replace_all("[:space:]h", " ") -> stpaul_ort_zeit 

# Ort und Zeit teilen
remove <- c(seq(1,19,by=2))
remove2 <- c(seq(2,20,by=2))
stpaul_nr <- c(1:20)
stpaul_ort_zeit <- data.frame(stpaul_nr, stpaul_ort_zeit)
stpaul_ort <- stpaul_ort_zeit[-remove,]
stpaul_ort <- select(stpaul_ort, stpaul_ort_zeit)
stpaul_zeit <- stpaul_ort_zeit[-remove2,]
stpaul_zeit <- select(stpaul_zeit, stpaul_ort_zeit)
stpaul_ort$stpaul_ort_zeit -> stpaul_ort

# Inhalt anpassen
stpaul_zeit$stpaul_ort_zeit %>% 
  str_replace_all("\\.", ":") -> stpaul_zeit


# Beschreibung crawlen und Ort hinzufügen

stpauldaten %>% 
  read_html() %>% 
  html_nodes("td~ td+ td .MsoTitle+ .MsoTitle span")%>% 
  html_text() %>% 
  gsub("\\.", "", .) -> stpaul_text 

# Inhalt anpassen
stpaul_text %>%
  str_replace_all("[:space:]{2,}", " ") -> stpaul_text
Nr11 <- c(01:11)
stpaul_text <- data.frame(Nr11, stpaul_text)
stpaul_text <- stpaul_text[-10,]
select(stpaul_text, stpaul_text)->stpaul_text
stpaul_text$stpaul_text -> stpaul_text
stpaul_text %>% 
  paste("- ") %>% 
  paste(stpaul_ort)-> stpaul_text
# Restliche Werte übergeben

url <- "http://www.heidingsfeld-evangelisch.de/aktuelles.php"
organizer <- "St Paul"
city <- "Wuerzburg"
zip <- "97080"
lat <- "49.7647395"
lng <- "9.9452118"
price <- NA
street <- "Reuterstraße 10"

# Dataframe erzeugen
df <- data.frame(stpaul_titel, url, lng, lat, stpaul_text, city, street, zip, stpaul_datum, stpaul_datum, stpaul_zeit, stpaul_zeit=NA, price, organizer)

#Reihenfolge der Spalten
df <- df[,c(1,2,5,3,4,6,7,8,9,10,11,12,13,14)] 

#Spaltennamen
colnames(df) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")


# Datenformate anpassen (lat und long muss zunächst zu chr umgewandelt werden, da sonst 1 ausgegeben wird)
as.character(df$organizer)->df$organizer
as.character(df$title)->df$title
as.character(df$city)->df$city
as.character(df$street)->df$street
as.character(df$zip)->df$zip
as.numeric(df$zip)->df$zip
as.Date(df$date_start)->df$date_start
as.Date(df$date_end)->df$date_end
as.character(df$description)->df$description
as.character(df$url)->df$url
df$time_start <- times(paste0(df$time_start, ":00"))
as.character(df$lat)->df$lat
as.numeric(df$lat)->df$lat
as.character(df$lng)->df$lng
as.numeric(df$lng)->df$lng

## Formate überprüfen
# str(df$organizer)
# str(df$title)
# str(df$city)
# str(df$street)
# str(df$zip)
# str(df$date_start)
# str(df$date_end)
# str(df$description)
# str(df$url)
# str(df$time_start)
# str(df$time_end)
# str(df$lng)
# str(df$lat)

return(df)

}