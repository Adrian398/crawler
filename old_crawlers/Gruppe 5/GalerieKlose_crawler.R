
GalerieKlose_crawler <- function(){
  

library(tidyverse)
require(rvest)

klosedaten="http://www.galerie-ilkaklose.de/aktuelles.html"


# Titel crawlen

klosedaten %>%
  read_html() %>%
  html_nodes(".centered+ p .bold , .subTitle")%>%
  html_text() -> klose_titel


# Datum crawlen

klosedaten %>%
  read_html() %>%
  html_nodes(".centered+ p .italicBold , .subTitle+ .centered .italicBold:nth-child(1)")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2}\\.[0-9]{1,2}(\\.20[0-9]{2})?") -> klose_datum
klose_datum %>% 
  strptime(format="%e. %m") %>% 
  str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") -> klose_datum
as.Date(klose_datum) -> klose_datum


# Zeit crawlen

klosedaten %>%
  read_html() %>%
  html_nodes(".centered+ p .italicBold , .subTitle+ .centered .italicBold:nth-child(1)")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2}(-| bis )[0-9]{1,2} Uhr") %>% 
  str_extract("[0-9]{1,2}(-| bis )") %>% 
  str_replace("(-| bis )", ".00") %>% 
  str_replace("\\.", ":")-> klose_zeit

klosedaten %>%
  read_html() %>%
  html_nodes(".centered+ p .italicBold , .subTitle+ .centered .italicBold:nth-child(1)")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2} Uhr") %>% 
  str_replace(" Uhr", ".00") %>% 
  str_replace("\\.", ":")-> klose_zeit_bis


# Ort crawlen

klosedaten %>%
  read_html() %>%
  html_nodes(".centered .italicBold:nth-child(2)")%>%
  html_text() %>% 
  str_replace_all(", 97080 Würzburg","") %>% 
  str_replace_all("In den Räumen der Firma BARC ","")-> klose_street
klose_street<-c(klose_street,"Leitengraben 3")

df <- data.frame(title=klose_titel, url="http://www.galerie-ilkaklose.de/aktuelles.html", description="In den Raeumen der Firma BARC", lng="9.93938", lat="49.76346", city="Wuerzburg", street=klose_street, zip="97080", date_start=klose_datum, date_end=klose_datum, time_start=klose_zeit, time_end=klose_zeit_bis, price=NA, organizer="Galerie Ilka Klose")


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
df$time_end <- times(paste0(df$time_end, ":00"))
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