
Tanzspeicher_crawler <- function(){


library(tidyverse)
require(rvest)
library(devtools)
library(RSelenium)


url="http://tanzspeicher.de/sample-page-2/produktionen"

url %>%
  read_html() -> raw_data

# Nodes für die einzelnen Veranstaltungen
raw_data %>%
  html_nodes(".entry-content") -> node_data

# Nodes aus Übersichtsseite crawlen
title_selector <- ".wp-show-posts-entry-title a"
datum_selector <- ".published"
link_selector <- ".more-link"

# Titel, Datum und Links crawlen
node_data %>%
  html_nodes(title_selector) %>%
  html_text(trim = T) -> title

#node_data %>%
#  html_nodes(datum_selector) %>%
#  html_text(trim = T) %>% 
#  strptime(format = "%B %e, %Y")-> datum

node_data %>%
  html_nodes(link_selector) %>%
  html_attr("href")-> links



## Beschreibung aus Unterseite crawlen

get_beschreibung <- function(event_url) {
  
  #event_url = "http://tanzspeicher.de/sample-page-2/produktionen"
  event_url %>%
    read_html() -> raw_event_data
  
  raw_event_data %>%
    html_nodes(".entry-the-content") %>%
    html_text(trim = T) -> beschreibung
  
  df_singlePage <- data.frame(beschreibung, event_url)
  
  return(df_singlePage)
}

map_dfr(links, get_beschreibung) -> event_daten

event_daten$beschreibung -> Beschreibung
Beschreibung %>% 
str_replace_all("[:space:]{2,}", " ") -> Beschreibung

# Links crawlen
event_daten$event_url -> links


# Datum, Zeit und Preis aus Beschreibung crawlen

Beschreibung%>% 
  str_extract_all("[0-9]{1,3} Euro", simplify=TRUE) -> price
Beschreibung%>% 
  str_extract_all("[0-9]{1,2}[\\.:][0-9]{2}", simplify=TRUE) -> zeit
Beschreibung %>% 
  str_extract_all("[0-9]{1,2}[\\./][0-9]{1,2}[\\./]?([0-9]{4})?|[0-9]{1,2}\\. Januar [0-9]{0,4}|[0-9]{1,2}\\. Februar [0-9]{0,4}|[0-9]{1,2}\\. März [0-9]{0,4}|[0-9]{1,2}\\. April [0-9]{0,4}|[0-9]{1,2}\\. Mai [0-9]{0,4}|[0-9]{1,2}\\. Juni [0-9]{0,4}|[0-9]{1,2}\\. Juli [0-9]{0,4}|[1-9]{1,2}\\. August [0-9]{0,4}|[0-9]{1,2}\\. September [0-9]{0,4}|[1-9]{1,2}\\. Oktober[0-9]{0,4}|[0-9]{1,2}\\. November [0-9]{0,4}|[0-9]{1,2}\\. Dezember", simplify =TRUE) -> datum

datum[,1] -> datum_von
datum[,2] -> datum_bis

strptime(datum_von, format="%e. %B")->datum_von
strptime(datum_bis, format="%e. %B")->datum_bis



# Restliche Daten eingeben
city <- "Wuerzburg"
street <- "Oskar-Laredo-Platz 1"
zip <- "97080"
organizer <- "Tanzspeicher"
lng <- "9.9102"
lat <- "49.8158"


# Dataframe erzeugen
df <- data.frame(title, url=links, description=Beschreibung, lng, lat, city, street, zip, date_start=datum_von, date_end=datum_bis, time_start=zeit, time_end=NA, price, organizer)


# Anpassung Datenformate
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
as.character(df$lat)->df$lat
as.numeric(df$lat)->df$lat
as.character(df$lng)->df$lng
as.numeric(df$lng)->df$lng
df$time_start <- times(paste0(df$time_start, ":00"))



# Überprüfung Datenformate
# str(df$organizer)
# str(df$title)
# str(df$city)
# str(df$street)
# str(df$zip)
# str(df$date_start)
# str(df$date_end)
# str(df$description)
# str(df$url)
# str(df$lat)
# str(df$lng)
# str(df$time_start)
# str(df$time_end)

return(df)

}