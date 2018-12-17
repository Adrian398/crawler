StAlfons_crawler <- function(){


library(tidyverse)
require(rvest)
library(devtools)
library(RSelenium)
library(chron)


url="http://st-alfons.pg-alfons-nikolaus.de/veranstaltungen-in-st--alfons#page1"

url %>%
  read_html() -> raw_data

# Nodes f¸r die einzelnen Veranstaltungen
raw_data %>%
  html_nodes(".listcontainer") -> node_data

# Nodes aus ‹bersichtsseite crawlen
title_selector <- ".itemtitle a"
link_selector <- ".more-text"
text_selector <- ".itemcontent"

# Titel und Links crawlen
node_data %>%
  html_nodes(title_selector) %>%
  html_text(trim = T) -> title

node_data %>%
  html_nodes(text_selector) %>%
  html_text(trim = T) %>% 
  str_remove("MEHR") %>% 
  str_replace_all("[:space:]{2,}", " ")-> description
description
node_data %>%
  html_nodes(link_selector) %>%
  html_attr("href")-> links
paste("http:",links) -> links
links %>%
  str_replace_all(" ", "")->links

## Beschreibung aus Unterseite crawlen

get_beschreibung <- function(event_url) {
  
  #event_url = "http://st-alfons.pg-alfons-nikolaus.de/veranstaltungen-in-st--alfons#page1"
  event_url %>%
    read_html() -> raw_event_data
  
  raw_event_data %>%
    html_nodes(".head-date-container .date") %>%
    html_text(trim = T) -> datum

  df_singlePage <- data.frame(datum, event_url)
  
  return(df_singlePage)
}

map_dfr(links, get_beschreibung) -> event_daten

event_daten$datum -> datum
#datum
# Links crawlen
event_daten$event_url -> links
#links

# Datum anpassen
datum
datum %>%
  str_extract("[0-9]{1,2}[\\./][0-9]{1,2}[\\./]20[0-9]{1,2}") -> datum_von
datum %>%
  str_extract("[-(bis)] [0-9]{1,2}[\\./][0-9]{1,2}[\\./]20[0-9]{1,2}") -> datum_bis
datum_bis %>% 
  str_replace("- ","")->datum_bis
datum_bis
datum_von
strptime(datum_von, format="%d. %m. %Y")->datum_von
strptime(datum_bis, format="%d. %m. %Y")->datum_bis

lng = NA
lat = NA
price = NA
street = NA
city = NA
zip = NA
url=NA
organizer=NA



# Dataframe erzeugen
df1 <- data.frame(title, url=links, description, lng, lat, city, street, zip, date_start=datum_von, date_end=datum_bis, time_start=NA, time_end=NA, price, organizer)




### Daten f¸r weitere Seiten crawlen

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://st-alfons.pg-alfons-nikolaus.de/veranstaltungen-in-st--alfons#page1")

# Seiten laden
run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'css selector', ".nav-next .ui-icons-block")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

# Get HTML
site_stalfons <- read_html(remDr$getPageSource()[[1]])


# Nodes f¸r die einzelnen Veranstaltungen
site_stalfons %>%
  html_nodes(".listcontainer") -> node_data

# Nodes aus ‹bersichtsseite crawlen
title_selector <- ".itemtitle a"
link_selector <- ".more-text"
text_selector <- ".itemcontent"

# Titel und Links crawlen
node_data %>%
  html_nodes(title_selector) %>%
  html_text(trim = T) -> title

node_data %>%
  html_nodes(text_selector) %>%
  html_text(trim = T) %>% 
  str_remove("MEHR") %>% 
  str_replace_all("[:space:]{2,}", " ")-> description
#description
node_data %>%
  html_nodes(link_selector) %>%
  html_attr("href")-> links
paste("http:",links) -> links
links %>%
  str_replace_all(" ", "")->links

## Beschreibung aus Unterseite crawlen

get_beschreibung <- function(event_url) {
  
  #event_url = "http://st-alfons.pg-alfons-nikolaus.de/veranstaltungen-in-st--alfons#page1"
  event_url %>%
    read_html() -> raw_event_data
  
  raw_event_data %>%
    html_nodes(".head-date-container .date") %>%
    html_text(trim = T) -> datum
  
  df_singlePage <- data.frame(datum, event_url)
  
  return(df_singlePage)
}

map_dfr(links, get_beschreibung) -> event_daten

event_daten$datum -> datum
#datum
# Links crawlen
event_daten$event_url -> links
#links

# Datum anpassen
datum
datum %>%
  str_extract("[0-9]{1,2}[\\./][0-9]{1,2}[\\./]20[0-9]{1,2}") -> datum_von
datum %>%
  str_extract("[-(bis)] [0-9]{1,2}[\\./][0-9]{1,2}[\\./]20[0-9]{1,2}") -> datum_bis
datum_bis %>% 
  str_replace("- ","")->datum_bis
#datum_bis
#datum_von
strptime(datum_von, format="%d. %m. %Y")->datum_von
strptime(datum_bis, format="%d. %m. %Y")->datum_bis


df2 <- data.frame(title, url=links, description, lng, lat, city, street, zip, date_start=datum_von, date_end=datum_bis, time_start=NA, time_end=NA, price, organizer)



## Dataframes zusammenf¸gen

bind_rows(df1, df2) -> df

# Restliche Werte ¸bergeben
df %>% 
  mutate(organizer="St Alfons") %>% 
  mutate(url="http://st-alfons.pg-alfons-nikolaus.de/veranstaltungen-in-st--alfons#page1") %>% 
  mutate(lng = "9.95303") %>% 
  mutate(lat = "49.77511") %>% 
  mutate(street="Matthias-Ehrenfried-Straﬂe 2") %>% 
  mutate(city="Wuerzburg") %>% 
  mutate(zip="97074") %>% 
  mutate(price=NA) -> df

# Inhalt anpassen
df$street %>% 
  str_replace_all("- W¸rzburg","") ->df$street

#Reihenfolge der Spalten
df <- df[,c(1, 9, 7, 10, 11, 12, 2, 13, 3, 4, 5, 6, 14, 8)]
df
#Bei nur einer Seite werden Veranstaltungen mehrfacht gecrawlt -> entfernen
unique(df) -> df

# Datenformate anpassen (lat und long muss zun‰chst zu chr umgewandelt werden, da sonst 1 ausgegeben wird)
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

# Browser schlieﬂen
remDr$close()
rm(rD)
gc()


return(df)


}




