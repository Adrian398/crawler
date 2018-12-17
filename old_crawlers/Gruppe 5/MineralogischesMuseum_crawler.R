
MineralogischesMuseum_crawler <- function(){


library(tidyverse)
require(rvest)

minmusdaten="http://www.mineralogisches-museum.uni-wuerzburg.de/aktuelles/"

# Titel crawlen

minmusdaten %>%
  read_html() %>%
  html_nodes(".csc-header-n2+ h2")%>%
  html_text() -> minmus_titel


# Datum crawlen

minmusdaten %>%
  read_html() %>%
  html_nodes(".csc-header-n2 h2")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2}[\\./]{1}.{0,9}") %>% 
  strptime(format="%e. %B") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}") -> minmus_datum
as.Date(minmus_datum)->minmus_datum

# Zeit crawlen

minmusdaten %>%
  read_html() %>%
  html_nodes("h2+ h3")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2}.{1,5}") %>% 
  str_replace("[^0-9]{1,5}", ".00") %>% 
  str_replace("\\.", ":")-> minmus_zeit

minmusdaten %>%
  read_html() %>%
  html_nodes("h2+ h3")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2} Uhr") %>% 
  str_replace(" Uhr", ".00") %>% 
  str_replace("\\.", ":")-> minmus_zeit_bis


# Beschreibung crawlen

minmusdaten %>%
  read_html() %>%
  html_nodes(".bodytext+ .bodytext")%>%
  html_text()  -> minmus_text


# Eintritt crawlen
minmusdaten %>%
  read_html() %>%
  html_nodes(".bodytext+ .bodytext")%>%
  html_text()  %>% 
  str_extract("[0-9]{1,3}.{1,2}Euro") -> minmus_eintritt



## Standardausstellung crawlen

minmusdaten2 = "http://www.mineralogisches-museum.uni-wuerzburg.de/willkommen/"

minmusdaten2 %>% 
  read_html() %>% 
  html_nodes("#c564590 .csc-firstHeader") %>% 
  html_text() %>% 
  str_replace("Öffnungszeiten", "Standardausstellung")-> minmus2_titel

minmusdaten2 %>%
  read_html() %>%
  html_nodes("#c564591 .intro:nth-child(1)")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2}.{1,5}") %>% 
  str_replace("[^0-9]{1,5}", ".00")-> minmus2_zeit

minmusdaten2 %>%
  read_html() %>%
  html_nodes("#c564591 .intro:nth-child(1)")%>%
  html_text() %>% 
  str_extract("[0-9]{1,2} Uhr") %>% 
  str_replace(" Uhr", ".00")-> minmus2_zeit_bis

minmusdaten2 %>% 
  read_html() %>% 
  html_nodes("#c564591 .intro:nth-child(2)") %>% 
  html_text() -> minmus2_text




### Auf der Webseite nur Sonntag und Mittwoch gegeben: Alle Sonntage und Mittwochs ausgeben

getAllSundays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==0]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

getAllWednesdays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==3]
  Ms[!is.na(Ms)]
}

getAllWednesdays(2018) -> all_wednesdays
getAllSundays(2018) -> all_sundays
all_sundays
all_wednesdays

str(all_sundays)
str(all_wednesdays)
as.Date(all_wednesdays) -> all_wednesdays
as.Date(all_sundays) -> all_sundays



# Dataframes erstellen

minmus1 <- data.frame(minmus_titel, minmus_datum, minmus_datum, minmus_zeit, minmus_zeit_bis, minmus_eintritt, minmus_text)
colnames(minmus1) <- c("title", "date_start", "date_end", "time_start", "time_end", "price", "description")
minmus2_Mittwoch <- data.frame(minmus2_titel, all_wednesdays, all_wednesdays, minmus2_zeit, minmus2_zeit_bis, minmus2_text)
colnames(minmus2_Mittwoch) <- c("title", "date_start", "date_end", "time_start", "time_end", "description")
minmus2_Sonntag <- data.frame(minmus2_titel, all_sundays, all_sundays, minmus2_zeit, minmus2_zeit_bis, minmus2_text)
colnames(minmus2_Sonntag) <- c("title", "date_start", "date_end", "time_start", "time_end", "description")


# Dataframes zusammenfügen

bind_rows(minmus1, minmus2_Mittwoch, minmus2_Sonntag)->df


# Inhalt anpassen

df$time_start %>% 
  str_replace_all("\\.", ":") ->df$time_start
df$time_end %>% 
  str_replace_all("\\.", ":") ->df$time_end

# Restliche Werte einfügen

df %>% 
  mutate(organizer="Mineralogisches Museum") %>% 
  mutate(url="http://www.mineralogisches-museum.uni-wuerzburg.de/willkommen/") %>% 
  mutate(lng="9.9707861") %>% 
  mutate(lat="49.7815829") %>% 
  mutate(city="Wuerzburg") %>% 
  mutate(street="Am Hubland Sued, Gebaeude G1") %>% 
  mutate(zip="97074") -> df

df <- df[,c(1, 9, 7, 10, 11, 12, 13, 14, 2, 3, 4, 5, 6, 8)] 


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
as.character(df$price)->df$price

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
# str(df$price)

return(df)

}
