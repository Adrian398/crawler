library(tidyverse)
library(rvest)
library(dplyr)
library(chron)

#### Kulturspeicher ####
Kultur <- function() {
#bleibt die Seite 1-Seitig, wenn mehr als ein Monat zur verfügung steht?!
url5 <-"https://www.kulturspeicher.de/kulturspeicher2016/kalender/index.html"
url5 %>% read_html() -> raw_data5
raw_data5 %>% html_nodes(".uebersicht") -> node_data5

title_selector5 <- ".c_title"
text_selector5 <- "p"
preis_selector5<- "br+ span span span , p+ p"
link_selector5 <- ".mehrbutton"

node_data5 %>%
  html_node(title_selector5) %>%
  html_text(trim = T) -> t5
setdiff(t5, t5[is.na(t5)])-> t5

node_data5 %>%
  html_node(preis_selector5) %>%
  html_text(trim = T) -> p5

p5 <- p5[-c(1)]

node_data5 %>%
  html_node(link_selector5)%>%
  html_attr("href")-> l5
l5 <- l5[-c(1)]

link5 <- paste0("https://www.kulturspeicher.de",l5)



datum5 ="[0-9]{1,2}[\\./]{1} ?[A-z]{4,10}"
#uhrzeit5="([0-9]{1,2}[\\./]{1}?[0-9]{0,2}( Uhr))|([0-9]{1,2}[\\./]{1}?[0-9]{0,2}( Uhr)( - | bis )[0-9]{1,2}[\\./]{1}?[0-9]{0,2}( Uhr))"
titel5 =": .*"

t5 %>%
  str_extract_all(titel5) -> tit5

as.character(tit5)-> char

#Titel bereinigen
sub(": [0-9]{1,2}\\.[0-9]{1,2} Uhr: ", "",char) -> char1
sub(": [0-9]{1,2} Uhr bis [0-9]{1,2} Uhr: ", "",char1) -> char2
sub(": [0-9]{1,2}\\.[0-9]{1,2} Uhr - [0-9]{1,2}\\.[0-9]{1,2} Uhr: ","",char2) -> char3
sub("^: [0-9]{1,2}\\.[0-9]{1,2} Uhr bis [0-9]{1,2}\\.[0-9]{1,2} Uhr: ","",char3)->char4
sub("^: ","",char4)->char5


node_data5 %>%
  html_node(text_selector5) %>%
  html_text(trim = T) -> txt5
txt5 <- txt5[-c(1)]

df5 <- data.frame(title = t5, description=txt5, price=p5, url=link5)
df5test <- separate(data= df5, col=title, into = c("Datum", "Uhrzeit"), sep="\\:" )
#df5test <- df5test[-c(1),]

df5tes <- cbind(title=char5, df5test)
df5tes$Uhrzeit <- sub(pattern = " bis ", replacement = " - ", x = df5tes$Uhrzeit)
df5tes <- separate(data= df5tes, col=Uhrzeit, into = c("time_start", "time_end"), sep=" - " )
df5tes <- separate(data= df5tes, col=Datum, into = c("date_start", "date_end"), sep=" - " )

as.Date(df5tes$date_start, "%d. %B") -> df5tes$date_start
as.Date(df5tes$date_end, "%d. %B") -> df5tes$date_end
as.Date(df5tes$date_start, "%d.%B") -> df5tes$date_start
as.Date(df5tes$date_end, "%d.%B") -> df5tes$date_end
format(df5tes$date_start, format="%d.%m.") -> df5tes$date_start
format(df5tes$date_end, format="%d.%m.") -> df5tes$date_end


as.character(df5tes$title) -> df5tes$title
bereinigt <- df5tes

for (i in 1:nrow(bereinigt)) {
  if (is.na(bereinigt[i,]$title)| bereinigt[i,]=="character(0)") {
    bereinigt <- bereinigt[-c(i),]
  }
}
bereinigt1 <- bereinigt[,c("title","url","description","date_start","date_end","time_start","time_end", "price")]

bereinigt1 <- separate_rows(bereinigt1, time_start,sep=" und ",convert = TRUE)


bereinigt1$date_start <- as.Date(bereinigt1$date_start, format = "%d.%m")
bereinigt1$date_end <- as.Date(bereinigt1$date_end, format = "%d.%m")


bereinigt1$time_start <- sub(pattern = "\\.", replacement = ":", x = bereinigt1$time_start)
bereinigt1$time_end <- sub(pattern = "\\.", replacement = ":", x = bereinigt1$time_end)

bereinigt1$time_start <- sub(pattern = "Uhr", replacement = "", x=bereinigt1$time_start)
bereinigt1$time_end <- sub(pattern = "Uhr", replacement = "", x=bereinigt1$time_end)


bereinigt1$time_start <- times(paste0(bereinigt1$time_start, ":00"))
bereinigt1$time_end <- times(paste0(bereinigt1$time_end, ":00"))

#testi <- times(times=bereinigt1$time_start, format= c(times='h.m'))
#times()

# bereinigt1$time_start <- as.Date(bereinigt1$time_start)
# bereinigt1$time_end <- as.Date(bereinigt1$time_end)

  
veranstaltungenKulturspeicher <- cbind(
  bereinigt1,
  organizer = "Museum im Kulturspeicher",
  street = "Oskar-Laredo-Platz 1",
  city="Wuerzburg",
  zip=97080,
  lng=9.92119,
  lat=49.80243
)
return(veranstaltungenKulturspeicher)

}