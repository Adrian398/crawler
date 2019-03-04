url_boot <- "https://www.das-boot.com/programm"


url_boot %>%
  read_html() %>%
  html_nodes(".btn-u") %>%
  html_attr("href") -> links

links <- links[!is.na(links)]

getDataTitel <- function(links){
  links %>%
    read_html() %>%
    html_nodes(".termin-detail-title") %>%
    html_text(trim = T) -> titel
}
map(links, getDataTitel) -> Titel_List


getDataDatum <- function(links){
  links %>%
    read_html() %>%
    html_nodes(".month , .month+ h1") %>%
    html_text(trim = T) -> datum
}
map(links, getDataDatum) -> Datum_List


getDataUhrzeit <- function(links){
  links %>%
    read_html() %>%
    html_nodes(".time_age") %>%
    html_text(trim = T) -> uhrzeit
  uhrzeit <- substr(uhrzeit, 1, 9)
}
map(links, getDataUhrzeit) -> Uhrzeit_List


getDataBeschreibung <- function(links){
  links %>%
    read_html() %>%
    html_nodes(".specials") %>%
    html_text(trim = T) -> Beschreibung
}
map(links, getDataBeschreibung) -> Beschreibung_List

getDataPreis <- function(links){
  links%>%
    read_html() %>%
    html_nodes(".list-specials li:nth-child(1)") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> Preis
}
map(links, getDataPreis) -> Preis_List


Titel_List <- unlist(Titel_List, use.names=FALSE)
Datum_List <- unlist(Datum_List, use.names=FALSE)
Uhrzeit_List <- unlist(Uhrzeit_List, use.names=FALSE)
Beschreibung_List <- unlist(Beschreibung_List, use.names=FALSE)
Preis_List <- unlist(Preis_List, use.names=FALSE)


i <- 1
x <- 1
Monat <- list()
Tag <- list()

while (i < length(Datum_List)) {
  Monat[[x]] <-c(Datum_List[i])
  Tag[[x]] <-c(Datum_List[i+1])
  x <- x+1
  i <- i+2
}

Monat <- unlist(Monat, use.names=FALSE)
Tag <- unlist(Tag, use.names=FALSE)

boot_df <- data.frame(Titel_List, Uhrzeit_List, Beschreibung_List, Monat, Tag)

boot_df$Monat <- boot_df$Monat %>%
{ gsub("Jan","01", .) } %>%
{ gsub("Feb","02", .) } %>%
{ gsub("Mär","03", .) } %>%
{ gsub("Apr","04", .) } %>%
{ gsub("Mai","05", .) } %>%
{ gsub("Jun","06", .) } %>%
{ gsub("Jul","07", .) } %>%
{ gsub("Aug","08", .) } %>%
{ gsub("Sep","09", .) } %>%
{ gsub("Okt","10", .) } %>%
{ gsub("Nov","11", .) } %>%
{ gsub("Dez","12", .) }


boot_df$Tag <- as.numeric(gsub("\\D", "", boot_df$Tag)) 

boot_df$Uhrzeit_List <- str_sub(boot_df$Uhrzeit_List, str_length(boot_df$Uhrzeit_List) - 6, str_length(boot_df$Uhrzeit_List)-1)
#Uhrzeit bekommen

boot_df$Tag <- as.character(boot_df$Tag)
boot_df <- boot_df %>%
  mutate(Datum = if_else(str_length(boot_df$Tag) == 1, as.character(paste("0",boot_df$Tag, sep = "")), boot_df$Tag))

boot_df <- mutate(boot_df, StartDatum = paste(boot_df$Datum, boot_df$Monat, sep = "."))
#Datum erstellen


boot_latitude <- rep(49.8018,nrow(boot_df))
boot_longitude <- rep(9.92291,nrow(boot_df)) 
boot_city <- rep("Würzburg",nrow(boot_df))
boot_street <- rep("Veitshöchheimer Straße 14",nrow(boot_df))
boot_zip <- rep(97080,nrow(boot_df))
boot_organizer <- rep("Das Boot",nrow(boot_df))
boot_EndUhrzeit <- rep("",nrow(boot_df))
#Latitude und Longitude deklarieren


df_preis <- data.frame(Preis_List)
df_preis <- mutate(df_preis, Preis = if_else(grepl("Eintritt", df_preis$Preis_List) == T, df_preis$Preis_List,as.integer(1)))
#Preis herausfinden

df_boot_final <- data.frame(title=boot_df$Titel_List, url = links, description = boot_df$Beschreibung_List, lng = boot_longitude, lat = boot_latitude, city = boot_city, street = boot_street, zip = boot_zip, date_start = boot_df$StartDatum, date_end = boot_df$StartDatum, time_start = boot_df$Uhrzeit_List, time_end = boot_EndUhrzeit, price = df_preis$Preis, organizer = boot_organizer)
#fertig
df_boot_final <- unique(df_boot_final)
names(df_boot_final)


##fix old dataframe to new format
crawled_df <- data.frame(
  title = title,
  date_start = date_start,
  date_end = date_end, 
  time_start = time_start,
  time_end = time_end,
  description = description,
  lat = lat,
  lng = lng,
  street = street,
  zip = zip,
  city = city,
  link = link,
  image_url = image_url)

meta_df = data.frame(url_crawler = url
                     , organizer = organizer)


#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
