HFM <- function() {

#---------------------HFM So gut wie fertig-----------------
hfm_url <- "http://www.hfm-wuerzburg.de/veranstaltungen"

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(hfm_url)

# Smarter: Click button until all sites are loaded
run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'css selector', ".more-link")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(7)
}
raw_data_hfm <- read_html(remDr$getPageSource()[[1]])

raw_data_hfm %>%
  html_nodes(".cntblk a") %>%
  html_attr("href") -> links_hfm

links_hfm <- links_hfm[!is.na(links_hfm)]
links_hfm <- links_hfm[-(1:3)]
links_hfm


getDataTitel_hfm <- function(links_hfm){
  links_hfm %>%
    read_html() %>%
    html_nodes("h2") %>%
    html_text(trim = T) -> titel_hf2
  titel_hf2 <- data.frame(titel_hf2)
  titel <- titel_hf2$titel_hf2[1]
}
map(links_hfm, getDataTitel_hfm) -> titel_hfm

getDataZeit_hfm <- function(links_hfm){
  links_hfm %>%
    read_html() %>%
    html_nodes(".event-date") %>%
    html_text(trim = T) -> zeit_hfm
}
map(links_hfm, getDataZeit_hfm) -> zeit_hfm

getDataBeschreibung_hfm <- function(links_hfm){
  links_hfm %>%
    read_html() %>%
    html_nodes(".event-description") %>%
    html_text(trim = T) -> beschreibung_hfm
}
map(links_hfm, getDataBeschreibung_hfm) -> beschreibung_hfm

getDataOrt_hfm <- function(links_hfm){
  links_hfm %>%
    read_html() %>%
    html_nodes(".event-location") %>%
    html_text(trim = T) -> ort_hfm
}
map(links_hfm, getDataOrt_hfm) -> ort_hfm

titel_hfm <- unlist(titel_hfm, use.names=FALSE)
zeit_hfm <- unlist(zeit_hfm, use.names=FALSE)
beschreibung_hfm <- unlist(beschreibung_hfm, use.names=FALSE)
ort_hfm <- unlist(ort_hfm, use.names=FALSE)

hfm_df <- data.frame(Titel = titel_hfm, Zeit = zeit_hfm, Beschreibung = beschreibung_hfm, Ort = ort_hfm)
#to do: richtiges Format, und ort hinzufügen

# Shut down selenium
remDr$close()
rm(rD)
gc()


hfm_df <- hfm_df %>%
  separate(Zeit, c("A", "B"), sep=",") 

hfm_df <- hfm_df %>%
  separate(A, c("C", "D","E"), sep=" ") 

hfm_df$D <- hfm_df$D %>%
{ gsub("Januar","01", .) } %>%
{ gsub("Februar","02", .) } %>%
{ gsub("März","03", .) } %>%
{ gsub("April","04", .) } %>%
{ gsub("Mai","05", .) } %>%
{ gsub("Juni","06", .) } %>%
{ gsub("Juli","07", .) } %>%
{ gsub("August","08", .) } %>%
{ gsub("September","09", .) } %>%
{ gsub("Oktober","10", .) } %>%
{ gsub("November","11", .) } %>%
{ gsub("Dezember","12", .) }

hfm_df$C <- gsub('[[:punct:] ]+','',hfm_df$C)

hfm_df$C <- if_else(str_length(hfm_df$C) == 1, as.character(paste("0",hfm_df$C, sep = "")), hfm_df$C)

hfm_df <- mutate(hfm_df, StartDatum = paste(hfm_df$C, hfm_df$D, hfm_df$E, sep = "."))
#Datum erstellen
hfm_df$B <- gsub(" Uhr", "", hfm_df$B)


hfm_latitude <- rep(49.79439,nrow(hfm_df))
hfm_longitude <- rep(9.94059,nrow(hfm_df)) 
hfm_city <- rep("Würzburg",nrow(hfm_df))
hfm_street <- rep("Hofstallstraße 6-8",nrow(hfm_df))
hfm_zip <- rep(97070,nrow(hfm_df))
hfm_organizer <- rep("Hochschule für Musik",nrow(hfm_df)) 
hfm_preis <- rep(NA,nrow(hfm_df)) 
hfm_EndUhrzeit <- rep("",nrow(hfm_df))
#Latitude und Longitude deklarieren

hfm_df <- mutate(hfm_df, Beschreibung = paste(hfm_df$Beschreibung, "," ,hfm_df$Ort))

#df_hfm_final <- data.frame(Titel=hfm_df$Titel, Beschreibung=hfm_df$Beschreibung, StartDatum = hfm_df$StartDatum, EndDatum = hfm_df$StartDatum, StartUhrzeit = hfm_df$B, EndZeit = hfm_EndUhrzeit, BeschreibenderOrt = hfm_df$Ort, Adresse = hfm_adresse, Latitude = hfm_latitude, Longitude = hfm_longitude)
#fertig
#df_hfm_final <- unique(df_hfm_final)

df_hfm_final <- data.frame(title=hfm_df$Titel, url = links_hfm, description = hfm_df$Beschreibung, lng = hfm_longitude, lat = hfm_latitude, city = hfm_city, street = hfm_street, zip = hfm_zip, date_start = hfm_df$StartDatum, date_end = hfm_df$StartDatum, time_start = hfm_df$B, time_end = hfm_EndUhrzeit, price = hfm_preis, organizer = hfm_organizer)
df_hfm_final <- unique(df_hfm_final)

return(df_hfm_final)
}
