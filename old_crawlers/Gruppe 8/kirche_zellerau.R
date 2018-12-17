library(rvest)
library(tidyverse)
library(chron)
url <- "http://www.kirche-zellerau.de/veranstaltungen"

#url %>%
# read_html() %>%
#html_nodes(".ui-helper-reset") %>%
#html_text()

#url %>%
# read_html() %>%
#html_nodes(".date") %>%
#html_text()



# Setting up RSelenium
library(devtools)
library(RSelenium)

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com")
remDr$navigate(url)

run <- TRUE
i <- 1
while (run){
  tryCatch(
    remDr$findElement(using = 'css selector', ".nav-text")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  i <- i + 1
  Sys.sleep(2)
}

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

#Titel
site %>%
  html_nodes(".itemtitle a") %>%  
  html_text(trim = T) -> title

#Alle links
site %>%
  html_nodes(".itemtitle a") %>% 
  html_attr("href") -> link
gsub("//www.","www.", link) -> link
link

#Beschreibung
site %>%
  html_nodes(".itemcontent") %>% 
  html_text(trim = T) -> beschreibung


#Ort mit Datum -> Dataframe 
site %>% 
  html_nodes(".city") %>%
  html_text(trim = T)-> city 
city2 <- paste(city, " Würzburg", sep="")


site %>% 
  html_nodes(".date , .city") %>%
  html_text(trim = T)-> test 
paste(test,collapse=" ") -> test
stringr::str_extract_all(test, "\\d{2}[.]\\d{2}[.]\\d{4}[ ]\\D+") -> DatumText
stringr::str_extract_all(DatumText, "\\d{2}[.]\\d{2}[.]\\d{4}") -> Datum2
unlist(Datum2) -> date
date <- as.POSIXct(date,format="%d.%m.%Y")
dfOrtDatum <- data.frame(Ort = city2, Datum = date)
dfOrtDatum

#Uhrzeit
site %>% 
  html_nodes(".datetime") %>%
  html_text(trim = T)-> time

stringr::str_extract_all(time, "\\d{2}[:]\\d{2}") -> Zeit
unlist(Zeit) -> Zeit
Zeit

time <- times(paste0(Zeit, ":00"))
time

#Datum von allen Events
site %>% 
  html_nodes(".date") %>%
  html_text(trim = T)-> alldates
alldates <- as.POSIXct(alldates,format="%d.%m.%Y")
alldates

# Veranstalter, Eventende, city
length <- length(title)
c(rep(NA, length)) -> Eventende
c(rep("Pfarrgemeinschaft Heiligkereuz und St. Elisabeth",length)) -> veranstalter
c(rep("Wuerzburg", length)) -> stadt


#Dataframe
df1 <- data.frame (Veranstalter = veranstalter, Title = title, Uhrzeit = time, Datum = alldates, Beschreibung = beschreibung, url = link)
df2 <- left_join(df1, dfOrtDatum, by = "Datum")
df2

#Adresse

# Adresse Event 1 ---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/meditativer-tanz/59edf604-a66f-4975-b07a-d8df3d3f1c94?mode=detail"
url %>%
  read_html() %>%
  html_nodes("tr:nth-child(3) .info_value") %>%
  html_text(trim = T)-> street1 

url %>%
  read_html() %>%
  html_nodes("tr:nth-child(4) .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip1

ad1 <- paste(street1, zip1, sep=" ")
ad1

# Adresse Event 3 ---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/erstes-treffen-fuer-firmlinge/b0714d7f-d4ad-4fcd-a19f-0d2ef9fdc925?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(12) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street3 
street3

url %>%
  read_html() %>%
  html_nodes("tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip3
zip3
ad3 <- paste(street3, zip3, sep=" ")
ad3


# Adresse Event 4 ---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/familiengottedienst-u-parallel-kinderkirche-anschl-kirchenkaffee/700165be-5e2c-4200-aa65-a9b5d6006ef7?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(10) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street4 

url %>%
  read_html() %>%
  html_nodes("tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip4

ad4 <- paste(street4, zip4, sep=" ")
ad4

# Adresse Event 5 ---------------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/pgr-vorstandssitzung/f38796bd-b2e6-4ab4-ad1c-dede32e7d9ce?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(10) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street5


url %>%
  read_html() %>%
  html_nodes("tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip5

ad5 <- paste(street5, zip5, sep=" ")
# Adresse Event 6 ---------------------------------------------------------


url <- "http://www.kirche-zellerau.de/veranstaltungen/ewige-anbetung----heiligkreuz/37b4b044-2857-4122-9258-be5408dc4760?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(10) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street6


url %>%
  read_html() %>%
  html_nodes("tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip6
ad6 <- paste(street6, zip6, sep=" ")

# Adresse Event 7---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/ewige-anbetung---st-elisabeth/e89d343f-83b7-4789-8ee6-8e6f3275b406?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(10) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street7
street7

url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(5) tr+ tr .info_value , tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip7
zip7
ad7 <- paste(street7, zip7, sep=" ")


# Adresse Event 8 ---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/pgr-sitzung/3a71a720-512b-4cb7-b157-0be9978375c0?mode=detail"
url %>%
  read_html() %>%
  html_nodes(".even-margin:nth-child(10) tr:nth-child(2) .info_value") %>%
  html_text(trim = T)-> street8


url %>%
  read_html() %>%
  html_nodes("tr~ tr+ tr .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip8
ad8 <- paste(street8, zip8, sep=" ")

# Adresse Event 9 ---------------------------------------------------------

url <- "http://www.kirche-zellerau.de/veranstaltungen/meditativer-tanz/b1f84a46-cfac-4cec-9f3c-a6bfc26469ab?mode=detail"
url %>%
  read_html() %>%
  html_nodes("tr:nth-child(3) .info_value") %>%
  html_text(trim = T)-> street9

url %>%
  read_html() %>%
  html_nodes("tr:nth-child(4) .info_value") %>%
  html_text(trim = T)-> zip 
stringr::str_extract_all(zip, "\\d+") -> zip
unlist(zip) -> zip9
ad9 <- paste(street9, zip9, sep=" ")

adresse <- c(ad1,NA, ad3, ad4, ad5, ad6, ad7, ad8, ad9, NA)
adresse
street <- c(street1,NA, street3, street4, street5, street6, street7, street8, street9, NA)
zip <- c(zip1, NA, zip3, zip4, zip5, zip6, zip7, zip8, zip9, NA)



#Long Lat
addressToGeoLoc <- function(address)
{
  address = str_replace_all(address, " ", "")
  gurl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",address,"&key=AIzaSyAx0fpXEbPEYrKIQfv3so2TFU96-tUcaww")
  print(gurl)
  req <- fromJSON(gurl)
  
  if (req$status == "OK") {
    print("OK")
    location = req$results[[1]]$geometry$`location`
    lat = location[[1]]
    lon = location[[2]]
    #print(typeof(lat))
    #print(lon)
    print(location)
  } else {
    location <- NA
  }
  Sys.sleep(0.3)
  return(location)
}
adresse
#d <- adresse[!is.na(adresse)]
#adresse
data <- lapply(adresse, addressToGeoLoc)
data
unlist(data) -> data

data
stringr::str_extract_all(data, "[-]?\\d+[.]\\d+") -> data
unlist(data) -> data
data

lat <- c(data[1], NA, data[5], data[7], data[9], data[11], data[13], data[15], data[17], NA)
long <- c(data[2], NA, data[6], data[8], data[10], data[12], data[14], data[16], data[18],  NA)

lat
long


#Finaler Dataframe
df <- data.frame(title = df2$Title, url = link, description= df2$Beschreibung, lng= long, lat= lat, city= stadt, street= street, zip= zip, date_start= df2$Datum, date_end= df2$Datum, time_start= time, time_end= Eventende, price= Eventende, organizer= df2$Veranstalter)



# Shut down selenium
remDr$close()
rm(rD)
gc()