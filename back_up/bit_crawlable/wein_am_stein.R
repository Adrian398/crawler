# test
library(plyr)
library(rvest)
library(tidyverse)
library(purrr)
library(gsubfn)
library(stringr)
library(chron)

url <- "http://weingut-am-stein.de/de/aktuelles/termine.html"

# ziehe Tag



url %>%
  read_html() %>%
  html_nodes(".date") %>%
  html_text() -> dates

# get start and end date
#get substr Right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

date_start <- c()
date_end <- c()
for (i in 1:length(dates)){
  splitdate = strsplit(dates[i], " - ")
  if(nchar(splitdate[[1]][1])==10){
    date_start = c(date_start,splitdate[[1]][1])
    date_end = c(date_end,splitdate[[1]][2])
    ####fixen
  }else{
    print("hello")
    date_start = c(date_start, paste0(splitdate[[1]][1], paste0(".",format(Sys.Date(), "%Y"))))
    date_end = c(date_end,splitdate[[1]][2])
  }
  
}
date_start
date_end

##TITLE
url %>%
  read_html() %>%
  html_nodes(".title") %>%
  html_text() -> title
sapply(title, tolower) -> title


##LOCATION
# ziehe zusätzliche Information über Location
url %>%
  read_html() %>%
  html_nodes(".location") %>%
  html_text() -> location
lapply(location, tolower) -> location
#sapply(location, function(x) {
 # if(x=="würzburg")
  #{x <- "wuerzburg"} 
  #else if (x=="münchen") {
    #x <- "muenchen"
  #} else 
  #  {x}
  #}) -> location
#lapply(location, function(x) paste0("Veranstaltung findet statt in: ",x)) -> location_info

#GEO Daten
lng <- as.numeric("9.92813")
lat <- as.numeric("49.80518")

#ADRESSE
#city <- "wuerzburg"
#street <- "mittlerer Steinbergerweg 5"
#zip <- 97080
organizer <- "Weingut am Stein"


# ziehe generelle Infos
url %>%
  read_html() %>%
  html_nodes("li:first-child") %>%
  html_text()-> infos

# erste Zeile nicht relevant
infos <- infos[-1]

# ziehe Zeit nach dem Format...
time <- strapply(infos, "\\d{1,2}\\.\\d{2}")
time <- as.data.frame(as.matrix(time))
time$V2 <- sapply(time$V1, function(x) if(length(x)>1) {x[2]})
time$V1 <- sapply(time$V1, function(x){if(length(x)>1) {x[1]} else {x}})
time_start <- time$V1
unlist(lapply(time_start, function(x) {if(is.null(x)) {x<-NA} else {x}})) -> time_start
gsub("\\.",":",time_start) -> time_start
times(paste0(time_start,":00")) -> time_start
unlist(lapply(time$V2 , function(x) {if(is.null(x)) {x<-NA} else {x}})) -> time_end
gsub("\\.",":",time_end) -> time_end
times(paste0(time_end,":00")) -> time_end

 
##PRICE
sapply(infos, function(x) str_extract_all(x, "([0-9]+,[0-9]+.(?=€))|(kostenfrei)|((?<=Eintritt:\\s)[0-9]+)")) -> price
gsub("character\\(0\\)",NA,price) -> price

##URL
link <- "http://weingut-am-stein.de/de/aktuelles/termine.html"

wst_df <- data.frame(title=title,url=link,description=infos,lng=lng,lat=lat,
                     city=city,street=street,zip=zip,date_start=dati_start,date_end=dati_end,
                     time_start=time_start,time_end=time_end,price=price,organizer=organizer)
rownames(wst_df) <- NULL


