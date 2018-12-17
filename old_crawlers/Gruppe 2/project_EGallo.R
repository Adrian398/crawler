library(RCurl)
library(RJSONIO)
library(tidyverse)
require(ggmap)
require(leaflet)
require(rvest)
library(dplyr)
library(lubridate)
library(selectr)
library(stringr)
library(tidyr)
library(zoo)
library(chron)
library(RSelenium)
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
getwd()
liste <- read.csv("Location-Group-Assignments.csv", header = T, sep = ";")


# St Josef Grombuehl FERTIG####
  #subseite Veranstaltungen ist leer

stj_url <- "http://www.st-josef-grombuehl.de/"
stj_url %>%
  read_html() %>%
  html_nodes(".subBlockTitle , h2 a") %>%
  html_text(trim = T) -> stj
head(stj)
stj_df <- data.frame(stj)
head(stj_df)

# St Stephan   FERTIG ##############
  #Seite 7: 2 days 1 time rows dont match
sts_get <- function(url){
  url %>%
  read_html() %>%
  html_nodes("#et_content_container div") %>% #Wähle gesamtes Ereignis
  map_df(~list(date = html_nodes(.x, ".et_content_date") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
               title = html_nodes(.x, ".et_link_title") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .},
               city = html_nodes(.x, ".et_city") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .},
               description = html_nodes(.x, ".et_placename") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .})) -> df
df_clean <- df[rowSums(is.na(df)) < 3,] #keep rows with < 2 NAs
}
sts_urls <- paste0("http://wuerzburg-ststephan.de/gemeindeleben/?pageID=", 
                   1:11, "&until=yes")
map_df(sts_urls, sts_get) -> sts_df

 #Clean Data
sts_df$date <- str_remove_all(sts_df$date, "[A-z]|ä|,")
sts_df$date_start <- str_extract(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
  sts_df$date <- str_remove(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
sts_df$date_end <- str_extract_all(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
  sts_df$date_end[sts_df$date_end == "character(0)"] <- NA
sts_df$time_start <- str_extract(sts_df$date, "[0-9]{1,2}(:[0-9]{1,2})?")
sts_df$time_end <- str_extract(sts_df$date, "(?<=-)[0-9]{1,2}(:[0-9]{1,2})?")
filter(sts_df, !is.na(date_start)) -> sts_df
sts_df_clean <- subset(sts_df, select = -c(date))


  #formate dates (umwandeln und jahr anpassen)
sysyear <- year(Sys.Date())
sts_df_clean$date_start <- as.Date(sts_df_clean$date_start, format = "%d.%m.")
year(sts_df_clean$date_start) <- ifelse(month(sts_df_clean$date_start) < month(Sys.Date()), 
       year(sts_df_clean$date_start) <- sysyear+1,
       year(sts_df_clean$date_start) <- sysyear) 
sts_df_clean$date_end <- unlist(sts_df_clean$date_end)
sts_df_clean$date_end <- as.Date(sts_df_clean$date_end, format = "%d.%m.")
year(sts_df_clean$date_end) <- ifelse(month(sts_df_clean$date_end) < month(Sys.Date()), 
                                          year(sts_df_clean$date_end) <- sysyear+1,
                                          year(sts_df_clean$date_end) <- sysyear) 

sts_df_clean$time_start <- ifelse(is.na(str_extract(sts_df_clean$time_start, ":")),
                                      paste0(sts_df_clean$time_start, ":00"), 
                                      sts_df_clean$time_start)
sts_df_clean$time_start <- ifelse(is.na(str_extract(sts_df_clean$time_start, ".{2}:")),
                                      paste0("0", sts_df_clean$time_start),
                                      sts_df_clean$time_start)
sts_df_clean$time_start <- times(paste0(sts_df_clean$time_start, ":00"))
sts_df_clean$time_end <- ifelse(is.na(str_extract(sts_df_clean$time_end, ":")),
                                      paste0(sts_df_clean$time_end, ":00"), 
                                      sts_df_clean$time_end)
sts_df_clean$time_end <- ifelse(is.na(str_extract(sts_df_clean$time_end, ".{2}:")),
                                      paste0("0", sts_df_clean$time_end),
                                      sts_df_clean$time_end)
sts_df_clean$time_end <- times(paste0(sts_df_clean$time_end, ":00"))

  #erweitern mit liste
sts_df_final <- data.frame(sts_df_clean, organizer = liste$Veranstalter[28],
                           street = liste$Strasse..Hausnummer[28], zip = liste$PLZ[28], url = liste$Homepage[28],
                           lat = liste$Lat[28], lng = liste$Lon[28] ,stringsAsFactors=FALSE)
sts_df_final$organizer <- as.character(sts_df_final$organizer)
sts_df_final$street <- as.character(sts_df_final$street)
sts_df_final$zip <- as.numeric(sts_df_final$zip)
sts_df_final$url <- as.character(sts_df_final$url)
sts_df_final$lat <- as.numeric(levels(sts_df_final$lat))[sts_df_final$lat]
sts_df_final$lng <- as.numeric(levels(sts_df_final$lng))[sts_df_final$lng]
sts_df_final$price <- as.character(NA)

#  Hochschule für Musik FERTIG ####   
hfm_url <- "http://www.hfm-wuerzburg.de/veranstaltungen"


  #preparation
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(hfm_url)
  
  #click "mehr zeigen"
i <- 1
run <- T
while (run) {
  tryCatch(remDr$findElement(using = "css selector", ".more-link")$clickElement(),
  error = function(c){run <<- F},
  warning = function(w){run <<- F},
  finally = print(paste("Pressed buttom", i, "times"))
  )
  i <- i + 1
  Sys.sleep(5)
}

  #Get HTML, Nodes and Values
site_hfm <- read_html(remDr$getPageSource()[[1]])
site_hfm %>%
  html_nodes(".cntblk") %>% #Wähle gesamtes Ereignis
  map_df(~list(title = html_nodes(.x, "h2") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
               description = html_nodes(.x, ".alist-item-body") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .})) -> hfm_df 
remDr$close
rm(rD)
gc

  #clean data
hfm_df$date_start <-  str_extract_all(hfm_df$description, "[0-9]{1,2}[\\.] [a-zA-z]{1,9} (20[1-2]{1}[0-9]{1})?")
hfm_df$time_start <-  str_extract_all(hfm_df$description, "[0-9]{1,2}[\\:][0-9]{1,2} ")
hfm_df$description <- str_extract_all(hfm_df$description, "(?<=Uhr).*") 
hfm_df %>%
  filter(hfm_df$date_start != "character(0)") ->hfm_df_clean
hfm_df_clean$date_end <- as.Date(NA)
hfm_df_clean$time_end <- NA
hfm_df_clean$price <- as.character(NA)

  #formate dates (umwandeln und jahr anpassen)
hfm_df_clean$date_start <- str_replace_all(hfm_df_clean$date_start, c("Januar" = "01", "Februar" = "02", "März" = "03",
                                                                      "April" = "04","Mai" = "05","Juni" = "06",
                                                                      "Juli" = "07","August" = "08","September" = "09",
                                                                      "Oktober" = "10","November" = "11","Dezember" = "12"))
hfm_df_clean$date_start <- as.Date(hfm_df_clean$date_start, format = "%d. %m %Y")
hfm_df_clean$time_start <- times(paste0(hfm_df_clean$time_start, ":00"))
hfm_df_clean$time_end <- times(hfm_df_clean$time_end)
hfm_df_final <- data.frame(hfm_df_clean, organizer = liste$Veranstalter[29],
                           street = liste$Strasse..Hausnummer[29], zip = liste$PLZ[29], url = liste$Homepage[29],
                           lat = liste$Lat[29], lng = liste$Lon[29] ,stringsAsFactors=FALSE)
hfm_df_final$organizer <- as.character(hfm_df_final$organizer)
hfm_df_final$street <- as.character(hfm_df_final$street)
hfm_df_final$zip <- as.numeric(hfm_df_final$zip)
hfm_df_final$url <- as.character(hfm_df_final$url)
hfm_df_final$lat <- as.numeric(levels(hfm_df_final$lat))[hfm_df_final$lat]
hfm_df_final$lng <- as.numeric(levels(hfm_df_final$lng))[hfm_df_final$lng]
hfm_df_final$description <- unlist(hfm_df_final$description)
hfm_df_final$city <- "Wuerzburg"

# Mainfranken Theater FERTIG ####
  #Erprobe Regex
mft_url <- "https://www.theaterwuerzburg.de/index.php?option=com_mftplayground&view=repertoires&Itemid=116"
mft_url %>%
  read_html() %>%
  html_nodes("#spielplan")  %>%
  map_df(~list(Event = html_nodes(.x, ".repdate") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .},
               title = html_nodes(.x, ".title") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .})) ->mft_df
  #Clean and edit data 
mft_df$date_start <- str_extract(mft_df$Event, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.]")
  mft_df$date_start[mft_df$date_start == "character(0)"] <- NA
  mft_df$date_start <- na.locf(mft_df$date_start) #fill up missings with values from above
  mft_df$Event <- gsub("[A-Za-z]{1,2} \\|{1} [0-9]{1,2}[\\.][0-9]{1,2}[\\.]", "", mft_df$Event) #remove Mo und date
mft_df$Ort <- str_extract_all(mft_df$Event, "^[a-zA-ZßÄäÖöÜü[:punct:][:space:]]{1,50}")
mft_df$Einfuehrung <- str_extract_all(mft_df$Event, "(?<=\\|{1}) (EF:){1} [0-9]{1,2}[\\.]{1}[0-9]{1,2}") #?<= suche aber matche nicht
  mft_df$Einfuehrung[mft_df$Einfuehrung == "character(0)"] <- NA
  mft_df$Event <- gsub("\\|{1} (EF:){1} [0-9]{1,2}[\\.]{1}[0-9]{1,2}", "", mft_df$Event)
mft_df$Uhrzeit <- str_extract_all(mft_df$Event, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}(.*[0-9]{1,2}[\\.]{1}[0-9]{1,2})?")
  mft_df$time_start <- str_extract(mft_df$Uhrzeit, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}")
  mft_df$time_end <- str_extract(mft_df$Uhrzeit, "(?<=ca\\. )[0-9]{1,2}[\\.]{1}[0-9]{1,2}")
mft_df$description <- str_extract_all(mft_df$Event, "(?<=(.{0,100}\\|){2,3}).{0,100}") #?<= bracht finite Anzahl an WErten kein * etc
mft_df <- unite(mft_df, description, c(Einfuehrung, Ort, description), remove = T)
mft_df$description <- str_remove_all(mft_df$description, "NA_")
mft_df_clean <- subset(mft_df, select = -c(Event, Uhrzeit))
mft_df_clean$date_end <- NA

  #Formatierung
sysyear <- year(Sys.Date())
mft_df_clean$date_start <- as.Date(mft_df_clean$date_start, format = "%d.%m.")
year(mft_df_clean$date_start) <- ifelse(month(mft_df_clean$date_start) < month(Sys.Date()), 
                                        year(mft_df_clean$date_start) <- sysyear+1,
                                        year(mft_df_clean$date_start) <- sysyear) 
mft_df_clean$date_end <- as.Date(mft_df_clean$date_end)
mft_df_clean$time_start %>%
  str_replace_all("\\.", ":") %>%
  paste0(":00") %>%
  times() -> mft_df_clean$time_start
mft_df_clean$time_end %>%str_replace_all("\\.", ":") %>%
  paste0(":00") %>%
  times() -> mft_df_clean$time_end


  #erweitern mit liste

mft_df_final <- data.frame(mft_df_clean, organizer = liste$Veranstalter[30],
                           street = liste$Strasse..Hausnummer[30], zip = liste$PLZ[30], url = liste$Homepage[30],
                           lat = liste$Lat[30], lng = liste$Lon[30] ,stringsAsFactors=FALSE)
mft_df_final$organizer <- as.character(mft_df_final$organizer)
mft_df_final$street <- as.character(mft_df_final$street)
mft_df_final$zip <- as.numeric(mft_df_final$zip)
mft_df_final$url <- as.character(mft_df_final$url)
mft_df_final$lat <- as.numeric(levels(mft_df_final$lat))[mft_df_final$lat]
mft_df_final$lng <- as.numeric(levels(mft_df_final$lng))[mft_df_final$lng]
mft_df_final$price <- as.character(NA) 
mft_df_final$city <- "Wuerzburg"

# SOliver Baskets FERTIG####
  #keine Info zu spielenden Teams nur Icons.
sol_url <- "https://www.soliver-wuerzburg.de/saison/spielplan/"
sol_url %>%
  read_html() %>%
  html_nodes(".csc-table") %>% #Wähle gesamtes Ereignis
  map_df(~list(title = html_nodes(.x, "td.spieltag") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
               Datum = html_nodes(.x, "td.date") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .},
               Heim = html_nodes(.x, "td.team-home") %>%
                 str_extract(. , "(alt=\").*(\")") %>%
                 gsub("alt=", "", .) %>%
                 {if(length(.) == 0) NA else .},
               Gast = html_nodes(.x, "td.team-gast") %>%
                 str_extract(. , "(alt=\").*(\")") %>%
                 gsub("alt=", "", .) %>%
                 {if(length(.) == 0) NA else .},
               Details = html_nodes(.x, ".result") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}))  -> sol_df
sol_df$title <- paste0("Speiltag ", sol_df$title)
sol_df$date_start <- str_extract(sol_df$Datum, "[0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}")
sol_df$time_start <- str_extract(sol_df$Datum, "[0-9]{1,2}:[0-9]{1,2}")
sol_df <- unite(sol_df, description, c(Heim, Gast, Details), remove = T)
sol_df_clean <- subset(sol_df, select = -c(Datum))
sol_df_clean$date_end <- as.Date(NA)
sol_df_clean$time_end <- times(NA)

  #formatierung
sol_df_clean$date_start <- as.Date(sol_df_clean$date_start, format = "%d.%m.%Y")
sol_df_clean$time_start %>%
  paste0(":00") %>%
  times() -> sol_df_clean$time_start
sol_df_clean$time_end <- times(sol_df_clean$time_end)

sol_df_final <- data.frame(sol_df_clean, organizer = liste$Veranstalter[31],
                           street = liste$Strasse..Hausnummer[31], zip = liste$PLZ[31], url = liste$Homepage[31],
                           lat = liste$Lat[31], lng = liste$Lon[31] ,stringsAsFactors=FALSE)
sol_df_final$organizer <- as.character(sol_df_final$organizer)
sol_df_final$street <- as.character(sol_df_final$street)
sol_df_final$zip <- as.numeric(sol_df_final$zip)
sol_df_final$url <- as.character(sol_df_final$url)
sol_df_final$lat <- as.numeric(levels(sol_df_final$lat))[sol_df_final$lat]
sol_df_final$lng <- as.numeric(levels(sol_df_final$lng))[sol_df_final$lng]
sol_df_final$price <- as.character(NA)
sol_df_final$city <- "Wuerzburg"

# BBK FERTIG####
bbk_url <- "http://www.bbk-unterfranken.de/ausstellungen_bbk.html"
bbk_url %>%
  read_html() %>%
  html_nodes(".box-info") %>% #Wähle gesamtes Ereignis
  map_df(~list(description = html_nodes(.x, ".blau") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
               title = html_nodes(.x, ".title") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .},
               Datum = html_nodes(.x, ".event-date") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}))  -> bbk_df
  #edit data
bbk_df$Datum <- str_remove_all(bbk_df$Datum, "[A-z]")
bbk_df$date_start <- unlist(str_extract_all(bbk_df$Datum, "^.{10}"))
bbk_df$date_end <- gsub(".* -*", "", bbk_df$Datum)  
bbk_df_clean <- subset(bbk_df, select = -c(Datum))
bbk_df_clean$time_start <- NA
bbk_df_clean$time_end <- NA


  #Formatierung
bbk_df_clean$date_start <- as.Date(bbk_df_clean$date_start, format = "%d.%m.%y")
bbk_df_clean$date_end <- as.Date(bbk_df_clean$date_end, format = "%d.%m.%y")
bbk_df_clean$time_start <- times(bbk_df_clean$time_start)
bbk_df_clean$time_end <- times(bbk_df_clean$time_end)


bbk_df_final <- data.frame(bbk_df_clean, organizer = liste$Veranstalter[32],
                           street = liste$Strasse..Hausnummer[32], zip = liste$PLZ[32], url = liste$Homepage[32],
                           lat = liste$Lat[32], lng = liste$Lon[32] ,stringsAsFactors=FALSE)
bbk_df_final$organizer <- as.character(bbk_df_final$organizer)
bbk_df_final$street <- as.character(bbk_df_final$street)
bbk_df_final$zip <- as.numeric(bbk_df_final$zip)
bbk_df_final$url <- as.character(bbk_df_final$url)
bbk_df_final$lat <- as.numeric(levels(bbk_df_final$lat))[bbk_df_final$lat]
bbk_df_final$lng <- as.numeric(levels(bbk_df_final$lng))[bbk_df_final$lng]
bbk_df_final$price <- as.character(NA)
bbk_df_final$city <- "Wuerzburg"

# KHG FERTIG####
khg_url <- "http://www.khg-wuerzburg.de/veranstaltungen/index.html#page1"

  #start browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(khg_url)

  #crawl first site
khg_url %>%
  read_html() %>%
  html_nodes(".listcontainer") %>% #Wähle gesamtes Ereignis
  map_df(~list(Termin = html_nodes(.x, ".datetime") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace length 0 elements with NA
               title = html_nodes(.x, ".itemtitle a") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}))  ->khg_df1
  #click button
try(remDr$findElement(using = "css selector", ".active")$clickElement())
Sys.sleep(5)
  #crawl 2nd site
site2 <- read_html(remDr$getPageSource()[[1]])
site2 %>%
  html_nodes(".listcontainer") %>% #Wähle gesamtes Ereignis
  map_df(~list(Termin = html_nodes(.x, ".datetime") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace length 0 elements with NA
               title = html_nodes(.x, ".itemtitle a") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}))  ->khg_df2
  #click button
try(remDr$findElement(using = "css selector", ".passive+ .active")$clickElement())
Sys.sleep(5)
  #crawl 3rd site
site3 <- read_html(remDr$getPageSource()[[1]])
site3 %>%
  html_nodes(".listcontainer") %>% #Wähle gesamtes Ereignis
  map_df(~list(Termin = html_nodes(.x, ".datetime") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}, #replace length 0 elements with NA
               title = html_nodes(.x, ".itemtitle a") %>%
                 html_text(trim = T) %>%
                 {if(length(.) == 0) NA else .}))  ->khg_df3
remDr$close
rm(rD)
gc
  #merge and edit data
khg_df <- rbind(khg_df1, khg_df2, khg_df3)
khg_df$Termin <- str_remove_all(khg_df$Termin, "Uhr")
khg_df$date_start <- str_remove_all(khg_df$Termin, ",.{1,100} | bis.{1,100}")
khg_df$time_start <- str_extract(khg_df$Termin, "[0-9]{1,2}:[0-9]{1,2}")
khg_df$date_end <- str_extract(khg_df$Termin, "(?<=bis )[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
khg_df$time_end <- str_extract(khg_df$Termin, "(?<=bis .{1,20},) [0-9]{1,2}:[0-9]{1,2}")
khg_df_clean <- subset(khg_df, select = -c(Termin))
khg_df_clean$city <- "Wuerzbrug"
khg_df_clean$description <- as.character(NA)

  #formatierung
khg_df_clean$date_start <- as.Date(khg_df_clean$date_start, format = "%d.%m.%Y")
khg_df_clean$date_end <- as.Date(khg_df_clean$date_end, format = "%d.%m.%Y")
khg_df_clean$time_start <- ifelse(is.na(str_extract(khg_df_clean$time_start, ".{2}:")),
                                  paste0("0", khg_df_clean$time_start),
                                  khg_df_clean$time_start)
khg_df_clean$time_start %>%
  paste0(":00") %>%
  times() -> khg_df_clean$time_start
khg_df_clean$time_end <- ifelse(is.na(str_extract(khg_df_clean$time_end, ".{2}:")),
                                  paste0("0", khg_df_clean$time_end),
                                khg_df_clean$time_end)
khg_df_clean$time_end %>%
  paste0(":00") %>%
  times() -> khg_df_clean$time_end

khg_df_final <- data.frame(khg_df_clean, organizer = liste$Veranstalter[33],
                           street = liste$Strasse..Hausnummer[33], zip = liste$PLZ[33], url = liste$Homepage[33],
                           lat = liste$Lat[33], lng = liste$Lon[33] ,stringsAsFactors=FALSE)
khg_df_final$organizer <- as.character(khg_df_final$organizer)
khg_df_final$street <- as.character(khg_df_final$street)
khg_df_final$zip <- as.numeric(khg_df_final$zip)
khg_df_final$url <- as.character(khg_df_final$url)
khg_df_final$lat <- as.numeric(levels(khg_df_final$lat))[khg_df_final$lat]
khg_df_final$lng <- as.numeric(levels(khg_df_final$lng))[khg_df_final$lng]
khg_df_final$price <- as.character(NA)

  #merge all dfs #############
df_complete <- rbind(if(exists("sts_df_final")) sts_df_final, 
                     if(exists("hfm_df_final")) hfm_df_final,
                     if(exists("mft_df_final")) mft_df_final, 
                     if(exists("sol_df_final")) sol_df_final,
                     if(exists("bbk_df_final")) bbk_df_final,
                     if(exists("khg_df_final")) khg_df_final)
#df_test <- df_complete

#df_test <- apply(df_test, 2, as.character)              
#write.csv2(df_test, "df_complete_E_Gallo.csv")
