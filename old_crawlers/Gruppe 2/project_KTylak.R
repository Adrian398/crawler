#### GETTING DATA ####

# load libraries
library(tidyverse)
library(RCurl)
library(RJSONIO)
library(readxl)
library(rvest)
library(data.table)
library(devtools)
library(RSelenium)
library(chron)  # change times
library(feather)

# # Setting up RSelenium
# install.packages("devtools")
# library(devtools)
# install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
# install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
# install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
# library(RSelenium)

# URLS List Data Set
List <- read_excel("Location-Group-Assignments.xlsx")

#### 1. Uni Hubland ####

urls_set <- paste('https://www.uni-wuerzburg.de/sonstiges/veranstaltungskalender/zeitraum/2018/',6:12,'/', sep ="")     

 ## ** RSelenium - activate
rD <- rsDriver()
remDr <- rD[["client"]]

 # Get Values
title_selector <- ".news-list__item-header a"
date_selector <- ".news-single__item-value"
ort_selector <- ".news-list__item-event-location"
details_selector <- ".news-list__item-value"

getData_sel <- function(url) {
  
  remDr$navigate(url)
  site <- read_html(remDr$getPageSource()[[1]])
  # Get Nodes
  site %>%
    html_nodes(".news-list__item") -> node_data

  node_data %>%
    html_node(title_selector) %>%
    html_text() -> title
  
  node_data %>%
    html_node(date_selector) %>%
    html_text() -> date
  
  node_data %>%
    html_node(ort_selector) %>%
    html_text() -> ort
  
  node_data %>%
    html_node(details_selector) %>%
    html_text() -> details
  
  # Merge
  df <- data.frame(title = title,
                   url = url,
                   Details = details,
                   Ort = ort,
                   Date = date)
                   
  # Clean up
  df %>%
    filter(!is.na(title)) -> df_clean
}

events_list <- map(urls_set,getData_sel)
 # Shut down selenium
remDr$close()
rm(rD)
gc()

 ## Organize dataframe
events_df <- rbindlist(events_list)
events_df$organizer <- List$Titel[35]
events_df$lng <- List$Lon[35]
events_df$lat <- List$Lat[35]
events_df$street <- List$`Strasse, Hausnummer`[35]

events_df$Ort <- gsub("Ort:", "", events_df$Ort)
events_df$date_start <- str_extract( events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
events_df$time_start <- str_extract(events_df$Date, "[0-9]{1,2}[\\:][0-9]{1,2}")
events_df$time_end <- str_extract_all(events_df$Date, "(?<=-.{1,15})[0-9]{1,2}[\\:][0-9]{1,2}")

uni_events <- data.frame(title = events_df$title, url = events_df$url, description = paste(events_df$Ort,", ",events_df$Details, sep=" "), lng = events_df$lng,
                         lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97074, date_start = events_df$date_start, date_end = events_df$date_end,
                         time_start = events_df$time_start, time_end = as.character(events_df$time_end), price = NA, organizer = events_df$organizer)

  ## Change dataformat
  # date
uni_events$date_start <- as.Date(uni_events$date_start, "%d.%m.%Y")
uni_events$date_end   <- as.Date(uni_events$date_start, "%d.%m.%Y")
  # time
t <- as.POSIXct(uni_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
uni_events$time_start <-times(format(t, "%H:%M:%S"))
t <- as.POSIXct(uni_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
uni_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
uni_events[] <- lapply(uni_events, function(x) if (is.factor(x)) as.character(x) else {x})
str(uni_events)

rm(events_df,events_list, date_selector,details_selector,ort_selector,title_selector,t, urls_set)

#### 4 Uni Bib Hubland ####

urls_set <- paste('https://www.bibliothek.uni-wuerzburg.de/ueber-uns/veranstaltungen/veranstaltungskalender/zeitraum/2018/',c(6:9,11),'/', sep ="")  

 ## ** RSelenium - activate
rD <- rsDriver()
remDr <- rD[["client"]]

 # Get Values
title_selector <- ".news-list__item-header a"
date_selector <- ".news-single__item-value"
ort_selector <- ".news-list__item-event-location"
details_selector <- ".news-list__item-value"

events_list <- map(urls_set,getData_sel)
 # Shut down selenium
remDr$close()
rm(rD)
gc()

 ## Organize dataframe
events_df <- rbindlist(events_list)
events_df$organizer <- List$Titel[37]
events_df$lng <- List$Lon[37]
events_df$lat <- List$Lat[37]
events_df$street <- List$`Strasse, Hausnummer`[37]

events_df$Ort <- gsub("Ort:", "", events_df$Ort)
events_df$date_start <- str_extract( events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
events_df$time_start <- str_extract(events_df$Date, "[0-9]{1,2}[\\:][0-9]{1,2}")
events_df$time_end <- str_extract_all(events_df$Date, "(?<=-.{1,15})[0-9]{1,2}[\\:][0-9]{1,2}")

bib_events <- data.frame(title = events_df$title, url = events_df$url, description = events_df$Details, lng = events_df$lng,
                         lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97074, date_start = events_df$date_start, date_end = events_df$date_end,
                         time_start = events_df$time_start, time_end = as.character(events_df$time_end), price = NA, organizer = events_df$organizer)

 ## Change dataformat
  # date
bib_events$date_start <- as.Date(bib_events$date_start, "%d.%m.%Y")
bib_events$date_end   <- as.Date(bib_events$date_start, "%d.%m.%Y")
  # time
t <- as.POSIXct(bib_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
bib_events$time_start <-times(format(t, "%H:%M:%S"))
t <- as.POSIXct(bib_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
bib_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
bib_events[] <- lapply(bib_events, function(x) if (is.factor(x)) as.character(x) else {x})
str(bib_events)

rm(events_df,events_list, date_selector, details_selector,ort_selector, title_selector, t, urls_set)

### 5 Fraunhofer Institut für Silicatforschung ISC

url <- "https://www.isc.fraunhofer.de/de/messen-und-termine/messen-und-termine-2018.html?q=w%C3%BCrzburg#search-events"

getEvents <- function(url) {
  url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes(".internal") %>%
    html_text()  -> title
  
  raw_data %>%
    html_nodes(".place") %>%
    html_text()  -> ort
  
  raw_data %>%
    html_nodes(".date") %>%
    html_text()  -> datum
  
  df <- data.frame(title=title, Ort=ort, Date=datum)
}

events_df <- getEvents(url)

 ## Organize dataframe
events_df$organizer <- List$Titel[39]
events_df$lng <- List$Lon[39]
events_df$lat <- List$Lat[39]
events_df$street <- List$`Strasse, Hausnummer`[39]

events_df$date_start<- str_extract(events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
events_df$time_start <- str_extract(events_df$Date, "(?<=,.{1,15})[0-9]{1}")
events_df$time_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\ ]")

 # get links of each event - url column
pg <- read_html(url)
links <- html_attr(html_nodes(pg, "a"), "href")
links <- as.vector(links)
links <- links[grep("^/de/messen-und-termine/messen-und-termine-2018/.*", links)]
links <- unique(links)
url_list <- data.frame(paste('https://www.isc.fraunhofer.de/',links,sep=""))
colnames(url_list) <- c("url")


Frau_Ins_events <- data.frame(title = events_df$title, url = url_list, description = NA, lng = events_df$lng,
                              lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97082, date_start = events_df$date_start, date_end = events_df$date_end,
                              time_start = NA, time_end =NA, price = NA, organizer = events_df$organizer)

 ## Change dataformat
  # date
Frau_Ins_events$date_start <- as.Date(Frau_Ins_events$date_start, "%d.%m.%Y")
Frau_Ins_events$date_end   <- as.Date(Frau_Ins_events$date_start, "%d.%m.%Y")
  # time
t <- as.POSIXct(Frau_Ins_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
Frau_Ins_events$time_start <-times(format(t, "%H:%M:%S"))
t <- as.POSIXct(Frau_Ins_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
Frau_Ins_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
Frau_Ins_events[] <- lapply(Frau_Ins_events, function(x) if (is.factor(x)) as.character(x) else {x})
str(Frau_Ins_events)

rm(events_df, pg, url_list, links,t, time_start, time_end, url)

  ## **6. St. Lioba ** ##
url <- 'http://www.kirche-lengfeld.de/blog---aktuelles/veranstaltungen'

url %>%
  read_html() -> raw_data

raw_data %>%
  html_nodes(".itemtitle a") %>%
  html_text()  -> title


raw_data %>%
  html_nodes(".datetime , .date") %>%
  html_text()  -> datum    

 
events_df <- data.frame(title = title,Date= datum,  Ort="St. Lioba")

 ## Organize dataframe
events_df$organizer <- List$Titel[40]
events_df$lng <- List$Lon[40]
events_df$lat <- List$Lat[40]
events_df$street <- List$`Strasse, Hausnummer`[40]

events_df$date_start<- str_extract( events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
events_df$time_start <- str_extract(events_df$Date, "(?<=,.{1,15})[0-9]{1,2}[\\:][0-9]{1,2}")
events_df$time_end <- str_extract(events_df$Date, "(?<=bis)[0-9]{1,2}:[0-9]{1,2}")

StLioba_events <- data.frame(title = events_df$title, url = "http://www.kirche-lengfeld.de/blog---aktuelles/veranstaltungen", description = NA, lng = events_df$lng,
                             lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97074, date_start = events_df$date_start, date_end = events_df$date_end,
                             time_start = events_df$time_start, time_end = as.character(events_df$time_end), price = NA, organizer = events_df$organizer)


 ## Change dataformat
  # date
StLioba_events$date_start <- as.Date(StLioba_events$date_start, "%d.%m.%Y")
StLioba_events$date_end   <- as.Date(StLioba_events$date_start, "%d.%m.%Y")
  # time
t <- as.POSIXct(StLioba_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
StLioba_events$time_start <-times(format(t, "%H:%M:%S"))
t <- as.POSIXct(StLioba_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
StLioba_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
StLioba_events[] <- lapply(StLioba_events, function(x) if (is.factor(x)) as.character(x) else {x})
str(StLioba_events)

rm(events_df, title,t, datum, raw_data, url, remDr)

### **Create one Data Frame with Events 
events_all <- rbind(if(exists("uni_events")) uni_events,
                     if(exists("bib_events")) bib_events,
                     if(exists("Frau_Ins_events")) Frau_Ins_events,
                     if(exists("StLioba_events")) StLioba_events )
                     
                    


### Data Frame check and clean
events_all$time_start <- times(events_all$time_start)
events_all$time_end <- times(events_all$time_end)
events_all$date_start <- as.Date(events_all$date_start)
events_all$date_end <- as.Date(events_all$date_end)
events_all$lng <- as.numeric(events_all$lng)
events_all$lat <- as.numeric(events_all$lat)
events_all$zip <- as.numeric(events_all$zip)

#Set real Coords
events_all$title <- str_replace_all(events_all$title, c("<c4>" = "Ae", "<d6>" = "Oe", "<dc>" = "Ue",
                                        "<e4>" = "ae", "<f6>" = "oe", "<fc>" = "ue",
                                        "<df>" = "ss", "<f9>" = "u"))
events_all$city <- str_replace_all(events_all$city, c("<c4>" = "Ae", "<d6>" = "Oe", "<dc>" = "Ue",
                                      "<e4>" = "ae", "<f6>" = "oe", "<fc>" = "ue",
                                      "<df>" = "ss", "<f9>" = "u"))
events_all$description <- str_replace_all(events_all$description, c("<c4>" = "Ae", "<d6>" = "Oe", "<dc>" = "Ue",
                                                    "<e4>" = "ae", "<f6>" = "oe", "<fc>" = "ue",
                                                    "<df>" = "ss", "<f9>" = "u"))

# write_csv(events_all, "events-all_KTylak.csv")
