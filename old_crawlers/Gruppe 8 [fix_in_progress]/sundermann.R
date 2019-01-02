library(rvest)
library(tidyverse)

replaceMonthNameWithNumber <- function(strORList)
{
  strORList <- str_replace_all(strORList, "Januar", "01.")
  strORList <- str_replace_all(strORList, "Februar", "02.")
  strORList <- str_replace_all(strORList, "März", "03.")
  strORList <- str_replace_all(strORList, "April", "04.")
  strORList <- str_replace_all(strORList, "Mai", "05.")
  strORList <- str_replace_all(strORList, "Juni", "06.")
  strORList <- str_replace_all(strORList, "Juli", "07.")
  strORList <- str_replace_all(strORList, "August", "08.")
  strORList <- str_replace_all(strORList, "September", "09.")
  strORList <- str_replace_all(strORList, "Okober", "10.")
  strORList <- str_replace_all(strORList, "November", "11.")
  strORList <- str_replace_all(strORList, "Dezember", "12.")
  
  return (strORList)
}

crawl_sundermannkunst <- function()
{
  eventcalender <- "https://www.sundermann-kunst.de/Ausstellungen"
  code <- read_html(eventcalender)
  infobox <- html_node(code, "#content_main")
  #print(infobox)
  description <- html_nodes(infobox, "span")
  description <- html_text(description)
  description <- paste(description, collapse = " ")
  #print(description)
  eventname <- html_node(infobox, "h1")
  eventname <- html_text(eventname)
  
  dateArea <- html_node(infobox, "h3")
  dateArea <- html_text(dateArea)
  
  dateArea <- str_extract(dateArea, "(?<=(Ausstellung)).*")
  #dateArea <- str_remove_all(dateArea, " ")
  dateArea <- str_replace_all(dateArea, "[\\s]*", "")
  #print(dateArea)
  #dates <- strsplit(dateArea, "-")
  
  dates <- replaceMonthNameWithNumber(dateArea)
  
  s1 <- unlist(strsplit(dates, "-"))
  dates <- unlist(strsplit(s1, "bis"))
  
  
  if(nchar(dates[[1]]) < 6)
  {
    dates[[1]] <- paste("0", dates[[1]], sep = "")
  }
  if(nchar(dates[[2]]) < 10)
  {
    dates[[2]] <- paste("0", dates[[2]], sep = "")
  }
  
  if((nchar(dates[[1]]) == 6) & nchar(dates[[2]]) == 10)
  {
    year <- substr(dates[[2]], 7, 10)
    dates[[1]] <- paste(dates[[1]], year, sep="")
    
  }
  #print(dates)
  
  startdt <- as.POSIXct(dates[[1]], "%d.%m.%Y", tz="")
  startDate <- as.Date(startdt)
  enddt <- as.POSIXct(dates[[2]], "%d.%m.%Y", tz="")
  endDate <- as.Date(enddt)
  
  #print(typeof(enddt))
  
  # maindfsundermann = data.frame(Eventname=eventname, Eventbeschreibung=description, Eventveranstalter="Galerie Sundermann", Ort=NA,
  #                               Lon = 9.92982, Lat =49.78833, Eventstart=startdt,  
  #                               Eventende=enddt , Eventurl=eventcalender, Eventkalenderurl=eventcalender, Preis = NA)
  
  maindfsundermann = data.frame(title=eventname, url = eventcalender, description=description, lng = 9.92982, lat =49.78833, city="Würzburg", street = "Peterstraße 10", zip = 97070, 
                            date_start=startDate, date_end=endDate , time_start=NA, time_end = NA, price=NA, organizer=("Galerie Sundermann"))
  #View(maindfsundermann)
  return (maindfsundermann)
}

#crawl_sundermannkunst()

