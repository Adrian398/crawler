library(tidyverse)
library(rvest)
library(chron)

source("geocode_tool.R")

crawl_evdgh <- function()
{
  
  # maindfevdgh = data.frame(Eventname=character(), Eventbeschreibung=character(), Eventveranstalter=character(), Ort=character(),
  #                             Lon = numeric(), Lat =numeric(), Eventstart=as.POSIXct(character()),  
  #                             Eventende=as.POSIXct(character()) , Eventurl=character(), Eventkalenderurl=character(), Preis = numeric())
  
  maindfevdgh = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
             date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
  
  calenderUrl <- "http://evdhg.de/"
  code <- read_html(calenderUrl)
  infoBlock <- html_node(code, "#masthead .caption")
  infoBlock <- html_text(infoBlock, trim = TRUE)
  infoBlockLines <- str_split(infoBlock, "\n")
  
  #print(infoBlockLines)
  
  infoBlockLines <- replaceMonthNameWithNumber(unlist(infoBlockLines))
  
  for (i in 1:length(infoBlockLines))
  {
    if (grepl("[0-9]{1,2}.([ ]?)([0-9]{1,2}.?)", infoBlockLines[i]) == FALSE)
    {
      infoBlockLines <- infoBlockLines[-i]
    }
  }
  
  datesAndDesc <- str_split(infoBlockLines, ": ")
  
  for (i in 1:length(datesAndDesc))
  {
    datesAndDesc[[i]][1] <- str_remove_all(datesAndDesc[[i]][1], "\\s*")
    datesAndDesc[[i]][1] <- unlist(str_split(datesAndDesc[[i]][1], "-"))
    datesAndDesc[[i]][1] <- str_split(datesAndDesc[[i]][1], "bis")
    
    startdate <- datesAndDesc[[i]][[1]][[1]]
    enddate <- datesAndDesc[[i]][[1]][2]
    
    if (nchar(startdate) <= 6)
    {
      startdate <- paste(startdate, format(Sys.Date(), "%Y"), sep = "")
      startdate <- as.POSIXct(startdate, "%d.%m.%Y", tz ="")
    }
    
    if ((nchar(enddate) <= 6) & (!is.na(enddate))) 
    {
      enddate <- paste(enddate, format(Sys.Date(), "%Y"), sep = "")
      enddate <- as.POSIXct(enddate, "%d.%m.%Y", tz ="")
    }
    
    description <- datesAndDesc[[i]][[2]]
    
    startDate <- as.Date(startdate)
    endDate <- as.Date(enddate)
    
    # subdfevdgh = data.frame(Eventname=description, Eventbeschreibung=NA, Eventveranstalter="Dag-Hammarskjöld-Gymnasium Würzburg", Ort=NA,
    #                          Lon = 49.7801362, Lat =9.9647348, Eventstart=startdate,  
    #                          Eventende=enddate , Eventurl=calenderUrl, Eventkalenderurl=calenderUrl, Preis = NA)
    
    subdfevdgh = data.frame(title=description, url = calenderUrl, description=NA, lng = 9.9647348, lat =49.7801362, city="Würzburg", street = "Frauenlandplatz 5", zip = 97074, 
                             date_start=startDate, date_end=endDate , time_start=NA, time_end =NA, price=NA, organizer="Dag-Hammarskjöld-Gymnasium Würzburg")
    
    maindfevdgh <- rbind(maindfevdgh, subdfevdgh)
  }
  #View(maindfevdgh)
  return (maindfevdgh)
}

#crawl_evdgh()


