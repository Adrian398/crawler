library(rvest)
library(tidyverse)
library(chron)

#print(Sys.timezone())

crawl_franz_obertheur_schule <- function()
{
  eventsurl = "https://www.franz-oberthuer-schule.de/events/event/"
  getMaxPages <- function(url, i=1)
  {
    code <- read_html(url)
    mainBlock <- html_node(code, xpath="//*[@id='primary']")
    
    morePages <- grepl("Spätere Termine", mainBlock, fixed = TRUE)
    
    if(morePages == TRUE)
    {
      i = i + 1
      url <- paste0("https://www.franz-oberthuer-schule.de/events/event/page/", i)
      print(url)
      getMaxPages(url, i)
    }
    
    return (i)
  }
  
  maxPages = getMaxPages(eventsurl)
  
  pages <- seq(1, maxPages, 1)
  print(pages)
  pageUrls <- paste0("https://www.franz-oberthuer-schule.de/events/event/page/", pages)
  print(pageUrls)
  
  
  getEvents <- function(url)
  {
    code <- read_html(url)
    mainBlock <- html_node(code, xpath="//*[@id='primary']")
    events <- html_nodes(code, ".hentry")
    
    startDates = c()
    
    # pagedfschule = data.frame(Eventname=character(), Eventbeschreibung=character(), Eventveranstalter=character(), Ort=character(),
    #                           Lon = numeric(), Lat =numeric(), Eventstart=as.POSIXct(character()),  
    #                           Eventende=as.POSIXct(character()) , Eventurl=character(), Eventkalenderurl=character(), Preis = numeric())
    
    pagedfschule = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                                date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
    
    for (i in 1:length(events))
    {
      print("-------------------")
      title_node <- html_node(events[i], ".entry-title span")
      title = html_text(title_node)
      print(title)
      dateRow <- html_node(events[i], "p")
      timeRowNode <- html_node(events[i], ".eo-event-date , time")
      timeTag <- html_nodes(timeRowNode, "time")
      dtValues <- html_attr(timeTag, "datetime")
      dtValues <- str_replace(dtValues, "\\+[\\d]{2}:[\\d]{2}", "")
      
      startTime <- NA
      endTime <- NA
      if (nchar(dtValues) >= 12)
      {
        #print("a")
        dateFormatStrFromSchool = "%Y-%m-%dT%H:%M:%S"
        dateFormatStr = "%d.%m.%Y %H:%M"
    
        dtValues <- as.POSIXct(dtValues, dateFormatStrFromSchool, tz="")
        
        startTime <- strftime(dtValues[[1]], "%H:%M:%S")
        print(startTime)
        startTime <- times(startTime)
        print(startTime)
        
        endTime <- strftime(dtValues[[2]], "%H:%M:%S")
        endTime <- times(endTime)
        
      }
      else if((nchar(dtValues) <= 11) & (nchar(dtValues) >= 9))
      {
        dateFormatStrFromSchool = "%Y-%m-%d"
        dateFormatStr = "%d.%m.%Y"
        dtValues <- as.POSIXct(dtValues, dateFormatStrFromSchool, tz="")
      }
      startDt <- dtValues[[1]]
      startDate <- as.Date(startDt)
      endDt <- dtValues[[2]]
      endDate <- as.Date(endDt)
      
      #print(typeof(startDt))
      #print(typeof(endDt))
      # 
      # subdfschule = data.frame(Eventname= c(title), Eventbeschreibung=c(NA), Eventveranstalter=c("Franz-Oberthuer-Schule"), Ort=c(NA),
      #                           Lon = c(9.95439), Lat =c(49.7865), Eventstart=c(startDt),  
      #                           Eventende= c(endDt), Eventurl=c(NA), Eventkalenderurl=c("https://www.franz-oberthuer-schule.de/events/event/"), Preis = c(NA))
      
      print(startTime)
      subdfschule = data.frame(title=title, url = eventsurl, description=NA, lng = 9.95439, lat =49.7865, city="Würzburg", street = "Zwerchgraben 2", zip = "97074", 
                                  date_start=startDate, date_end=endDate , time_start=times(startTime), time_end = times(endTime), price=NA, organizer="Franz-Oberthuer-Schule")
     
      pagedfschule <- rbind(pagedfschule, subdfschule)
      
    }

    #print("blub")
    #print(length(pagedfschule))
    return (pagedfschule)
  }
  
  
  maindfschule = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                            date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())

  allpageDFs <- map(pageUrls, getEvents)
  print(length(allpageDFs))
  
  for (i in 1:length(allpageDFs))
  {
    maindfschule <- rbind(maindfschule, allpageDFs[[i]])
  }
  #View(allpageDFs[[1]])
  #View(maindfschule)
  return (maindfschule)
}

#crawl_franz_obertheur_schule()



