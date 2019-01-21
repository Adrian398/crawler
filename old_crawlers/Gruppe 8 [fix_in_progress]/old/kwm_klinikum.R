library(rvest)
library(tidyverse)
library(httr)
library(RJSONIO)
library(chron)
source("geocode_tool.R")



crawl_kwm_klinikum <- function()
{
  overviewurl <- "http://www.kwm-klinikum.de/veranstaltungen/?page=1"
  
  getMaxPages <- function(overviewurl)
  {
    code <- read_html(overviewurl)
    linkPart <- html_node(code, "body > div.container > div > div.paging > ul > li:nth-child(7) > a")
    href <- html_attr(linkPart, "href")
    maxP = str_extract(href, "[0-9]+")
    return(as.numeric(maxP))
  }
  
  maxP <- getMaxPages(overviewurl)
  pageNumbers <- seq(from = 1, to = maxP, by = 1)
  
  getOverviewPage <- function(page)
  {
    base = "http://www.kwm-klinikum.de/veranstaltungen/?page="
    url <- paste0(base, page)
    return (url)
  }
  
  overViewURLs <- map(pageNumbers, getOverviewPage)
  
  getEventsPerEventpage <- function(url)
  {
    code <- read_html(url)
    eventLinks <- html_nodes(code, ".title a")
    #print(length(eventLinks))
    return (eventLinks)
  }
  
  eventLinks <- map(overViewURLs, getEventsPerEventpage)
  eventLinksListEndings <- c()
  counter <- 1
  for (i in 1:length(eventLinks))
  {
    for (a in 1:length(eventLinks[[i]]))
    {
      #print(a)
      href <- html_attr(eventLinks[[i]][a], "href")
      if(href != "")
      {
        eventLinksListEndings[counter] <- href
        counter = counter + 1
      }
    }
  }
  
  
  createFullEventLink <- function(ending)
  {
    base <- "http://www.kwm-klinikum.de/"
    fullurl <- paste0(base, ending)
    return(fullurl)
  }
  
  fullEventURLs <- map(eventLinksListEndings, createFullEventLink)
  #print(typeof(fullEventURLs))
  
  dimn= list("Eventname", "Description", "Organizer", "Place", "EventStart", "EventEnd")
  
  maindfklinikum = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                              date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
  
  getEventInformation <- function(eventurl)
  {
    print(eventurl)
    response <- httr::GET(eventurl)
    code <- httr::content(response)
    stat_code<- httr::status_code(response)
    
    if (stat_code == 200)
    {
      #code <- read_html(eventurl)
      
      titleArea <- html_node(code, xpath="/html/body/div[1]/div/article/h2")
      
      sideLoaded <- FALSE
      
      if (length(titleArea) > 0)
      {
        sideLoaded <- TRUE
      }
      if (sideLoaded == TRUE)
      {
        infoBox <- html_nodes(code, "#content")[1]
        whenAreaType <- html_nodes(infoBox, "p:nth-child(4)")
        whenArea <- NULL
        whereArea <- NULL
        
        
        pAreas <- html_nodes(code, xpath = "//*[@id='content']/p")
        
        for(i in 1: length(pAreas))
        {
          #print(pAreas[i])
          if (grepl("Wann:", pAreas[i], fixed = TRUE) == TRUE)
          {
            whenArea <- pAreas[i]
          }
          
          else if (grepl("Wo:", pAreas[i], fixed = TRUE) == TRUE)
          {
            whereArea <- pAreas[i]
          }
          
        }
        #print(whereArea)
        dateText <- html_text(whenArea)
        
        datY4 <- str_extract(dateText, "\\d+\\.\\d+\\.\\d{4}")
        dat <- datY4
        datY2 <- str_extract(dateText, "\\d+\\.\\d+\\.\\d{2}")
        dateFormatStr = "%d.%m.%Y %H:%M"
        
        if (is.na(datY4) & !is.na(datY2))
        {
          dat <- datY2
          dateFormatStr = "%d.%m.%y %H:%M"
        }
        
        
        times <- str_extract_all(dateText, "(?<= )\\d{1,2}[:.]\\d{2}(?= )")
        startTime <- str_replace(times[[1]][1], "\\.", ":")
        
        endTime <- ""
        if (length(times[[1]]) >= 2)
        {
          endTime <- str_replace(times[[1]][2], "\\.", ":")
        }
        
        #description <- html_node(code, xpath = "//*[@id='content']/div[1]")
        description <- html_node(code, ".rowall")
        #//*[@id="content"]
        desc_text <- html_text(description)
        
        dtStart <- paste(dat, startTime)
        dtEnd <- paste(dat, endTime)
        dtStart <- as.POSIXct(dtStart, format=dateFormatStr, tz = "")
        dtEnd <- as.POSIXct(dtEnd, format=dateFormatStr, tz = "")
        startTime <- strftime(dtStart, "%H:%M:%S")
        startTime <- times(startTime)
        endTime <- strftime(dtEnd, "%H:%M:%S")
        endTime <- times(endTime)
        
        startDate <- as.Date(dtStart)
        endDate <- as.Date(dtEnd)
        titleDirty <- html_text(titleArea)
        titleclean <- str_extract(titleDirty, "(?<=: ).*") # erfordert kein Postprocessing
        locDirty <- html_text(whereArea)
        locClean <- str_extract(locDirty, "(?<=Wo: ).*")
        
        
        locCord <- addressToGeoLoc(locClean)
        lat <- 49.79464
        lon <- 9.95295
        if(!is.na(locCord))
        {
          lat <- locCord[[1]]
          lon <- locCord[[2]]
        }
        
        if(is.na(lat) | lat >= 52 | lat <= 46)
        {
          lat <- 49.79464
        }
        
        if (is.na(lon) | lon >= 12 | lon <= 6)
        {
          lon <- 9.95295
        }
        
        
        
        subdf = data.frame(title=titleclean, url = eventurl, description=desc_text, lng = lon, lat =lat, city=NA, street = NA, zip = NA, 
                           date_start=startDate, date_end=endDate , time_start=startTime, time_end = endTime, price=NA, organizer="Missionsaerztliche Klinik")
        return (subdf)
      }
      
    }
    
  }
  
  
  eventData <- map(fullEventURLs, getEventInformation)
  df1 <- eventData[[1]]
  
  
  for (i in 1: length(eventData))
  {
    maindfklinikum <- rbind(maindfklinikum, eventData[[i]])
  }
  
  View(maindfklinikum)
  return(maindfklinikum)
}

# for (i in 1:nrow(b))
# {
#   if ((is.na(b$city[i])) | (is.na(b$street[i])) | (is.na(b$zip[i])))
#   {
#     if ((!is.na(b$lat[i])) | (!is.na(b$lng[i])))
#     {
#       #print(b$lng[i])
#       address <- geoLocToAddress(b$lat[i], b$lng[i])
# 
#       if (!is.na(address))
#       {
#         b$street[i] <- address[1]
#         b$zip[i] <- address[2]
#         b$city[i] <- address[3]
#       }
#     }
#     
#   }
# }
