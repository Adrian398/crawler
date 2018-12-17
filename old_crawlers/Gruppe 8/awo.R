library(rvest)
library(tidyverse)
library(chron)


crawl_awo_jw <- function()
{
  calenderUrl = "https://www.awo-jw.de/"
  # maindfawo <- data.frame(Eventname=character(), Eventbeschreibung=character(), Eventveranstalter=character(), Ort=character(),
  #                         lon = numeric(), Lat =numeric(), Eventstart=as.POSIXct(character()),  
  #                         Eventende=as.POSIXct(character()) , Eventurl=character(), Eventkalenderurl=character(), Preis=numeric())
  
  maindfawo <- data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                          date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
  
  getTermineUndNews <- function(calenderUrl)
  {
    code <- read_html(calenderUrl)
    mainNode <- html_node(code, '.clearfix td')
    tableNode <- html_node(mainNode, "table")
    #print(b)
    df <- html_table(tableNode, header = NA, trim = TRUE, fill = TRUE)
    #print(t)
    df <- df[ -c(3:5) ]
    
    mask <- !grepl("[a-zA-Z]+", df$X1)
    df <- subset(df, mask)
    df <- separate(df, X1, c("Start", "End"), "-")

    maskStartDate <- grepl("^[0-9]{2}.$", df$Start)
    #print(maskStartDate)

    for (i in 1: nrow(df))
    {
      if((nchar(df$Start[i]) <= 3) & (nchar(df$End[i]) >= 10))
      {
        monthAndYear <- str_extract(df$End[i], "[0-9]{2}\\.[0-9]{4}")
        startDate <- paste0(df$Start[i], monthAndYear)
        df$Start[i] <- startDate
      }
    }

    # df$Start <- as.POSIXct(df$Start, "%d.%m.%Y", tz="")
    # df$End <- as.POSIXct(df$End, "%d.%m.%Y", tz = "")
    df$Start <- as.Date(df$Start, "%d.%m.%Y")
    df$End <- as.Date(df$End, "%d.%m.%Y")

    # subdf <- data.frame(Eventname=df$X2, Eventbeschreibung=NA, Eventveranstalter="Jugendwerk der Arbeiterwohlfahrt Unterfranken e.V.", Ort=NA,
    #                     Lon = 9.94391, Lat =49.77505, Eventstart=df$Start,
    #                     Eventende=df$End, Eventurl=NA, Eventkalenderurl=calenderUrl, Preis = NA)
    
    
    subdf <- data.frame(title=df$X2, url = calenderUrl, description=NA, lng = 9.94391, lat =49.77505, city="Würzburg", street = "97074", zip = "97074",
                            date_start=df$Start, date_end=df$End , time_start=NA, time_end = NA, price=NA, organizer="Jugendwerk der Arbeiterwohlfahrt Unterfranken e.V.")
    
    #View(subdf)
    return (subdf)
  }
  
  termineundnews <- getTermineUndNews(calenderUrl)
  
  getOneVacationEvent <- function(eventUrl)
  {
    code <- read_html(eventUrl)
    allDivs <- html_nodes(code, xpath = "//div[@class='mui-vacation-box vacations-details']")

    detailBox <- NA
    for (i in 1:length(allDivs))
    {
      if(grepl("Details", allDivs[i], fixed = TRUE))
      {
        #print(allDivs[i])
        detailBox <- allDivs[i]
      }
    }

    listElements <- html_nodes(detailBox, "ul > li")
    
    dates <- str_extract_all(listElements, "[0-9]{2}.[0-9]{2}.[0-9]{4}")
    # startDate <- as.POSIXct(dates[[1]], "%d.%m.%Y", tz ="")
    # endDate <- as.POSIXct(dates[[2]], "%d.%m.%Y", tz = "")
    
    startDate <- as.Date(dates[[1]], "%d.%m.%Y")
    endDate <- as.Date(dates[[2]], "%d.%m.%Y")
    
    descBox <- html_node(code, ".mui-vacation-content")
    description <- html_text(descBox)
    eventname <- html_node(descBox, "h1")
    eventname <- html_text(eventname)
    #print(typeof(startDate))
    # subdfawo = data.frame(Eventname=eventname, Eventbeschreibung=description, Eventveranstalter="Jugendwerk der Arbeiterwohlfahrt Unterfranken e.V.", Ort=NA,
    #                             Lon = 9.94391, Lat =49.77505, Eventstart=startDate,
    #                             Eventende=endDate , Eventurl=eventUrl, Eventkalenderurl=calenderUrl, Preis = NA)
    
    # 
    subdfawo <- data.frame(title=eventname, url = calenderUrl, description=description, lng = 9.94391, lat =49.77505, city="Würzburg", street = "97074", zip = "97074",
                        date_start=startDate, date_end=endDate , time_start=NA, time_end = NA, price=NA, organizer="Jugendwerk der Arbeiterwohlfahrt Unterfranken e.V.")
    
    
    return (subdfawo)
  }
  
  #getOneVacationEvent("https://www.awo-jw.de/component/vacations/vacation/240")
  
  getVacationEvents <- function(calenderUrl)
  {
    code <- read_html(calenderUrl)
    
    vacContainer <- html_node(code, "#vacations-overview")
    vacNodes <- html_nodes(vacContainer, ".vacations-bar")
    urlTags <- html_nodes(vacNodes, "a")
    hrefs <- html_attr(urlTags, "href")
    
    baseUrl <- "https://www.awo-jw.de"
    
    eventUrls <- paste(baseUrl, hrefs, sep = "")
    print(eventUrls)
    
  }
  
  vacUrls <- getVacationEvents(calenderUrl)
  vacDFs <- map(vacUrls, getOneVacationEvent)
  maindfawo <- rbind(maindfawo, termineundnews)
  #maindfawo <- rbind(maindfawo, vacDFs)
  
  for (i in 1:length(vacDFs))
  {
    maindfawo <- rbind(maindfawo, vacDFs[[i]])
  }
  
  #View(maindfawo)
  return(maindfawo)
}

#crawl_awo_jw()