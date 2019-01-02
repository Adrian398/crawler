require(tidyverse)
require(rvest)
require(chron)

if(!exists("readLinks", mode="function")){ source("utils.R")}
if(!exists("addressToGeoLoc", mode="function")){source("geocode_tool.R")}

getEventData_kaeppele=function(rawData, url){
  ##get Title
  rawData%>%
    html_nodes(".title")%>%
    html_text()->sTitle
  ##get subtitle
  rawData%>%
    html_nodes(".subtitle")%>%
    html_text()->sSubTitle
  ##get date
  rawData%>%
    html_nodes(".datetime")%>%
    html_text()->sDateTime
  sDateTime=sDateTime[2]
  ##getDesc
  rawData%>%
    html_nodes(".footNotesParent")%>%
    html_text_collapse()->sDesc
  ##getLocation
##  rawData%>%
##    html_nodes(".even-margin")->aLocation
##  aLocation[3]%>%html_text()->sLocation
##  sLocation=gsub("\r|\n|\t", '', sLocation)
  lat=NA
  lon=NA
  rawData%>%html_nodes(".defaultTable")->aTables
  if (length(aTables)>=2){
    aTables[2]%>%html_nodes(".info_value")->aLocation
    if (length(aLocation)>=1 && str_length(html_text(aLocation[1]))>2){
      sLocation=html_text(aLocation[1])
      sLocation=paste0(sLocation,", W?rzburg")
      aCoordinates=addressToGeoLoc(sLocation)
      
      if (is.numeric(aCoordinates)){
        lat=aCoordinates[[1]]
        lon=aCoordinates[[2]]
      }
    }
    else if (length(aLocation)>=2){
      sLocation=html_text(aLocation[2])
     
      sLocation=paste0(sLocation,", W?rzburg")
      aCoordinates=addressToGeoLoc(sLocation)
      
      if (is.numeric(aCoordinates)){
        lat=aCoordinates[[1]]
        lon=aCoordinates[[2]]
      }
    }
  }


  
  
  
  sDate=str_extract(sDateTime,"\\d{2}\\.\\d{2}\\.\\d{4}")
  sTime=str_extract_all(sDateTime,"\\d{2}\\:\\d{2}",simplify=TRUE)
  
  oStartDate=as.Date(sDate,format = "%d.%m.%Y")
  oEndDate=NA
  oStartTime=NA
  oEndTime=NA
  if (length(sTime)>=1){
    oStartTime=times(paste0(sTime[1],":00"))
  }
  if (length(sTime)>=2){
    oEndDate=oStartDate
    oEndTime=times(paste0(sTime[2],":00"))
  }
 
  
  sDesc=gsub("^\r\n\t ","",sDesc)
  sDesc=gsub("\t","",sDesc)
  sDesc=gsub(" \r\n$","",sDesc)

  event=data.frame(title=sTitle,description=paste(sSubTitle,sDesc,sep=".\n"), organizer="K?ppele W?rzburg", city=NA, street=NA,zip=NA, lat=lat, lng=lon, date_start=oStartDate, date_end=oEndDate, url=url, time_start=oStartTime, time_end=oEndTime, price=NA)
  event$time_start=times(event$time_start)
  event$time_end=times(event$time_end)
  event$date_end=as.Date(event$date_end)

  return (event)
}
getEvents_kaeppele=function(){
  #readEventLinks
  url="http://www.kaeppele-wuerzburg.de/veranstaltungen"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".itemtitle a")->eventList
  eventList%>%
    html_attr("href")->aEventLinks
  aEventLinks
  oRes=readLinks(aEventLinks,getEventData_kaeppele,"http:")
  return (oRes)
}
