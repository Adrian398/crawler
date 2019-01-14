require(tidyverse)
require(rvest)
require(chron)
if(!exists("readLinks", mode="function")){ source("utils.R")}
if(!exists("addressToGeoLoc", mode="function")){source("geocode_tool.R")}

getEventData_stift=function(rawData, url){
  lat=NA
  lon=NA
  oStartDate=NA
  ##get Title
  rawData%>%
    html_nodes(".title")%>%
    html_text()->sTitle
  ##get date
  rawData%>%
    html_nodes(".datetime")%>%
    html_text()->sDateTime
  if (length(sDateTime>=2)){
    sDateTime=sDateTime[2]
    sDate=str_extract(sDateTime,"\\d{2}\\.\\d{2}\\.\\d{4}")
    sTime=str_extract(sDateTime,"\\d{2}\\:\\d{2}")
    
    oStartDate=as.Date(sDate,format="%d.%m.%Y")
    oStartTime=times(paste0(sTime,":00"))
    
  }

  ##getDesc
  rawData%>%
    html_nodes(".footNotesParent")%>%
    html_text_collapse()->sDesc
  ##getLocation
  rawData%>%
    html_nodes(".even-margin .info_value")->aLocation
  if (length(aLocation)>=4){
    aLocation[4]%>%html_text()->sLocation
    sLocation=gsub("\r|\n|\t", '', sLocation)
    sLocation=str_remove_all(sLocation, "in|im")
    if (is.na(str_match(sLocation,"W?rzburg"))){
      sLocation=paste0(sLocation ,", W?rzburg")
    }
    aCoordinates=addressToGeoLoc(sLocation)
 
    if (is.numeric(aCoordinates)){
      lat=aCoordinates[[1]]
      lon=aCoordinates[[2]]
    }
  }

  

  
  
  

  
  sDesc=gsub("^\r\n\t ","",sDesc)
  sDesc=gsub("\t","",sDesc)
  sDesc=gsub(" \r\n$","",sDesc)
  
  if(length(sDesc)==0){
    sDesc=NA
  }
  
  event=data.frame(title=sTitle,description=sDesc, organizer="Stift Haug W?rzburg", city=NA, street=NA,zip=NA, lat=lat, lng=lon, date_start=oStartDate, date_end=NA, url=url, time_start=oStartTime, time_end=NA, price=NA)
  event$time_start=times(event$time_start)
  
  return (event)
}
getEvents_stift=function(){
  #readEventLinks
 ## url="C:/Users/Jannis/Desktop/stiftHaug.html"
  url="http://www.stift-haug.de/aktuelle-termine"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".itemtitle a")->eventList
  eventList%>%
    html_attr("href")->aEventLinks
  aEventLinks
  oRes=readLinks(aEventLinks,getEventData_stift,"")
  return (oRes)
}
