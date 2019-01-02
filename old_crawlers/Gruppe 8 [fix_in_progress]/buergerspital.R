require(tidyverse)
require(rvest)
require(chron)

if(!exists("readLinks", mode="function")){ source("old_crawlers/Gruppe 8/utils.R")}
if(!exists("addressToGeoLoc", mode="function")){source("old_crawlers/Gruppe 8/geocode_tool.R")}

#getEventData
getEventData_spital = function (rawData,url){
  event=data.frame(matrix(ncol = 11, nrow = 1));
  #getDesc
  rawData%>%
    html_nodes("td")->eventList
  eventList%>%
    html_text_collapse()->sDesc
  
  
  startDate=NA
  endDate=NA
  startTime=NA
  endTime=NA
  #getDatet
  date=str_extract(sDesc,"Termin:  \\d{2}.\\d{2}.\\d{4}")
  if (!is.na(date) && nchar(date)>=19){
    endDate=substr(date,10,nchar(date))
    date=substr(date,10,nchar(date))
    startDate=as.Date(date,format = "%d.%m.%Y")
    endDate=as.Date(endDate, format="%d.%m.%Y")
    
  }
  #getTime
  time=str_extract(sDesc,"Uhrzeit:  \\d{2}:\\d{2} - \\d{2}:\\d{2}")
  if (!is.na(time) && nchar(time)>=23){
    time=substr(time,10,nchar(time))
    startTime=times(paste0(substr(time,2,6),":00"))
    endTime=times(paste0(substr(time,10,14),":00"))
  }
  sTitle=gsub( "Termin.*$", "", sDesc )
  sDesc=substr(sDesc,0,nchar(sDesc)-15)
  sDesc=gsub("^.*\\t\\t\\t", "", sDesc )
  sDesc=gsub("\\s\\s+/g", ' ', sDesc)
  sDesc=gsub("^ +","",sDesc)
  event=data.frame(title=sTitle,description=sDesc, organizer="B?rgerspital W?rzburg", city=NA, street=NA,zip=NA, lat=9.9350524, lng=49.7957823, date_start=startDate, date_end=endDate, url=url, time_start=startTime, time_end=endTime, price=NA)
  event$time_start=times(event$time_start)
  event$time_end=times(event$time_end)

  return (event)
}

getEvents_spital=function(){
  #readEventLinks
  url="https://www.buergerspital.de/weingut/termine"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".artikeluebersicht_classic .title a")->eventList
  eventList%>%
    html_attr("href")->aEventLinks
  oRes=readLinks(aEventLinks,getEventData_spital,"https://www.buergerspital.de")
  return (oRes)
}
