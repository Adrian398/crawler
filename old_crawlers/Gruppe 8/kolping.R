require(tidyverse)
require(rvest)
require(chron)

if(!exists("readLinks", mode="function")){ source("utils.R")}
if(!exists("addressToGeoLoc", mode="function")){source("geocode_tool.R")}


#getEventData
getEventData_kolping = function (rawData,url){
  event=data.frame(matrix(ncol = 11, nrow = 1));
  #getTitle
  rawData%>%
    html_nodes(".title")%>%html_text()->sTitle
  
  #getDate
  rawData%>%
    html_nodes(".info_value .datetime")%>%html_text()->oDate
  aDates=str_extract_all(oDate,"\\d{2}\\.\\d{2}\\.\\d{4}", simplify=TRUE)
  aTimes=str_extract_all(oDate,"\\d{2}\\:\\d{2}",simplify=TRUE)
  oStartDate=NA
  oEndDate=NA
  oStartTime=NA
  oEndTime=NA
  if (length(aDates>=1)){
    oStartDate=as.Date(aDates[1], format="%d.%m.%Y")
  }
  if (length(aDates>=2)){
    oEndDate=as.Date(aDates[2], format="%d.%m.%Y")
  }
  if (length(aTimes)>1){
    oStartTime=times(paste0(aTimes[1],":00"))
  }
  if (length(aTimes)>=2){
    oEndTime=times(paste0(aTimes[2],":00"))
  }
  rawData%>%
    html_nodes(".datarow-container")%>%html_text_collapse()->sDesc
  
  sDesc=str_replace_all(sDesc,"\t vergr??ern|\t mehr Bilder|\tKolping-Akademie","")
  sDesc=str_replace_all(sDesc,"\t|\n|\r","")
  
  rawData%>%
    html_nodes(".defaultDataTable")%>%html_table()->oTable
  sLoc=NA
  if(length(oTable)>=2 && (length(oTable[[1]])==length(oTable[[2]]))){
    oInfoTable=oTable[[2]]
    for (i in 1:length(oInfoTable[[1]])){
      if (oInfoTable[[1]][[i]]=="Ort"){
        sLoc=oInfoTable[[2]][[i]]
        if (length(oInfoTable[[1]])>=(i+1) && oInfoTable[[1]][[i+1]]==""){
          sLoc=paste(sLoc, oInfoTable[[2]][[i+1]]," ")
        }
        break;
      }
    }
  }
  lat=NA
  lon=NA
  if (!is.na(sLoc)){
    aCoordinates=addressToGeoLoc(sLoc)
    if (is.numeric(aCoordinates)){
      lat=aCoordinates[[1]]
      lon=aCoordinates[[2]]
    }
  }
  
 
  
  event=data.frame(Eventname=sTitle,Eventbeschreibung=sDesc, Eventveranstalter="Kolping Akademie W?rzburg", Ort=sLoc, Lat=lat, Lon=lon, Eventstart=oStartDate, Eventende=oEndDate, Eventurl=url, Eventkalenderurl=NA, Preis=NA)
  event=data.frame(title=sTitle,description=sDesc, organizer="Kolping Akademie W?rzburg", city=NA, street=NA,zip=NA, lat=lat, lng=lon, date_start=oStartDate, date_end=oEndDate, url=url, time_start=oStartTime, time_end=oEndTime, price=NA)
  event$time_start=times(event$time_start)
  event$time_end=times(event$time_end)
  event$date_end=as.Date(event$date_end)
  return (event)
  
}

getEvents_kolping=function(){
  #readEventLinks
  url="https://www.kolping-akademie-wuerzburg.de/alle-veranstaltungen"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".itemtitle a") %>%
    html_attr("href")->aEventLinks
  oRes=readLinks(aEventLinks,getEventData_kolping,"http:")
  return (oRes)
}


