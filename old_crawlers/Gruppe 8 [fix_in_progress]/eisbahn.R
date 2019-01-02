require(tidyverse)
require(rvest)
require(chron)
if(!exists("addressToGeoLoc", mode="function")){source("geocode_tool.R")}
if(!exists("readLinks", mode="function")){ source("utils.R")}



getEisbahndisco_eisbahn=function (sDate, sDesc, url){
  oStartDate=as.Date(sDate,format="%d.%m.%Y")
  oEndDate=NA
  startTime=NA
  endTime=NA
  aTime=str_extract_all(sDesc,"\\d{2}:\\d{2}",simplify = TRUE)
  if (length(aTime)>=1 && !is.na(aTime[1])){
    startTime=times(paste0(aTime[1],":00"))
    
  }
  if (length(aTime)>=2 && !is.na(aTime[2])){
    oEndDate=oStartDate
    endTime=times(paste0(aTime[2],":00"))
  }
  event=data.frame(title="Eisbahndisco",description="Eisbahndisco", organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
  event$time_start=times(event$time_start)
  event$time_end=times(event$time_end)
  return (event)
}



getEvents_eisbahn=function(){
  #readEventLinks
  url="https://www.wvv.de/de/privatkunden/baeder/unser-angebot/eisbahn/eishockey/"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".w670 table") %>%
    html_table()->oTable
  oRes=data.frame()
  if (length(oTable)>=1){
    oTable=oTable[[1]]
    if (length(oTable)==6){
      for (i in 1:length(oTable[[1]])){
        oTemp=data.frame();
        sDate=str_extract(oTable[[1]][[i]],"\\d{2}\\.\\d{2}\\.\\d{4}")
        sBeginn=oTable[[2]][[i]]
        if (startsWith(sBeginn,"Eisbahndisco")){
          event=getEisbahndisco_eisbahn(sDate,sBeginn,url)
        }else if (startsWith(sBeginn,"ABGESAGT")){
          next;
        }else{
          sBeginn=str_extract(sBeginn,"\\d{2}:\\d{2}")
          oStartDate=as.Date(sDate,format="%d.%m.%Y")
          startTime=times(paste0(sBeginn,":00"))
          oEndDate=NA
          endTime=NA
          sDesc=NA
          sGegner=oTable[[3]][[i]]
          sMannschaft=oTable[[4]][[i]]
          sLetzerEinlass=oTable[[5]][[i]]
          Eislaufende=oTable[[6]][[i]]
          
          if (startsWith(sGegner,"Decathlon")){
            sDesc=sGegner
            event=data.frame(title=sDesc,description=sDesc, organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
            event$time_start=times(event$time_start)
            event$time_end=times(event$time_end)      
          }else{
            sDesc=paste("Gegner: ",sGegner,"\n")
            sMannschaft=paste0("Mannschaft: ",sMannschaft)
            sDesc=paste(sDesc,sMannschaft,"\n")
            sLetzerEinlass=paste0("Letzter Einlass: ",sLetzerEinlass);
            sDesc=paste(sDesc,sLetzerEinlass,"\n")
            if (Eislaufende!=""){
              Eislaufende=paste("Eislaufende: ",Eislaufende)
              sDesc=paste(sDesc,Eislaufende,"\n")
            }
            event=data.frame(title="Eishockeyspiel",description=sDesc, organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
            event$time_start=times(event$time_start)
            event$time_end=times(event$time_end)  
           
          }
          
        } 
        oRes=data.frame(rbind((oRes), (event)))
      }
    }
  }

  
  return (oRes)
}


