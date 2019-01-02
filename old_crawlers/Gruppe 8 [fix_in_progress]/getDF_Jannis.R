require(tidyverse)
require(rvest)
source("utils.R")
source("buergerspital.R")
source("eisbahn.R")
source("kolping.R")
source("kaeppele.R")
source("stiftHaug.R")

getDataFrames_Jannis=function(){
  oRes=getEvents_spital()
  oRes=data.frame(rbind((oRes), (getEvents_eisbahn())))
  oRes=data.frame(rbind((oRes), (getEvents_stift())))
  oRes=data.frame(rbind((oRes), (getEvents_kolping())))
  oRes=data.frame(rbind((oRes), (getEvents_kaeppele())))
  return (oRes)
  
}
