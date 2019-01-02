#### combine my Data to one Dataframe 
library(dplyr)

library (devtools)
library (ggmap)

# wie exectue
source("crawler_schroederHaus.R")
source("crawler_sparkasse.R")
source("crawler_sieboldMuseum.R")

getDF_Sabrina <- function(){
  
  bind_rows(veranstaltungen_sparkasse, veranstaltungen_schroederHaus, veranstaltungen_siebold) -> crawler_sabrina
  
  aNames=colnames(crawler_sabrina)
  colnames(crawler_sabrina)<-aNames
  return(crawler_sabrina)
}

getDF_Sabrina()
