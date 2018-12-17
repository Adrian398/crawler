source("Run.R")
source("getDF_Jannis.R")
source("df_Pia.R")
source("dfNiklas.R")

source("geocode_tool.R")

#library(devtools)
#devtools::install_github("dkahle/ggmap")

allEvents <- rbind(getDF_Sabrina(),getDataFrames_Jannis(), getdfNiklas(), df_final)

allEvents$description <- str_replace_all(allEvents$description, "[\r\n]" , " ")
allEvents$title <- str_replace_all(allEvents$title, "[\r\n]" , " ")
allEvents$price <- str_replace_all(allEvents$price, "[\r\n]" , " ")
allEvents$organizer <- str_replace_all(allEvents$organizer, "[\r\n]" , " ")
allEvents$url <- str_replace_all(allEvents$url, "[\r\n]" , " ")


for (i in 1:nrow(allEvents))
{
  if ((is.na(allEvents$city[i])) | (is.na(allEvents$street[i])) | (is.na(allEvents$zip[i])))
  {
    if ((!is.na(allEvents$lat[i])) | (!is.na(allEvents$lng[i])))
    {
      address <- geoLocToAddress(allEvents$lat[i], allEvents$lng[i])
      
      if (!is.na(address))
      {
        allEvents$street[i] <- address[1]
        allEvents$zip[i] <- address[2]
        allEvents$city[i] <- address[3]
      }
    }
    
  }
}

View(allEvents)
write.table(allEvents, sep="||", file = "gruppe8.csv", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")


