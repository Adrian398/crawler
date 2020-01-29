library(R.utils)
library(DBI)
library(kulife)
library(readr)
library(tidyverse)
library(rvest)
library(chron)

#File to write new events to the database
#database is hosted at AWS
source("write_to_database.R")

#file to create the XML from the crawled information
source("write_xml.R")



#SQL database connection
getSqlConnection <- function(){
  con <-dbConnect(
    RMySQL::MySQL(),
    username = 'crawler',
    password = 'crawler2018',
    
    host = 'a1.cj8zdbsk8kip.eu-central-1.rds.amazonaws.com',
    port = 3306,
    dbname = 'eventscalender',
    encoding = "utf8"
  )
  return(con)
}
conn <- getSqlConnection()

#delete database content
#delete all the content before the new crawl
#since 01.12.2019 we just need to provide all events that we crawl and send them to the city via this bucket: https://console.cloud.google.com/storage/browser/crawled-events?authuser=1&project=crawler2020-263611
dbSendStatement(conn, 'SET foreign_key_checks = 0;')
dbSendStatement(conn, 'TRUNCATE table organizer;')
dbSendStatement(conn, 'TRUNCATE table event;')
dbSendStatement(conn, 'SET foreign_key_checks = 1;')


#create new_events.xml files
write('',"new_events.xml",append=FALSE)
write('',"deleted_events.xml",append=FALSE)

#count for creating ID
countit <<- 0

#main programm running all the files in new_crawlers
## try catch for continuing the process when interrupted by an error
for (file_name in list.files("new_crawlers")){
  tryCatch({
    print(file_name)
    source(paste0("new_crawlers/", file_name))
  }, error = function(e) {
    message(paste("Error in crawler:", file_name))
    message("Here is the original error message:")
    message(e)
    return(NA)
  }, finally = {
  })
}

#merge all crawled XMLs and finish them with header information and closing tag
finish_xml("new_events.xml")
finish_xml("deleted_events.xml")


#disconnect all connections 
dbDisconnect(conn)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


#quit R for automatic application on server
#quit(save='no')
