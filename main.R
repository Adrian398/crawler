library(R.utils)
library(DBI)
library(kulife)
source("write_xml.R")
library(readr)
library(tidyverse)
library(rvest)
library(chron)
source("write_to_database.R")

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
dbSendStatement(conn, 'SET foreign_key_checks = 0;')
dbSendStatement(conn, 'TRUNCATE table event;')
dbSendStatement(conn, 'SET foreign_key_checks = 1;')

write('',"new_events.xml",append=FALSE)
write('',"deleted_events.xml",append=FALSE)

countit <<- 1 
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

#try(sourceDirectory("new_crawlers"))
finish_xml("new_events.xml")
finish_xml("deleted_events.xml")

dbDisconnect(conn)

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

#quit(save='no')
