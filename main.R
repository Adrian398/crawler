library(R.utils)
library(kulife)
source("write_xml.R")
library(readr)

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

write("","new_events.xml",append=FALSE)
write("","deleted_events.xml",append=FALSE)
#write(substring(as(saveXML(xml,encoding = "utf8"), "character"),24), "new_events.xml",append=TRUE)
try(sourceDirectory("new_crawlers"))
finish_xml("new_events.xml")
finish_xml("deleted_events.xml")

dbDisconnect(conn)

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

