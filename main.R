library(R.utils)
library(kulife)
source("write_xml.R")

getSqlConnection <- function(){
  con <-dbConnect(
    RMySQL::MySQL(),
    username = 'crawler',
    password = 'crawler2018',
    host = 'a1.cyawe3clu0j3.eu-west-1.rds.amazonaws.com',
    port = 3306,
    dbname = 'eventscalender',
    encoding = "utf8"
  )
  return(con)
}
conn <- getSqlConnection()

write("","new_events.xml",append=FALSE)
start_xml = "<?xml version='1.0'?>\n\n<crawlers date = '28.01.2019'>"
end_xml = "</crawlers>"
#write(substring(as(saveXML(xml,encoding = "utf8"), "character"),24), "new_events.xml",append=TRUE)
sourceDirectory("new_crawlers")
xml_document = read_file("new_events.xml")
#xml_document = substring(xml_document,nchar(start_xml)+1)
xml_document = gsub("\n","\n  ",xml_document)
xml_document = paste0(start_xml, xml_document)
xml_document = substr(xml_document,1,nchar(xml_document)-2)
xml_document = paste0(xml_document,end_xml)

write(xml_document,"new_events.xml",append=FALSE)

dbDisconnect(conn)

