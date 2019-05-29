library(R.utils)
library(DBI)
library(kulife)
source("write_xml.R")
library(readr)
library(foreach)
library(doParallel)

#library(furrr)

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

# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer
# registerDoParallel(cl)
# 
# output = foreach(file_name=list.files("new_crawlers")) %dopar% {
#   tryCatch({
#     print(file_name)
#     source(paste0("new_crawlers/", file_name))
#   }, warning = function(w) {
#     message("Warning!!!!")
#     return(NULL)
#   }, error = function(e) {
#     message(paste("Error in crawler:", file_name))
#     return(NA)
#   }, finally = {
#   })
# }
# #stop cluster
# stopCluster(cl)

#mclapply(list.files("new_crawlers")[20:24], source())




#source(paste0("new_crawlers/", "buergerbraeu.R"))

#list.files("new_crawlers")[20:24]

for (file_name in list.files("new_crawlers")[20:24]){
  tryCatch({
    print(file_name)
    source(paste0("new_crawlers/", file_name))
  }, error = function(e) {
    message(paste("Error in crawler:", file_name))
    return(NA)
  }, finally = {
  })
}





#try(sourceDirectory("new_crawlers"))
finish_xml("new_events.xml")
finish_xml("deleted_events.xml")

dbDisconnect(conn)

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

