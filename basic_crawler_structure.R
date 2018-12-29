### basic structure for the individual Crawlers
#source("util.R")

#get_events = function() {
  
 # initialize()
  
  #mySqlpart
  #create and use goolge database
  
#}

#if(!interactive()) {
 # get_events()
#}


###TASKS untill Monday
#Adrian
#create db
#upload to bitbucket
#create structured script

#Tsung-Wei
#check crawlers
#create structured




install.packages("RMySQL")
library("DBI")
getSqlConnection <- function(){
  con <-dbConnect(
      RMySQL::MySQL(),
      username = 'Adrian',
      password = 'root',
      host = '104.199.97.47',
      dbname = 'event_calender'
    )
  return(con)
}
conn <- getSqlConnection()
res <- dbListTables(conn)
print(res)
dbDisconnect(conn) # Closes all open connections




