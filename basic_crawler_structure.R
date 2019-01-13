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
      username = 'crawler',
      password = 'crawler2018',
      host = 'a1.cj8zdbsk8kip.eu-central-1.rds.amazonaws.com',
      port = 3306,
      dbname = 'eventscalender'
    )
  return(con)
}
conn <- getSqlConnection()
dbListTables(conn)

dbDisconnect(conn) # Closes all open connections


dbWriteTable(con, value = data.frame, name = "MyTable", overwrite = TRUE ) 

### matching mit zeit und title 
### crawler user
## dbplr


## serientermine erstmal rauslassen

###category bei event dazu!!! 

