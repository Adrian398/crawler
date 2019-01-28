library(R.utils)
library(kulife)

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

sourceDirectory("old_crawlers/Gruppe\ 9\ [finished]")


dbDisconnect(conn)
