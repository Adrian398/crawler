### file to write functions which we use in all of the crawlers 

intialize = function(){
  
  url = c()
  title = c()
  link = c()
  date_start = c()
  date_end = c()
  description = c()
  lng = c()
  lat = c()
  organizer = c()
  street = c()
  zip = c()
  city = c()
  time_start = c()
}

##get this in Mainfile (chris)
connect_to_database = function(){

}

write_to_database = function(){
  dbWriteTable(connection, value = data.frame, name = "MyTable", append = TRUE )
}

