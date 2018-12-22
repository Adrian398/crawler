### file to write functions which we use in all of the crawlers 

intialize = function(){
  
  url = c() # chr
  title = c() # chr
  link = c() # chr
  date_start = c() # date
  date_end = c() # date
  description = c() # chr
  lng = c() # numeric
  lat = c() # numeric
  organizer = c() # chr
  street = c() # chr
  zip = c() # numeric
  city = c() # chr
  time_start = c() #time
  time_end = c() #time
}

##get this in Mainfile (chris)
connect_to_database = function(){

}

write_to_database = function(){
  dbWriteTable(connection, value = data.frame, name = "MyTable", append = TRUE )
}

