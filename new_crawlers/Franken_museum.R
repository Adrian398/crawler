#Warning: Carwling might take some time (>1 minute)

url <- "https://museum-franken.de/no_cache/veranstaltungen/kalender.html"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".list_title") %>%
  html_text(trim = T) -> title

link = url

raw_read %>%
  html_nodes(".list_date") %>%
  html_text(trim = T) -> date_time

raw_read %>%
  html_nodes(".csc-text") %>%
  html_text(trim = T) -> description


month_convertor <- function(given_date){
  given_date = gsub("Januar,","01.",given_date)
  given_date = gsub("Februar,","02.",given_date)
  given_date = gsub("März,","03.",given_date)
  given_date = gsub("April,","04.",given_date)
  given_date = gsub("Mai,","05.",given_date)
  given_date = gsub("Juni,","06.",given_date)
  given_date = gsub("Juli,","07.",given_date)
  given_date = gsub("August,","08.",given_date)
  given_date = gsub("September,","09.",given_date)
  given_date = gsub("Oktober,","10.",given_date)
  given_date = gsub("November,","11.",given_date)
  given_date = gsub("Dezember,","12.",given_date)
  return(given_date)
}

date_time = strsplit(date_time, " ")
day = unlist(lapply(date_time, `[[`, 2))
month = unlist(lapply(date_time, `[[`, 3))

time = unlist(lapply(date_time, `[[`, 4))
month = unlist(map(month,month_convertor))

date_start = paste0(paste0(day,month),format(Sys.Date(), "%Y"))
date_start = as.Date(date_start, "%d.%m.%Y")

time_start = paste0(time,":00")
time_start <- chron(times = time_start)



# setting up the rest data
city = "Wuerzburg"
street = "Festung Marienberg, Oberer Burgweg"
zip = 97082
organizer = "Museum für Franken"
lat = 49.7897012
lng = 9.920944
time_end = NA
date_end = as.Date(NA, "%d.%m.%Y")

#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)

#add metadf idlocation
idlocation = 4688
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)



