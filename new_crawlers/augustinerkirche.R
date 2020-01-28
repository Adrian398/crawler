url <- "http://augustinerkirche-wuerzburg.de/veranstaltungen-3/"

url %>%
  read_html() -> raw_read

#get title 
raw_read %>%
  html_nodes(".ai1ec-load-event") %>%
  html_text(trim = T) -> title

#get link
raw_read %>%
  html_nodes(".ai1ec-load-event") %>%
  html_attr("href") -> link

#correct title and link:
title = title[seq(1,length(title),2)]
link = link[seq(1,length(link),2)]

#get date start
raw_read %>%
  html_nodes(".ai1ec-month") %>%
  html_text(trim = T) -> month

raw_read %>%
  html_nodes(".ai1ec-day") %>%
  html_text(trim = T) -> day

month_convertor <- function(given_date){
  given_date = gsub("Jan","01.",given_date)
  given_date = gsub("Feb","02.",given_date)
  given_date = gsub("Mär","03.",given_date)
  given_date = gsub("Apr","04.",given_date)
  given_date = gsub("Mai","05.",given_date)
  given_date = gsub("Jun","06.",given_date)
  given_date = gsub("Jul","07.",given_date)
  given_date = gsub("Aug","08.",given_date)
  given_date = gsub("Sep","09.",given_date)
  given_date = gsub("Okt","10.",given_date)
  given_date = gsub("Nov","11.",given_date)
  given_date = gsub("Dez","12.",given_date)
  return(given_date)
}

month = map(month,month_convertor)
month = unlist(month)
date = paste(day,month,sep = ".")
date_start = paste0(date, format(Sys.Date(), "%Y"))
date_start = as.Date(date_start,"%d.%m.%Y")
date_end = NA

#get time
raw_read %>%
  html_nodes(".ai1ec-event-time") %>%
  html_text(trim = T) -> times
time_start = c()
time_end = c()
times = strsplit(times, " ")
for (i in 1:length(times)){
  if(length(times[[i]]) == 4){
    time_start[i] = times[[i]][length(times[[i]])]
    time_end[i] = NA
    if(!is.na(time_start[i]))
      time_start[i] = paste0(time_start[i],":00")
    if(!is.na(time_end[i]))
      time_end[i] = paste0(time_end[i],":00")
  }else{
    time_start[i] = times[[i]][length(times[[i]])-2]
    time_end[i] = times[[i]][length(times[[i]])]
    if(!is.na(time_start[i]))
      time_start[i] = paste0(time_start[i],":00")
    if(!is.na(time_end[i]))
      time_end[i] = paste0(time_end[i],":00")
  }
  
} 

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

raw_read %>%
  html_nodes(".ai1ec-event-description") %>%
  html_text(trim = T) -> description

city = "Wuerzburg"
street = "Dominikanerplatz 2"
zip = 97070
organizer = "Augustinerkirche"
lat = 49.7963	
lng = 9.93119

#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
#add metadf idlocation
idlocation = 4231
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'
meta_df["idcrawler"] = 1
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
