##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

url= "https://www.theater-spielberg.de/wordpress/events/"

#read HTML
html = url %>%
  read_html() 

#get link, Titel, start,end and description
#html_text = html_nodes(html,".wp_theatre_event")
#html_text

#get link
html %>%
  html_nodes(".tribe-event-url") %>%
  html_attr("href") -> link

#get img
html %>%
  html_nodes(".wp-post-image") %>%
  html_attr("src")-> img

#get title
html_nodes(html, ".tribe-event-url") %>%
  html_text(trim = T) -> title

# get price
# price not crawlable
#price = 

#function get start and end Date
get_Date <- function(arg1){
  arg2 = strsplit(as.character(arg1),", ")[[1]]
  
  #get start and end
  subDate = arg2[2]
  subDate = strsplit(subDate," ")[[1]]
  
  day = substr(subDate[1],1,nchar(subDate[1])-1)
  
  list = setNames(as.list(c(seq(1,12,1))), c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
  
  month = eval(parse(text= paste0("list$", subDate[2])))
  
  year = format(Sys.Date(), "%Y")
  
  date = paste0(c(day, month, year), seq="", collapse = "/" )
  time1 = "00"
  
  datetime = paste0(c(date, subDate[4], time1), seq="", collapse = ":" )
  
  mydatetime = strptime(datetime,format='%d/%m/%Y:%H:%M:%S')
  
  return(mydatetime)
}

#get description
html_nodes(html, '#tribe-events-content p') %>%
  html_text(trim = T) -> description

  
#".wp_theatre_prod_excerpt"
#get lat lng
#same location

lng = 9.9158
lat = 49.79639
city = "Wuerzburg"
zip = 97082
organizer = "Theater am Neunerplatz"
price = price
street = "Adelgundenweg 2a"
time_start = NA
time_end = NA
date_end = NA

#create Dataframe

html_nodes(html, "#tribe-events-content .tribe-event-date-start") %>%
  html_text(trim = T) -> date_times


month_convertor <- function(given_date){
  given_date = gsub("Januar","01.",given_date)
  given_date = gsub("Februar","02.",given_date)
  given_date = gsub("März","03.",given_date)
  given_date = gsub("April","04.",given_date)
  given_date = gsub("Mai","05.",given_date)
  given_date = gsub("Juni","06.",given_date)
  given_date = gsub("Juli","07.",given_date)
  given_date = gsub("August","08.",given_date)
  given_date = gsub("September","09.",given_date)
  given_date = gsub("Oktober","10.",given_date)
  given_date = gsub("November","11.",given_date)
  given_date = gsub("Dezember","12.",given_date)
  return(given_date)
}
date_times = strsplit(date_times, " um ")
date_start = c()
time_start = c()

for (date_time in date_times){
  date = date_time[1]
  time = date_time[2]
  date = strsplit(date, ". ")
  date_day = date[[1]][1]
  date_month = date[[1]][2]
  date_month= month_convertor(date_month)
  date = paste0(paste(date_day,date_month,sep = "."), format(Sys.Date(), "%Y"))
  date_start = c(date_start, date)
  time_start = c(time_start,time)
}

time_start = paste0(time_start,":00")
time_start <- chron(times = time_start)

date_start = as.Date(date_start,"%d.%m.%Y")


#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
#add metadf idlocation
idlocation = 4147
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 21
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
