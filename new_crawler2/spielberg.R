##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")



##############################
##############################
##############################
######ICS DATEI website#######
##############################
##############################
##############################
##############################


Theater_URL= "https://www.theater-spielberg.de/wordpress/events/"

#read HTML
html = Theater_URL %>%
  read_html() 

#get link, Titel, start,end and description
#html_text = html_nodes(html,".wp_theatre_event")
#html_text

#get link
html %>%
  html_nodes(".tribe-event-url") %>%
  html_attr("href") -> url

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
  
  list = setNames(as.list(c(seq(1,12,1))), c("Januar", "Februar", "M?rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
  
  month = eval(parse(text= paste0("list$", subDate[2])))
  
  year = format(Sys.Date(), "%Y")
  
  date = paste0(c(day, month, year), seq="", collapse = "/" )
  time1 = "00"
  
  datetime = paste0(c(date, subDate[4], time1), seq="", collapse = ":" )
  
  mydatetime = strptime(datetime,format='%d/%m/%Y:%H:%M:%S')
  
  return(mydatetime)
}

#get description
html_nodes(html_text, ".wp_theatre_prod_excerpt") %>%
  html_text(trim = T) -> description

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

html_nodes(html_text, ".wp_theatre_event_datetime") %>%
  html_text(trim = T) -> event_date
event_date

date_start = map(as.character(event_date), get_Date)
date_start = sapply( date_start, paste0, collapse="")

df = data.frame(title, url, description, lng, lat, city, zip, street, date_start, date_end, time_start, time_end, price, organizer)

df = df %>%
  separate(date_start, c("date_start", "time_start"), " ") %>%
  mutate(
    date_start = ymd(date_start),
    time_start = times(time_start),
    title = as.character(title),
    url = as.character(url),
    description = as.character(description),
    city = as.character(city),
    street = as.character(street),
    organizer = as.character(organizer),
    date_end = NULL,
    time_end = NULL,
    price = as.character(price)
  )

for (row in 1:names(df)) {
  print(count())
}







### Neunerplatz Theater  ####
# functions initialization #
month_convertor <- function(given_date){
  given_date = gsub(" Januar ","01.",given_date)
  given_date = gsub(" Februar ","02.",given_date)
  given_date = gsub(" März ","03.",given_date)
  given_date = gsub(" April ","04.",given_date)
  given_date = gsub(" Mai ","05.",given_date)
  given_date = gsub(" Juni ","06.",given_date)
  given_date = gsub(" Juli ","07.",given_date)
  given_date = gsub(" August ","08.",given_date)
  given_date = gsub(" September ","09.",given_date)
  given_date = gsub(" Oktober ","10.",given_date)
  given_date = gsub(" November ","11.",given_date)
  given_date = gsub(" Dezember ","12.",given_date)
  return(given_date)
}

# crawl data
url <- "https://www.mainfrankentheater.de/spielplan/spielzeit-18-19/"

url %>%
  read_html() %>%
  html_nodes(".season__production") %>%
  html_attr("href") -> link

link = paste0("https://www.mainfrankentheater.de", link)

title = c()
date_start = c()
date_end = c()
description = c()

for (temp_url in link){
  temp_url %>%
    read_html() %>%
    html_nodes("#content") -> raw_read
  
  raw_read %>%
    html_node("h1") %>%
    html_text(trim = T)-> temp_title
  
  raw_read %>%
    html_node("h2+ strong") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") -> temp_date_start
  
  raw_read %>%
    html_node("h2+ strong") %>%
    html_text(trim = T) %>%
    str_extract("-\\s[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") -> temp_date_end
  
  temp_date_end = gsub("-\\s", "", temp_date_end)
  
  raw_read %>%
    html_node("#ktml") %>%
    html_text(trim = T)-> temp_description
  
  title = c(title, temp_title)
  date_start = c(date_start, temp_date_start)
  date_end = c(date_end, temp_date_end)
  description = c(description, temp_description)
  
}

# fixed data setup
time_start = rep(NA, length(title))
time_end = rep(NA, length(title))
organizer = rep("Salon 77", length(title))
lat = rep(49.79304, length(title))
lng = rep(9.95808, length(title))
street = rep("Richard-Wagner-Straße 60", length(title))
zip = rep(97074, length(title))
city = rep("Würzburg", length(title))

# data type conversion
date_start <- as.Date(date_start, "%d.%m.%Y")
date_end <- as.Date(date_end, "%d.%m.%Y")

time_start <- chron(times = time_start)
time_end <- chron(times = time_end)

# build table
df <- data.frame(title = title,
                 date_start = date_start,
                 date_end = date_end, 
                 time_start = time_start,
                 time_end = time_end,
                 description = description,
                 organizer = organizer,
                 lat = lat,
                 lng = lng,
                 street = street,
                 zip = zip,
                 city = city,
                 link = link)

#set up to write to database
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat")]
meta_df = df[c("organizer", "link")][1,]
names(meta_df)[names(meta_df) == 'link'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)