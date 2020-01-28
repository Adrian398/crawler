##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

Theater_URL= "http://www.neunerplatz.de/spielplan/"

#read HTML
html = Theater_URL %>%
  read_html() 

#get link, Titel, start,end and description
html_text = html_nodes(html,".wp_theatre_event")
html_text

#get link
html_text %>%
  html_nodes(".wp_theatre_event_title a") %>%
  html_attr("href") -> link

#get img
html_text %>%
  html_nodes(".wp-post-image") %>%
  html_attr("src")-> image_url

#get title
html_nodes(html_text, ".wp_theatre_event_title") %>%
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
  

  
  return(c(date,subDate[4]))
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
price = NA
street = "Adelgundenweg 2a"
date_end = NA
time_end = NA
#create Dataframe

html_nodes(html_text, ".wp_theatre_event_datetime") %>%
  html_text(trim = T) -> event_date

date_time= map(as.character(event_date), get_Date)
date_time = unlist(date_time)



date_start = date_time[seq(1,length(date_time),2)]
time_start = date_time[seq(2,length(date_time),2)]


time_start = paste0(time_start,":00")
time_start <- chron(times = time_start)

date_start = as.Date(date_start, "%d/%m/%Y")
date_end = as.Date(date_end, "%d.%m.%Y")

df = data.frame(title, link, description, lng, lat, city, zip, street, date_start, date_end, time_start, time_end, price, organizer, image_url)



#set up to write to database
crawled_df = df[c("title", "description", "link", "date_start", "date_end", "time_end", "time_start", "street", "city", "zip", "lng", "lat","image_url")]

#add metadf idlocation
idlocation = 6238
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'


meta_df["idcrawler"] = 30
meta_df["id_category"] = 10586

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)
