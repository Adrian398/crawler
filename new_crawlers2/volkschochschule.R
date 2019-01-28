##### Import Packages ####
library(rvest)
library(tidyverse)
library(chron)
source("write_to_database.R")

### Possible Improvement ###
# 1. only scrape bookable events
# 2. multiple appointments from each course cannot be crawled
# responsible: Wei

#### Volkschochschule Würzburg ###
# data setup
gesellschaft_url = "https://www.vhs-wuerzburg.info/programm/gesellschaft.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=405&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892354%22%2C%22page%22%3A0%2C%22returnType%22%3A%22default%22%2C%22pageSize%22%3A%2299999%22%2C%22searchString%22%3A%22%22%7D&filters%5Bpage%5D=1&filters%5Bbookable%5D=1&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgesellschaft.html%3Faction%255B405%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgesellschaft.html&sockets_ID=405&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
beruf_url = "https://www.vhs-wuerzburg.info/programm/beruf.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=401&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892355%22%2C%22pageSize%22%3A%229999999%22%2C%22searchString%22%3A%22%22%7D&filters%5Bpage%5D=1&filters%5Bbookable%5D=1&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fberuf.html%3Faction%255B401%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fberuf.html&sockets_ID=401&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
sprachen_url = "https://www.vhs-wuerzburg.info/programm/sprachen.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=402&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892356%22%2C%22pageSize%22%3A%22999999%22%2C%22returnType%22%3A%22default%22%2C%22searchString%22%3A%22%22%7D&filters%5Bpage%5D=1&filters%5Bbookable%5D=1&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fsprachen.html%3Faction%255B402%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fsprachen.html&sockets_ID=402&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
kultur_url <- "https://www.vhs-wuerzburg.info/programm/kultur.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=404&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892357%22%2C%22pageSize%22%3A%2299999%22%2C%22searchString%22%3A%22%22%7D&filters%5Bpage%5D=1&filters%5Bbookable%5D=1&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fkultur.html%3Faction%255B404%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fkultur.html&sockets_ID=404&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
grundbildung_url <- "https://www.vhs-wuerzburg.info/programm/grundbildung.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=406&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT9143021%22%2C%22pageSize%22%3A%22999999%22%2C%22searchString%22%3A%22%22%7D&filters%5Bpage%5D=1&filters%5Bbookable%5D=1&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgrundbildung.html%3Faction%255B406%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgrundbildung.html&sockets_ID=406&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="

# crawl links for each event
node_link = ".table-responsive > table > tbody > tr"

function_get_links = function(input_url){
  input_url %>%
    read_html() %>%
    html_nodes(node_link) %>%
    html_attr('data-href') -> input_link
  input_link = paste("https:", input_link, sep = "")
  return(input_link)
}

gesellschaft_link = function_get_links(gesellschaft_url)
beruf_link = function_get_links(beruf_url)
sprachen_link = function_get_links(sprachen_url)
kultur_link = function_get_links(kultur_url)
grundbildung_link = function_get_links(grundbildung_url)

link = c(gesellschaft_link, beruf_link, sprachen_link, kultur_link, grundbildung_link)

# crawl data
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
function_get_title = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("h1") %>%
    html_text(trim = T) -> title
  title = gsub("\n", " ", title)
  title = gsub("\t", "", title)
  return(title)
}
function_get_description = function(input_link){
  input_link %>%
    read_html() %>%
    html_node(".pull-left p") %>%
    html_text(trim = T) -> description
  description = gsub("\n", " ", description)
  description = gsub("\r", "", description)
  return(description)
}
function_get_date_start = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("tr:nth-child(3) td+ td") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}") -> date_start
  if (is.na(date_start)){
    input_link %>%
      read_html() %>%
      html_node(".date") %>%
      html_text(trim = T) %>%
      str_extract("[0-9]{2}\\.\\s[[:alpha:]]+\\s[0-9]{4}") -> date_start
    date_start = month_convertor(date_start)
  }
  return(date_start)
}
function_get_time_start = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("tr:nth-child(3) td+ td") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{2}:[0-9]{2}") -> time_start
  if (is.na(time_start)){
    input_link %>%
      read_html() %>%
      html_node(".date") %>%
      html_text(trim = T) %>%
      str_extract("[0-9]{2}:[0-9]{2}") -> time_start
  }
  time_start = paste(time_start, ":00", sep = "")
  return(time_start)
}
function_get_time_end = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("tr:nth-child(3) td+ td") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{2}:[0-9]{2}\\s") -> time_end
  if (is.na(time_end)){
    input_link %>%
      read_html() %>%
      html_node("tr:nth-child(1) td~ td+ td") %>%
      html_text(trim = T) %>%
      str_extract("[0-9]{2}:[0-9]{2}\\s") -> time_end
  }
  time_end = gsub(" ", "", time_end)
  time_end = paste(time_end, ":00", sep = "")
  return(time_end)
}
function_get_city = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("p span") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{5}\\s[[:alpha:]]+") -> city
  city = gsub("[0-9]{5}\\s", "", city)
  return(city)
}
function_get_zip = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("p span") %>%
    html_text(trim = T) %>%
    str_extract("[0-9]{5}") -> zip
  return(zip)
}
function_get_street = function(input_link){
  input_link %>%
    read_html() %>%
    html_node("p span") %>%
    html_text(trim = T) -> street
    street = gsub("[0-9]{5}\\s[[:alpha:]]+", "", street)
    street = gsub(" ", "", street)
    street = gsub("\n", " ", street)
  return(street)
}

title = c()
description = c()
date_start = c()
time_start = c()
time_end = c()
city = c()
zip = c()
street = c()

for (n in link){
  temp_title = function_get_title(n)
  title = c(title, temp_title)
  
  temp_description = function_get_description(n)
  description = c(description, temp_description)
  
  temp_date_start = function_get_date_start(n)
  date_start = c(date_start, temp_date_start)
  
  temp_time_start = function_get_time_start(n)
  time_start = c(time_start, temp_time_start)
  
  temp_time_end = function_get_time_end(n)
  time_end = c(time_end, temp_time_end)
  
  temp_city = function_get_city(n)
  city = c(city, temp_city)
  
  temp_zip = function_get_zip(n)
  zip = c(zip, temp_zip)
  
  temp_street = function_get_street(n)
  street = c(street, temp_street)
}

date_end = rep(NA, length(title))
organizer = rep("Volkshochschule Würzburg", length(title))
lat = rep(49.78884, length(title))
lng = rep(9.93252, length(title))

# data type conversion
date_start = as.Date(date_start, "%d.%m.%Y")

time_start = chron(times = time_start)
time_end = chron(times = time_end)

# Build table
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
