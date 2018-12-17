library(RMariaDB)
library(dbConnect)
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(tidyr)
library(knitr)
library(chron)
library(lubridate)
library(plyr)
library(qdapRegex)
library(RSelenium)
library(rowr)
library(purrr)
library(gsubfn)
library(RJSONIO)

# problem: unifying, simplfying library

getKompetenzzentrumEvents = function() {
  #save URL
  Kompetenzzentrum_URL= "http://www.energietechnik-hwk.de"
  
  #read HTML
  html = Kompetenzzentrum_URL %>%
    read_html() 
  
  #get links from html 
  html_nodes(html, ".list-group-item") %>%
    html_attr("href") -> links
  links = links[!is.na(links)]
  
  url = paste(Kompetenzzentrum_URL, links, sep = "")
  
  
  #get Titel, start,end and description
  html_text = html_nodes(html,".listgroup-text")
  html_text
  
  #get title
  html_nodes(html_text, "strong") %>%
    html_text(trim = T) -> title
  title
  
  
  #function get start and end Date
  
  get_Date <- function(arg1){
    arg2 = strsplit(as.character(arg1),"<br>")[[1]]
    arg2
    
    #get start and end
    subDate = arg2[1]
    
    subDate = strsplit(subDate,"[<>]")[[1]][3]
    
    extractedDates =  strsplit(subDate, " ")[[1]][-2]
    extractedDates = gsub('\\.', '/', extractedDates)
    
    
    extractedDates
    
    return(extractedDates)
  }
  
  #get description
  get_description <- function(arg1){
    arg2 = strsplit(as.character(arg1),"<br>")[[1]]
    arg2
    
    
    description = paste(arg2[4:length(arg2)], collapse = " ")
    description = substr(description, 1  , nchar(description)-6)
    
    description
    return(description)
  }
  
  #get lat lng
  #same location
  
  lng = 9.9083
  lat = 49.79923
  city = "Wuerzburg"
  zip = 97082
  organizer = "Handwerkskammer für Unterfranken"
  price = NA
  street = NA
  time_start = NA
  time_end = NA
  
  
  #create Dataframe
  description =map(as.character(html_text), get_description)
  
  description = sapply( description, paste0, collapse="")
  
  
  Date = map(as.character(html_text), get_Date)
  date_start = map(Date, 1) 
  date_end = map(Date, 2) 
  
  date_start = sapply( date_start, paste0, collapse="")
  date_end = sapply( date_end, paste0, collapse="")
  
  date_start = as.Date(date_start, format='%d/%m/%Y')
  date_end = as.Date(date_end, format='%d/%m/%Y')
  
  df = data.frame(title, url, description, lng, lat, city, zip, street, date_start, date_end, time_start, time_end, price, organizer) %>%
    mutate(title = as.character(title),
           url = as.character(url),
           description = as.character(description),
           street = "Dieselstr. 10",
           time_start = NA,
           time_end = NA,
           organizer = as.character(organizer),
           price = NA,
           city = as.character(city)
    )
  return(df)
  
}

KompetenzzentrumEvents=getKompetenzzentrumEvents()