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

getEventAttribute = function(event, node) {
  html_node(event, node) %>%
    html_text()
}
getHerbergeEvents = function() {
  
  url = "https://wuerzburg.jugendherberge.de/jugendherbergen/wuerzburg-281/reisen/familien"
  events = read_html(url) %>%
    html_nodes(".travel-program")
  
  title = c()
  link = c()
  daterange = c()
  description = c()
  price = c()
  s = html_session(url)
  for (i in 1:length(events)) {
    event = events[i]
    title[i] = getEventAttribute(event, '.card__title')
    daterange[i] = getEventAttribute(event, '.seasonal-text')
    eventLink = html_node(event, '.card__content a') %>%
      html_attr("href")
    
    link[i] = eventLink
    priceObject = html_nodes(event, '.total') %>%
      html_text()
    
    price[i] = paste(priceObject[1], priceObject[2], sep = ", ")
    
    description[i] = s %>%
      jump_to(eventLink) %>%
      read_html() %>%
      html_node('.crop-content') %>%
      html_text()
  }
  
  df = data.frame(
    title = title,
    url = link,
    daterange = daterange,
    description = description,
    price = price
  )
  
  df %>%
    separate(daterange, c("date_start", "date_end"), "-") %>%
    mutate(date_end = dmy(date_end)) %>%
    mutate(date_start = str_trim(as.character(date_start))) %>%
    mutate(date_start = ifelse(nchar(date_start) == 6, paste(date_start, year(date_end), sep = ""), date_start)) %>%
    mutate(date_start = dmy(date_start)) %>%
    mutate(
      url = paste0("https://wuerzburg.jugendherberge.de", url, sep = ""),
      lng = 9.9251022,
      lat = 49.790162,
      description = as.character(description),
      title = as.character(title),
      organizer = "DJH-Landesverband Bayern e.V.",
      street = "Fred-Joseph-Platz 2",
      zip = 97082,
      city = "Wuerzburg",
      price = as.character(price),
      time_start=NA,
      time_end=NA
      
    )
}

HerbergeEvents=getHerbergeEvents()
