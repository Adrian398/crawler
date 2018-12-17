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

# problem: unifying, simplfying library, browser crawler (without browser possible)

getKellerperlencrawler <- function(){
  title = c()
  link = c()
  time = c()
  description = c()
  img = c()
  startDate = c()
  endDate = c()
  endDate = c()
  startTime = c()
  endTime = c()
  lat=c()
  long=c()
  
  url <- "http://www.kellerperle.de/#/uebersicht"
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(url)
  site = read_html(remDr$getPageSource()[[1]])
  
  html_nodes(site,"#uebersicht .ng-scope a")->nodes
  html_text(nodes, trim=T)->text
  html_nodes(site,"strong")%>%
    html_text()->titles
  titles[6:length(titles)]->title
  title
  html_nodes(site,".small")%>%
    html_text()->description
  description[5:length(description)]->description
  description
  html_nodes(site,".right")%>%
    html_text(trim=T)->startDate
  startDate
  
  nodes%>%
    html_attr("href")->links
  paste0("http://www.kellerperle.de/",links)->urls 
  link <- urls
  
  for(i in 1:length(urls)){
    remDr$navigate(urls[i])
    read_html(remDr$getPageSource()[[1]])->rawdata
    rawdata%>%
      html_nodes(".text-md-right")%>%
      html_text(trim=T)->infos
    infos[4]
    str_extract(infos[4],"Beginn \\d\\d:\\d\\d")%>%
      str_replace_all("Beginn ","")->startTime[i]
    str_extract(infos[4],"Ende \\d\\d:\\d\\d")%>%
      str_replace_all("Ende ","")->endTime[i]
    
    49.7861264->lat[i]
    9.9342507->long[i]
  }
  #always the same times for some reason!! bug
  startTime
  endTime
  #
  #dmy_hm(paste0(str_extract(startDate, "[0-9]{2}\\.[0-9]{2}"), ".", year(Sys.Date()), " 00:00"))
  
  df = data.frame(
    title = title,
    url = link,
    description = description,
    date_start = dmy_hm(paste0(str_extract(startDate, "[0-9]{2}\\.[0-9]{2}"), ".", year(Sys.Date()), " 00:00")),
    date_end = NA,
    time_start = startTime,
    time_end = endTime,
    lng=long,
    lat=lat,
    price = NA,
    organizer = "Kellerperle",
    city = "Wuerzburg",
    street = "Am Studentenhaus",
    zip = 97072
  )
  #
  
  df = df %>%
    mutate(
      title = as.character(title),
      description = as.character(description),
      url = as.character(url),
      time_start = NA,
      time_end = NA,
      price = NA,
      street = as.character(street),
      city = as.character(city),
      organizer = as.character(organizer),
      date_start = as.Date(date_start, origin = '1970-01-01'),
      date_end = NA
    )
  
  # url2 <- "http://www.kellerperle.de/#/1455"
  #remDr$navigate(url2)
  #site2 <-  read_html(remDr$getPageSource()[[1]])
  #html_nodes(site2,".text-md-right")%>%
  #  html_text(trim=T)->text2
  #text2[4]
  #str_extract(text2[4],"Beginn \\d\\d:\\d\\d")%>%
  #  str_replace_all("Beginn ","")->start
  #str_extract(text2[4],"Ende \\d\\d:\\d\\d")%>%
  #  str_replace_all("Ende ","")->end
  remDr$close()
  rm(rD)
  gc()
  
  return(df)
}

Kellerperle=getKellerperlencrawler()