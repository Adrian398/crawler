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


# problem: unifying, simplfying library, SB@home no longer exists

getToscanasaalEvents = function() {
  url = "https://www-sbhome1.zv.uni-wuerzburg.de/qisserver/rds?state=wplan&act=Raum&pool=Raum&show=plan&P.vx=mittel&raum.rgid=82"
  # Start Browser
  rD = rsDriver()
  remDr = rD[["client"]]
  remDr$navigate(url)
  
  dfWrapper = data.frame()
  
  site = read_html(remDr$getPageSource()[[1]])
  df = toscanaCrawler(site, url)
  if (is.data.frame(df)) {
    dfWrapper = rbind(dfWrapper, df)
  }
  
  run <- TRUE
  i <- 1
  while (run) {
    tryCatch({
      remDr$findElement(
        'xpath',
        "//*[@id='wrapper']/div[6]/div[2]/table/tbody/tr/td/fieldset/form/input[3]"
      )$clickElement()
      site = read_html(remDr$getPageSource()[[1]])
      df = toscanaCrawler(site, url)
      if (is.data.frame(df)) {
        dfWrapper = rbind(dfWrapper, df)
      }
    },
    error = function(c) {
      run <<- F
    },
    warning = function(w) {
      run <<- F
    },
    finally = print(paste("Pressed button", i, "times")))
    i <- i + 1
    Sys.sleep(1)
    
  }
  remDr$close()
  rm(rD)
  gc()
  return(dfWrapper)
  
}
toscanaCrawler = function(site, url) {
  eventNodes = html_nodes(site, ".plan2")
  
  events = html_text(eventNodes)
  
  eventLinks = html_node(eventNodes, "a") %>%
    html_attr("href")
  
  singleBookings = str_detect(events, "Einzel")
  events = events[singleBookings] %>%
    str_replace_all("[\r\n\t]" , "")
  
  eventLinks = eventLinks[singleBookings]
  
  title = c()
  timeFrame = c()
  startDate = c()
  endDate = c()
  description = c()
  s = html_session(url)
  if (!is.na(nchar(events[1]))) {
    for (i in 1:length(events)) {
      event = events[i]
      timeFrame[i] = str_extract(event, "[0-9]{2}:[0-9]{2} - [0-9]{2}:[0-9]{2}")
      dates = str_extract_all(event, "[0-9]{2}.[0-9]{2}.[0-9]{4}", simplify = TRUE)
      startDate[i] = dates[1, 1]
      endDate[i] = dates[1, 2]
      
      title[i] = s %>%
        jump_to(eventLinks[i]) %>%
        read_html() %>%
        html_node('.form h1') %>%
        html_text()
      
      description[i]= s %>%
        read_html() %>%
        html_node('tr:nth-child(1) .mod_n') %>%
        html_text()
    }
    
    df = data.frame(
      title = title,
      timeFrame = timeFrame,
      date_start = startDate,
      date_end = endDate,
      url = eventLinks
    )
    
    df %>%
      separate(timeFrame, c("time_start", "time_end"), " - ") %>%
      mutate(
        title = as.character(title),
        url = as.character(url),
        date_start = dmy(date_start),
        date_end = dmy(date_end),
        time_start = times(paste0(time_start, ":00")),
        time_end = times(paste0(time_end, ":00")),
        lng = 9.93844,
        lat = 49.79331,
        organizer = "Dekanat Philosophische Fakultät",
        street = "Residenzplatz 2 (Toscanasaal)",
        city = "Wuerzburg",
        zip = 97070,
        price=NA,
        description=NA
        
      )
  } else {
    return(NULL)
  }
}

Toscana=getToscanasaalEvents()