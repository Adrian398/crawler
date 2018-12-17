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

# problem: unifying, simplfying library, time_start = NA(String), date broken

getBotanischerGartenEvents = function() {
  
  url = "http://www.bgw.uni-wuerzburg.de/fuehrungen-und-veranstaltungen/veranstaltungen/"
  nPages = read_html(url) %>%
    html_node('.news-list .page-navigation') %>%
    html_node('p') %>%
    html_text() %>%
    str_replace_all("[\r\n\t]" , "") %>%
    str_sub(start = -2, -2)
  
  df = botanischerGartenCrawler(url)
  
  for(i in 2:nPages) {
    url = paste0("http://www.bgw.uni-wuerzburg.de/fuehrungen-und-veranstaltungen/veranstaltungen/page/", i, sep = "" )
    newData = botanischerGartenCrawler(url)
    df = bind_rows(df, newData)
  }
  
  df$time_start = times(df$time_start)
  df$date_end = as.Date(df$date_end, origin = '1970-01-01')
  return(df)
}

BotanischerGartenEvents=getBotanischerGartenEvents()